type mode =
  | Away
  | Invisible
  | ReceivesWallops
  | Restricted
  | Operator
  | LocalOperator
  | ServerNotices

type message =
  | NICK of string
  | USER of string * string
  | OPER of string * string
  | MODE of string * string
  | QUIT of string
  | PART_ALL
  | JOIN of (string * string option) list
  | PART of string list * string
  | TOPIC of string * string
  | NAMES of string list * string option
  | LIST of string list * string option
  | INVITE of string * string
  | PRIVMSG of [ `Channel of string | `Nick of string ] list * string

let parse_message l =
  let tok lex l = lex (Lexing.from_string l) in
  let command, params = Lexer.message (Lexing.from_string l) in
  match String.uppercase command, params with
  | "NICK", [n] ->
      let n = tok Lexer.nickname n in
      NICK n
  | "USER", [user; mode; _; realname] ->
      let user = tok Lexer.user user in
      USER (user, realname)
  | "OPER", [name; password] ->
      OPER (name, password)
  | "MODE", [nick; modes] ->
      MODE (nick, modes)
  | "QUIT", [] ->
      QUIT ""
  | "QUIT", [msg] ->
      QUIT msg
  | "JOIN", ["0"] ->
      PART_ALL
  | "JOIN", [channels] ->
      let channels = tok Lexer.channel_list channels in
      JOIN (List.map (fun x -> (x, None)) channels)
  | "JOIN", [channels; keys] ->
      let channels = tok Lexer.channel_list channels in
      let keys = tok Lexer.key_list keys in
      let rec loop cs ks =
        match cs, ks with
        | c :: cs, k :: ks ->
            (c, Some k) :: loop cs ks
        | cs, [] ->
            List.map (fun c -> (c, None)) cs
        | [], _ ->
            []
      in
      JOIN (loop channels keys)
  | "PART", [channels] ->
      let channels = tok Lexer.channel_list channels in
      PART (channels, "")
  | "PART", [channels; msg] ->
      let channels = tok Lexer.channel_list channels in
      PART (channels, msg)
  | "TOPIC", [chan] ->
      let chan = tok Lexer.channel chan in
      TOPIC (chan, "")
  | "TOPIC", [chan; topic] ->
      let chan = tok Lexer.channel chan in
      TOPIC (chan, topic)
  | "PRIVMSG", [msgtarget; texttobesent] ->
      let msgtarget = tok Lexer.msgtarget msgtarget in
      PRIVMSG (msgtarget, texttobesent)
  | _ as cmd, _ ->
      failwith ("unrecognized command : " ^ cmd)

module H = Hashtbl.Make
    (struct
      type t = string
      let equal s1 s2 =
        if String.length s1 <> String.length s2 then
          false
        else
          let rec loop i =
            if i >= String.length s1 then
              true
            else if Char.lowercase s1.[i] <> Char.lowercase s2.[i] then
              false
            else
              loop (i+1)
          in
          loop 0
      let hash s =
        let h = ref 5381 in
        for i = 0 to String.length s - 1 do
          h := (!h lsl 5) + Char.code (Char.lowercase s.[i])
        done;
        !h
    end)

type channel =
  { mutable topic : string;
    mutable name : string;
    mutable members : user list }

and user =
  { mutable nick : string;
    mutable user : string;
    mutable realname : string;
    mutable joined : channel list;
    ic : Lwt_io.input_channel;
    oc : Lwt_io.output_channel;
    mutable last_act : float }

type server =
  { channels : channel H.t;
    users    : user H.t }

let motd = "Welcome to the Mirage IRC Server\nEnjoy!"
let motd = Stringext.split motd ~on:'\n'

let (>>=) = Lwt.(>>=)

let handle_client srv (ic, oc) =
  let rec loop () =
    lwt l = Lwt_io.read_line ic in
    match parse_message l with
    | NICK n ->
        lwt () = assert_lwt (not (H.mem srv.users n)) in
        Lwt.return n
    | _ ->
        loop ()
    | exception _ ->
        loop ()
  in
  lwt n = loop () in
  let rec loop () =
    lwt l = Lwt_io.read_line ic in
    match parse_message l with
    | USER (u, r) ->
        Lwt.return (u, r)
    | _ ->
        loop ()
    | exception _ ->
        loop ()
  in
  lwt u, r = loop () in
  let u =
    { nick = n; user = u; realname = r;
      joined = []; ic; oc; last_act = Unix.time () }
  in
  lwt () = Lwt_io.fprintf u.oc "001 %s :Welcome to the Mirage IRC Server\r\n" u.nick in
  lwt () = Lwt_io.fprintf u.oc "002 %s :Your host is ..., running ...\r\n" u.nick in
  lwt () = Lwt_io.fprintf u.oc "003 %s :This server was created on ...\r\n" u.nick in
  lwt () = Lwt_io.fprintf u.oc "375 :- Mirage IRC Message of the day - \r\n" in
  lwt () = Lwt_list.iter_s (fun l -> Lwt_io.fprintf u.oc "372 :- %s\r\n" l) motd in
  lwt () = Lwt_io.fprintf u.oc "376 :End of MOTD command\r\n" in
  let rec read_message () =
    Lwt_io.read_line ic >>= fun l ->
    match parse_message l with
    | JOIN chans ->
        let chans = List.map (H.find srv.channels) (List.map fst chans) in
        List.iter (fun ch -> ch.members <- u :: ch.members) chans;
        Lwt_list.iter_p begin fun ch ->
          Lwt_list.iter_p begin fun u' ->
            Lwt_io.fprintf u'.oc ":%s JOIN %s\r\n" u.nick ch.name
          end ch.members
        end chans
    | PRIVMSG (targets, msg) ->
        let send_user tgt u = Lwt_io.fprintf u.oc ":%s PRIVMSG %s :%s\r\n" u.nick tgt msg in
        let send = function
          | `Channel chan ->
              let ch = H.find srv.channels chan in
              Lwt_list.iter_p (send_user chan) ch.members
          | `Nick n ->
              send_user n (H.find srv.users n)
        in
        Lwt_list.iter_p send targets
    | _ ->
        read_message ()
    | exception _ ->
        read_message ()
  in
  Lwt.catch read_message
    (fun _ ->
       H.remove srv.users u.nick;
       List.iter (fun ch -> ch.members <- List.filter (fun u' -> u' != u) ch.members) u.joined;
       Lwt_list.iter_p begin fun ch ->
         Lwt_list.iter_p begin fun u' ->
           Lwt_io.fprintf u'.oc ":%s QUIT :%s\r\n" u.nick "Connection closed by peer"
         end ch.members
       end u.joined)

let server_loop srv =
  Lwt_io.establish_server
    (Unix.ADDR_INET (Unix.inet_addr_any, 6667))
    (fun io -> Lwt.async (fun () -> handle_client srv io))

let _ =
  let srv =
    { channels = H.create 0;
      users = H.create 0 }
  in
  let s = server_loop srv in
  let t, u = Lwt.wait () in
  Lwt_main.run t

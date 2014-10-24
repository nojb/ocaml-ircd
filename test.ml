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

exception ErroneusNickname of string
exception NoNicknameGiven
exception NeedMoreParams of string
exception UnknownCommand of string

let nickname n =
  try
    Lexer.nickname (Lexing.from_string n)
  with
  | _ -> raise (ErroneusNickname n)

let parse_message l =
  let tok lex l = lex (Lexing.from_string l) in
  let command, params = Lexer.message (Lexing.from_string l) in
  match String.uppercase command, params with
  | "NICK", [n] ->
      NICK (nickname n)
  | "NICK", [] ->
      raise NoNicknameGiven
  | "USER", user :: mode :: _ :: realname :: _ ->
      let user = tok Lexer.user user in
      USER (user, realname)
  | "USER", _ ->
      raise (NeedMoreParams command)
  | "OPER", name :: password :: _ ->
      OPER (name, password)
  | "OPER", _ ->
      raise (NeedMoreParams command)
  | "MODE", nick :: modes :: _ ->
      MODE (nick, modes)
  | "MODE", _ ->
      raise (NeedMoreParams command)
  | "QUIT", [] ->
      QUIT ""
  | "QUIT", msg :: _ ->
      QUIT msg
  | "JOIN", "0" :: [] ->
      PART_ALL
  | "JOIN", channels :: [] ->
      let channels = tok Lexer.channel_list channels in
      JOIN (List.map (fun x -> (x, None)) channels)
  | "JOIN", channels :: keys :: _ ->
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
  | "JOIN", [] ->
      raise (NeedMoreParams command)
  | "PART", [] ->
      raise (NeedMoreParams command)
  | "PART", channels :: [] ->
      let channels = tok Lexer.channel_list channels in
      PART (channels, "")
  | "PART", channels :: msg :: _ ->
      let channels = tok Lexer.channel_list channels in
      PART (channels, msg)
  | "TOPIC", [chan] ->
      let chan = tok Lexer.channel chan in
      TOPIC (chan, "")
  | "TOPIC", [chan; topic] ->
      let chan = tok Lexer.channel chan in
      TOPIC (chan, topic)
  | "PRIVMSG", msgtarget :: texttobesent :: _ ->
      let msgtarget = tok Lexer.msgtarget msgtarget in
      PRIVMSG (msgtarget, texttobesent)
  | _ ->
      raise (UnknownCommand command)

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

let broadcast u f =
  Lwt_list.iter_p begin fun ch ->
    Lwt_list.iter_p begin fun u' ->
      f u'
    end ch.members
  end u.joined

let find_channel srv name =
  try
    H.find srv.channels name
  with
  | Not_found ->
      let ch = { members = []; name; topic = "" } in
      H.add srv.channels name ch;
      ch

let handle_client srv (ic, oc) =
  let rec loop n u =
    lwt l = Lwt_io.read_line ic in
    match parse_message l, n, u with
    | NICK n, _, None ->
        lwt () = assert_lwt (not (H.mem srv.users n)) in
        loop (Some n) None
    | NICK n, _, Some (u, r) ->
        Lwt.return (n, u, r)
    | USER (u, r), None, _ ->
        loop None (Some (u, r))
    | USER (u, r), Some n, _ ->
        Lwt.return (n, u, r)
    | _ ->
        Lwt_io.fprintf oc "451 :You have not registered\r\n" >>
        loop n u
    | exception _ ->
        loop n u
  in
  lwt n, u, r = loop None None in
  let u =
    { nick = n; user = u; realname = r;
      joined = []; ic; oc; last_act = Unix.time () }
  in
  lwt () = Lwt_io.fprintf u.oc "001 %s :Welcome to the Mirage IRC Server\r\n" u.nick in
  (* lwt () = Lwt_io.fprintf u.oc "002 %s :Your host is ..., running ...\r\n" u.nick in *)
  (* lwt () = Lwt_io.fprintf u.oc "003 %s :This server was created on ...\r\n" u.nick in *)
  lwt () = Lwt_io.fprintf u.oc "375 :- Mirage IRC Message of the day - \r\n" in
  lwt () = Lwt_list.iter_s (fun l -> Lwt_io.fprintf u.oc "372 :- %s\r\n" l) motd in
  lwt () = Lwt_io.fprintf u.oc "376 :End of MOTD command\r\n" in
  let rec read_message () =
    Lwt_io.flush oc >>= fun () ->
    Lwt_io.read_line ic >>= fun l ->
    match parse_message l with
    | JOIN chans ->
        let chans = List.map (find_channel srv) (List.map fst chans) in
        Lwt_list.iter_p begin fun ch ->
          ch.members <- u :: ch.members;
          Lwt_list.iter_p begin fun u' ->
            Lwt_io.fprintf u'.oc ":%s JOIN %s\r\n" u.nick ch.name
          end ch.members
        end chans
    | PRIVMSG (_, "") ->
        Lwt_io.fprintf oc "412 :No text to send\r\n" >>=
        read_message
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
    | QUIT msg ->
        prerr_endline "QUIT!";
        broadcast u (fun u' -> Lwt_io.fprintf u'.oc ":%s QUIT :%s\r\n" u.nick msg) >>= fun () ->
        List.iter (fun ch -> ch.members <- List.filter (fun u' -> u' != u) ch.members) u.joined;
        Lwt_io.fprintf oc "ERROR Bye\r\n"
    | _ ->
        read_message ()
    | exception NoNicknameGiven ->
        Lwt_io.fprintf oc "431 :No nickname given\r\n" >>=
        read_message
    | exception (NeedMoreParams cmd) ->
        Lwt_io.fprintf oc "461 %s :Not enough parameters\r\n" cmd >>=
        read_message
    | exception (ErroneusNickname n) ->
        Lwt_io.fprintf oc "432 %s :Erroneus nickname\r\n" n >>=
        read_message
    | exception (UnknownCommand cmd) ->
        Lwt_io.fprintf oc "421 %s :Unknown command\r\n" cmd >>=
        read_message
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

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
  { mutable topic : string option;
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

let my_hostname =
  Unix.gethostname ()

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
      let ch = { members = []; name; topic = None } in
      H.add srv.channels name ch;
      ch

let rpl_welcome oc ~nick ~message =
  Lwt_io.fprintf oc ":%s 001 %s :%s\r\n" my_hostname nick message

let rpl_motd oc ~motd =
  Lwt_io.fprintf oc ":%s 375 :- Mirage IRC Message of the day - \r\n" my_hostname >>
  Lwt_list.iter_s (fun l -> Lwt_io.fprintf oc ":%s 372 :- %s\r\n" my_hostname l) motd >>
  Lwt_io.fprintf oc ":%s 376 :End of /MOTD command\r\n" my_hostname

let err_notregistered oc =
  Lwt_io.fprintf oc ":%s 451 :You have not registered\r\n" my_hostname

let err_useronchannel oc ~nick ~channel =
  Lwt_io.fprintf oc ":%s 443 %s %s :is already on channel\r\n" my_hostname nick channel

let rpl_topic oc ?topic ~channel =
  match topic with
  | None ->
      Lwt_io.fprintf oc ":%s 331 %s :No topic is set\r\n" my_hostname channel
  | Some topic ->
      Lwt_io.fprintf oc ":%s 332 %s :%s\r\n" my_hostname channel topic

let rpl_namereply oc ~nick ~channel ~nicks =
  Lwt_io.fprintf oc ":%s 353 %s = %s :%s\r\n" my_hostname nick channel (String.concat " " nicks) >>
  Lwt_io.fprintf oc ":%s 366 %s %s :End of /NAMES list\r\n" my_hostname nick channel

let err_notexttosend oc =
  Lwt_io.fprintf oc ":%s 412 :No text to send\r\n" my_hostname

let err_nonicknamegiven oc =
  Lwt_io.fprintf oc ":%s 431 :No nickname given\r\n" my_hostname

let err_needmoreparams oc ~cmd =
  Lwt_io.fprintf oc ":%s 461 %s :Not enough parameters\r\n" my_hostname cmd

let err_erroneusnickname oc ~nick =
  Lwt_io.fprintf oc ":%s 432 %s :Erroneus nickname\r\n" my_hostname nick

let err_unknowncommand oc ~cmd =
  Lwt_io.fprintf oc ":%s 421 %s :Unknown command\r\n" my_hostname cmd

let joined oc ~nick ~channel =
  Lwt_io.fprintf oc ":%s JOIN %s\r\n" nick channel

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
        err_notregistered oc >> loop n u
    | exception _ ->
        loop n u
  in
  lwt n, u, r = loop None None in
  let u = { nick = n; user = u; realname = r; joined = []; ic; oc; last_act = Unix.time () } in
  lwt () = rpl_welcome oc ~nick:u.nick ~message:"Welcome to the Mirage IRC Server" in
  lwt () = rpl_motd oc ~motd in
  let rec read_message () =
    Lwt_io.flush oc >>= fun () ->
    Lwt_io.read_line ic >>= fun l ->
    match parse_message l with
    | JOIN chans ->
        let chans = List.map (find_channel srv) (List.map fst chans) in
        Lwt_list.iter_p begin fun ch ->
          if List.memq u ch.members then
            err_useronchannel oc ~nick:u.nick ~channel:ch.name
          else begin
            lwt () = Lwt_list.iter_p (fun u' -> joined u'.oc u.nick ch.name) ch.members in
            ch.members <- u :: ch.members;
            rpl_topic oc ~channel:ch.name ?topic:ch.topic >>
            let nicks = List.map (fun u -> u.nick) ch.members in
            rpl_namereply oc ~nick:u.nick ~channel:ch.name ~nicks
          end
        end chans
    | PRIVMSG (_, "") ->
        err_notexttosend oc >>= read_message
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
        broadcast u (fun u' -> Lwt_io.fprintf u'.oc ":%s QUIT :%s\r\n" u.nick msg) >>= fun () ->
        List.iter (fun ch -> ch.members <- List.filter (fun u' -> u' != u) ch.members) u.joined;
        Lwt_io.fprintf oc ":%s ERROR Bye\r\n" my_hostname
    | _ ->
        read_message ()
    | exception NoNicknameGiven ->
        err_nonicknamegiven oc >>= read_message
    | exception (NeedMoreParams cmd) ->
        err_needmoreparams oc ~cmd >>= read_message
    | exception (ErroneusNickname n) ->
        err_erroneusnickname oc ~nick:n >>= read_message
    | exception (UnknownCommand cmd) ->
        err_unknowncommand oc ~cmd >>= read_message
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

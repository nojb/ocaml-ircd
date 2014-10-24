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
  | PART of string list * string option
  | GET_TOPIC of string
  | SET_TOPIC of string * string option
  | NAMES of string list * string option
  | LIST of string list * string option
  | INVITE of string * string
  | PRIVMSG of [ `Channel of string | `Nick of string ] list * string

exception ErroneusNickname of string
exception NoNicknameGiven
exception NeedMoreParams of string
exception UnknownCommand of string
exception NoTextToSend

let nickname n =
  try
    Lexer.nickname (Lexing.from_string n)
  with
  | _ -> raise (ErroneusNickname n)

let parse_message l =
  let tok lex l = lex (Lexing.from_string l) in
  let command, params = Lexer.message (Lexing.from_string l) in
  match String.uppercase command, params with
  | "NICK", [] ->
      raise NoNicknameGiven
  | "NICK", n :: _ ->
      NICK (nickname n)
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
      PART (channels, None)
  | "PART", channels :: msg :: _ ->
      let channels = tok Lexer.channel_list channels in
      PART (channels, Some msg)
  | "TOPIC", [] ->
      raise (NeedMoreParams command)
  | "TOPIC", chan :: [] ->
      let chan = tok Lexer.channel chan in
      GET_TOPIC chan
  | "TOPIC", chan :: "" :: _ ->
      let chan = tok Lexer.channel chan in
      SET_TOPIC (chan, None)
  | "TOPIC", chan :: topic :: _ ->
      let chan = tok Lexer.channel chan in
      SET_TOPIC (chan, Some topic)
  | "PRIVMSG", _ :: [] ->
      raise NoTextToSend
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
    name : string;
    mutable members : user list }

and user =
  { mutable nick : string;
    user : string;
    host : string;
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

let get_channel srv name =
  try
    H.find srv.channels name
  with
  | Not_found ->
      let ch = { members = []; name; topic = None } in
      H.add srv.channels name ch;
      ch

let rpl_welcome oc nick ~message =
  Lwt_io.fprintf oc ":%s 001 %s :%s\r\n" my_hostname nick message

let rpl_motd oc nick ~motd =
  Lwt_io.fprintf oc ":%s 375 %s :- Mirage IRC Message of the day - \r\n" my_hostname nick >>
  Lwt_list.iter_s (fun l -> Lwt_io.fprintf oc ":%s 372 %s :- %s\r\n" my_hostname nick l) motd >>
  Lwt_io.fprintf oc ":%s 376 %s :End of /MOTD command\r\n" my_hostname nick

let err_notregistered oc nick =
  Lwt_io.fprintf oc ":%s 451 %s :You have not registered\r\n" nick my_hostname

let err_useronchannel oc nick ~channel =
  Lwt_io.fprintf oc ":%s 443 %s %s :is already on channel\r\n" my_hostname nick channel

let rpl_topic oc nick ?topic ~channel =
  match topic with
  | None ->
      Lwt_io.fprintf oc ":%s 331 %s %s :No topic is set\r\n" my_hostname nick channel
  | Some topic ->
      Lwt_io.fprintf oc ":%s 332 %s %s :%s\r\n" my_hostname nick channel topic

let rpl_namereply oc nick ~channel ~nicks =
  Lwt_io.fprintf oc ":%s 353 %s = %s :%s\r\n" my_hostname nick channel (String.concat " " nicks) >>
  Lwt_io.fprintf oc ":%s 366 %s %s :End of /NAMES list\r\n" my_hostname nick channel

let err_notexttosend oc nick =
  Lwt_io.fprintf oc ":%s 412 %s :No text to send\r\n" my_hostname nick

let err_nonicknamegiven oc nick =
  Lwt_io.fprintf oc ":%s 431 %s :No nickname given\r\n" my_hostname nick

let err_needmoreparams oc nick ~cmd =
  Lwt_io.fprintf oc ":%s 461 %s %s :Not enough parameters\r\n" my_hostname nick cmd

let err_erroneusnickname oc nick =
  Lwt_io.fprintf oc ":%s 432 %s :Erroneus nickname\r\n" my_hostname nick

let err_unknowncommand oc nick ~cmd =
  Lwt_io.fprintf oc ":%s 421 %s %s :Unknown command\r\n" my_hostname nick cmd

let join oc u ~channel =
  Lwt_io.fprintf oc ":%s!%s@%s JOIN %s\r\n" u.nick u.user u.host channel

let quit oc u ~msg =
  Lwt_io.fprintf oc ":%s!%s@%s QUIT :%s\r\n" u.nick u.user u.host msg

let error oc nick ~msg =
  Lwt_io.fprintf oc ":%s ERROR %s :%s\r\n" my_hostname nick msg

let privmsg oc u ~target ~msg =
  Lwt_io.fprintf oc ":%s!%s@%s PRIVMSG %s :%s\r\n" u.nick u.user u.host target msg

let set_topic oc u ?topic ~channel =
  match topic with
  | None ->
      Lwt_io.fprintf oc ":%s!%s@%s TOPIC %s :\r\n" u.nick u.user u.host channel
  | Some topic ->
      Lwt_io.fprintf oc ":%s!%s@%s TOPIC %s :%s\r\n" u.nick u.user u.host channel topic

let err_nosuchnick oc nick ~target =
  Lwt_io.fprintf oc ":%s 401 %s %s :No such nick/channel\r\n" my_hostname nick target

let err_alreadyregistered oc nick =
  Lwt_io.fprintf oc ":%s 462 %s :Unauthorized command (already registered)\r\n" my_hostname nick

let err_nosuchchannel oc nick ~channel =
  Lwt_io.fprintf oc ":%s 403 %s %s :No such channel\r\n" my_hostname nick channel

let err_notonchannel oc nick ~channel =
  Lwt_io.fprintf oc ":%s 442 %s %s :You're not on that channel\r\n" my_hostname nick channel

let part oc u ~channel ~msg =
  Lwt_io.fprintf oc ":%s!%s@%s PART %s %s\r\n" u.nick u.user u.host channel msg

exception Quit

let handle_message s u m =
  match m with
  | JOIN chans ->
      let chans = List.map (get_channel s) (List.map fst chans) in
      Lwt_list.iter_p begin fun ch ->
        if List.memq u ch.members then
          err_useronchannel u.oc u.nick ~channel:ch.name
        else begin
          ch.members <- u :: ch.members;
          u.joined <- ch :: u.joined;
          lwt () = Lwt_list.iter_p (fun u' -> join u'.oc u ~channel:ch.name) ch.members in
          rpl_topic u.oc ?topic:ch.topic u.nick ~channel:ch.name >>
          let nicks = List.map (fun u -> u.nick) ch.members in
          rpl_namereply u.oc u.nick ~channel:ch.name ~nicks
        end
      end chans
  | PART (chans, msg) ->
      let msg = match msg with None -> u.nick | Some msg -> msg in
      Lwt_list.iter_p begin fun ch ->
        if not (H.mem s.channels ch) then
          err_nosuchchannel u.oc u.nick ~channel:ch
        else
          let ch = H.find s.channels ch in
          if not (List.memq ch u.joined) then
            err_notonchannel u.oc u.nick ~channel:ch.name
          else begin
            lwt () = Lwt_list.iter_p (fun u' -> part u'.oc u ~channel:ch.name ~msg) ch.members in
            u.joined <- List.filter (fun ch' -> ch' != ch) u.joined;
            ch.members <- List.filter (fun u' -> u' != u) ch.members;
            Lwt.return_unit
          end
      end chans
  | PRIVMSG (targets, msg) ->
      Lwt_list.iter_p begin function
        | `Channel chan ->
            if H.mem s.channels chan then
              let ch = H.find s.channels chan in
              Lwt_list.iter_p begin fun u' ->
                if u != u' then
                  privmsg u'.oc u ~target:chan ~msg
                else
                  Lwt.return_unit
              end ch.members
            else
              err_nosuchnick u.oc u.nick ~target:chan
        | `Nick n ->
            if H.mem s.users n then
              let u' = H.find s.users n in
              privmsg u'.oc u ~target:u'.nick ~msg
            else
              err_nosuchnick u.oc u.nick ~target:n
      end targets
  | QUIT msg ->
      Lwt_list.iter_p begin fun ch ->
        ch.members <- List.filter (fun u' -> u' != u) ch.members;
        Lwt_list.iter_p (fun u' -> quit u'.oc u ~msg) ch.members
      end u.joined >>
      error u.oc u.nick "Bye!" >>
      raise_lwt Quit
  | USER _ ->
      err_alreadyregistered u.oc u.nick
  | GET_TOPIC ch ->
      if H.mem s.channels ch then
        let c = H.find s.channels ch in
        if List.memq c u.joined then
          rpl_topic u.oc ?topic:c.topic u.nick ~channel:ch
        else
          err_notonchannel u.oc u.nick ~channel:ch
      else
        err_notonchannel u.oc u.nick ~channel:ch
  | SET_TOPIC (ch, topic) ->
      if H.mem s.channels ch then
        let c = H.find s.channels ch in
        if List.memq c u.joined then begin
          c.topic <- topic; (* FIXME perms *)
          Lwt_list.iter_p begin fun u' ->
            set_topic u'.oc u ?topic ~channel:ch
          end c.members
        end else
          err_notonchannel u.oc u.nick ~channel:ch
      else
        err_notonchannel u.oc u.nick ~channel:ch
  | _ ->
      Lwt.return_unit

let handle_registration s fd ic oc =
  let rec loop n u =
    lwt l = Lwt_io.read_line ic in
    match parse_message l, n, u with
    | NICK n, _, None ->
        lwt () = assert_lwt (not (H.mem s.users n)) in
        loop (Some n) None
    | NICK n, _, Some (u, r) ->
        Lwt.return (n, u, r)
    | USER (u, r), None, _ ->
        loop None (Some (u, r))
    | USER (u, r), Some n, _ ->
        Lwt.return (n, u, r)
    | _, Some nick, _ ->
        err_notregistered oc nick >>
        loop n u
    | _, None, _ ->
        err_notregistered oc "*" >>
        loop n u
    | exception _ ->
        loop n u
  in
  let addr = match Lwt_unix.getpeername fd with Unix.ADDR_INET (addr, _) -> addr | _ -> assert false in
  lwt he = Lwt_unix.gethostbyaddr addr
  and n, u, r = loop None None in
  let u =
    { nick = n; user = u; host = he.Unix.h_name; realname = r; joined = []; ic; oc; last_act = Unix.time () }
  in
  lwt () = rpl_welcome oc u.nick ~message:"Welcome to the Mirage IRC Server" in
  lwt () = rpl_motd oc u.nick ~motd in
  H.add s.users n u;
  Lwt.return u

let handle_client s fd =
  let close = lazy begin
    try_lwt
      Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL;
      Lwt_unix.close fd
    with
    | _ -> Lwt.return_unit
  end in
  let ic = Lwt_io.of_fd ~close:(fun () -> Lazy.force close) ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~close:(fun () -> Lazy.force close) ~mode:Lwt_io.Output fd in
  lwt u = handle_registration s fd ic oc in
  let rec read_message () =
    lwt () = Lwt_io.flush oc in
    lwt l = Lwt_io.read_line ic in
    match parse_message l with
    | m ->
        handle_message s u m >>=
        read_message
    | exception NoNicknameGiven ->
        err_nonicknamegiven oc u.nick >>=
        read_message
    | exception (NeedMoreParams cmd) ->
        err_needmoreparams oc u.nick ~cmd >>=
        read_message
    | exception (ErroneusNickname n) ->
        err_erroneusnickname oc u.nick >>=
        read_message
    | exception (UnknownCommand cmd) ->
        err_unknowncommand oc u.nick ~cmd >>=
        read_message
    | exception NoTextToSend ->
        err_notexttosend oc u.nick >>=
        read_message
    | exception _ ->
        read_message ()
  in
  Lwt.catch read_message begin function
    | Quit ->
        H.remove s.users u.nick;
        List.iter (fun ch -> ch.members <- List.filter (fun u' -> u' != u) ch.members) u.joined;
        Lwt_io.close oc
    | _ ->
        H.remove s.users u.nick;
        List.iter (fun ch -> ch.members <- List.filter (fun u' -> u' != u) ch.members) u.joined;
        lwt () =
          Lwt_list.iter_p begin fun ch ->
            Lwt_list.iter_p begin fun u' ->
              Lwt_io.fprintf u'.oc ":%s QUIT :%s\r\n" u.nick "Connection closed by peer"
            end ch.members
          end u.joined
        in
        Lwt_io.close oc
  end

let server_loop s =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt fd Unix.SO_REUSEADDR true;
  Lwt_unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, 6667));
  Lwt_unix.listen fd 5;
  let rec loop () =
    lwt fd, sa = Lwt_unix.accept fd in
    Lwt.async (fun () -> handle_client s fd);
    loop ()
  in
  loop ()

let _ =
  let srv =
    { channels = H.create 0;
      users = H.create 0 }
  in
  let s = server_loop srv in
  let t, u = Lwt.wait () in
  Lwt_main.run t

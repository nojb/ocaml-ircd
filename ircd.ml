(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

exception ErroneusNickname of string
exception NoNicknameGiven
exception NeedMoreParams of string
exception UnknownCommand of string
exception NoTextToSend
exception NoOrigin
exception NoRecipient of string
exception NotOnChannel of string
exception NoSuchNick of string
exception NoSuchChannel of string
exception AlreadyRegistered
exception UserOnChannel of string

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

module type STATE = sig
  type io

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
      io : io;
      mutable last_act : float }

  type server =
    { channels : channel H.t;
      users    : user H.t }
end

module State (C : V1_LWT.CHANNEL) : STATE with type io = C.t = struct
  type io = C.t

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
      io : io;
      mutable last_act : float }

  type server =
    { channels : channel H.t;
      users    : user H.t }
end

let motd = "Welcome to the Mirage IRC Server\nEnjoy!"
let motd = Stringext.split motd ~on:'\n'

let my_hostname =
  Unix.gethostname ()

module type RPL = sig
  type io
  val welcome : io -> string -> message:string -> unit
  val motd : io -> string -> motd:string list -> unit
  val topic : io -> string -> ?topic:string -> channel:string -> unit
  val namereply : io -> string -> channel:string -> nicks:string list -> unit
  val ison : io -> string -> string list -> unit
end

module Rpl (C : V1_LWT.CHANNEL) : RPL with type io = C.t = struct
  type io = C.t

  let write_string c fmt =
    Printf.ksprintf (fun s -> C.write_string c s 0 (String.length s)) fmt

  let welcome c nick ~message =
    write_string c ":%s 001 %s :%s\r\n" my_hostname nick message

  let motd c nick ~motd =
    write_string c ":%s 375 %s :- Mirage IRC Message of the day - \r\n" my_hostname nick;
    List.iter (fun l -> write_string c ":%s 372 %s :- %s\r\n" my_hostname nick l) motd;
    write_string c ":%s 376 %s :End of /MOTD command\r\n" my_hostname nick

  let topic c nick ?topic ~channel =
    match topic with
    | None ->
        write_string c ":%s 331 %s %s :No topic is set\r\n" my_hostname nick channel
    | Some topic ->
        write_string c ":%s 332 %s %s :%s\r\n" my_hostname nick channel topic

  let namereply c nick ~channel ~nicks =
    write_string c ":%s 353 %s = %s :%s\r\n" my_hostname nick channel (String.concat " " nicks);
    write_string c ":%s 366 %s %s :End of /NAMES list\r\n" my_hostname nick channel

  let ison c nick nicks =
    write_string c ":%s 303 %s :%s\r\n" my_hostname nick (String.concat " " nicks)
end

module type ERR = sig
  type io
  val notregistered : io -> string -> unit
  val useronchannel : io -> string -> channel:string -> unit
  val notexttosend : io -> string -> unit
  val nonicknamegiven : io -> string -> unit
  val needmoreparams : io -> string -> cmd:string -> unit
  val erroneusnickname : io -> string -> unit
  val unknowncommand : io -> string -> cmd:string -> unit
  val nosuchnick : io -> string -> target:string -> unit
  val alreadyregistered : io -> string -> unit
  val nosuchchannel : io -> string -> channel:string -> unit
  val notonchannel : io -> string -> channel:string -> unit
  val noorigin : io -> string -> unit
  val norecipient : io -> string -> cmd:string -> unit
  val nicknameinuse : io -> string -> nick:string -> unit
end

module Err (C : V1_LWT.CHANNEL) : ERR with type io = C.t = struct
  type io = C.t

  let write_string c fmt =
    Printf.ksprintf (fun s -> C.write_string c s 0 (String.length s)) fmt

  let notregistered c nick =
    write_string c ":%s 451 %s :You have not registered\r\n" nick my_hostname

  let useronchannel c nick ~channel =
    write_string c ":%s 443 %s %s :is already on channel\r\n" my_hostname nick channel

  let notexttosend c nick =
    write_string c ":%s 412 %s :No text to send\r\n" my_hostname nick

  let nonicknamegiven c nick =
    write_string c ":%s 431 %s :No nickname given\r\n" my_hostname nick

  let needmoreparams c nick ~cmd =
    write_string c ":%s 461 %s %s :Not enough parameters\r\n" my_hostname nick cmd

  let erroneusnickname c nick =
    write_string c ":%s 432 %s :Erroneus nickname\r\n" my_hostname nick

  let unknowncommand c nick ~cmd =
    write_string c ":%s 421 %s %s :Unknown command\r\n" my_hostname nick cmd

  let nosuchnick c nick ~target =
    write_string c ":%s 401 %s %s :No such nick/channel\r\n" my_hostname nick target

  let alreadyregistered c nick =
    write_string c ":%s 462 %s :Unauthorized command (already registered)\r\n" my_hostname nick

  let nosuchchannel c nick ~channel =
    write_string c ":%s 403 %s %s :No such channel\r\n" my_hostname nick channel

  let notonchannel c nick ~channel =
    write_string c ":%s 442 %s %s :You're not on that channel\r\n" my_hostname nick channel

  let noorigin c nick =
    write_string c ":%s 409 %s :No origin specified\r\n" my_hostname nick

  let norecipient c nick ~cmd =
    write_string c ":%s 411 %s :No recipient given (%s)\r\n" my_hostname nick cmd

  let nicknameinuse c n ~nick =
    write_string c ":%s 433 %s %s :Nickname is already in use\r\n" my_hostname n nick
end

module type ACT = sig
  type io
  type user
  val join : io -> user -> channel:string -> unit
  val quit : io -> user -> msg:string -> unit
  val error : io -> string -> msg:string -> unit
  val privmsg : io -> user -> target:string -> msg:string -> unit
  val set_topic : io -> user -> ?topic:string -> channel:string -> unit
  val part : io -> user -> channel:string -> msg:string -> unit
  val pong : io -> string -> msg:string -> unit
end

module Act (C : V1_LWT.CHANNEL) (S : STATE with type io = C.t) : ACT with type io = C.t and type user = S.user =
struct
  type io = C.t

  type user = S.user

  open S

  let write_string c fmt =
    Printf.ksprintf (fun s -> C.write_string c s 0 (String.length s)) fmt

  let join c u ~channel =
    write_string c ":%s!%s@%s JOIN %s\r\n" u.nick u.user u.host channel

  let quit c u ~msg =
    write_string c ":%s!%s@%s QUIT :%s\r\n" u.nick u.user u.host msg

  let error c nick ~msg =
    write_string c ":%s ERROR %s :%s\r\n" my_hostname nick msg

  let privmsg c u ~target ~msg =
    write_string c ":%s!%s@%s PRIVMSG %s :%s\r\n" u.nick u.user u.host target msg

  let set_topic c u ?topic ~channel =
    match topic with
    | None ->
        write_string c ":%s!%s@%s TOPIC %s :\r\n" u.nick u.user u.host channel
    | Some topic ->
        write_string c ":%s!%s@%s TOPIC %s :%s\r\n" u.nick u.user u.host channel topic

  let part c u ~channel ~msg =
    write_string c ":%s!%s@%s PART %s %s\r\n" u.nick u.user u.host channel msg

  let pong c nick ~msg =
    write_string c ":%s PONG %s\r\n" my_hostname msg
end

exception Quit

let parse_message l =
  let cmd, params = Lexer.message (Lexing.from_string l) in
  String.uppercase cmd, params

module Commands (C : V1_LWT.CHANNEL) (S : STATE with type io = C.t) = struct

  module Rpl = Rpl (C)
  module Err = Err (C)
  module Act = Act (C) (S)

  open S

  let get_channel s name =
    try
      H.find s.channels name
    with
    | Not_found ->
        let ch = { members = []; name; topic = None } in
        H.add s.channels name ch;
        ch

  let tok lex l = lex (Lexing.from_string l)

  let nickname n =
    try
      Lexer.nickname (Lexing.from_string n)
    with
    | _ -> raise (ErroneusNickname n)

  let join s u = function
    | [] ->
        raise (NeedMoreParams "JOIN")
    | "0" :: params ->
        let msg = match params with msg :: _ -> msg | [] -> u.nick in
        List.iter begin fun ch ->
          List.iter (fun u' -> Act.part u'.io u ~channel:ch.name ~msg) ch.members;
          ch.members <- List.filter (fun u' -> u' != u) ch.members
        end u.joined;
        u.joined <- []
    | chl :: _ ->
        let chl = tok Lexer.channel_list chl in
        let chl = List.map (get_channel s) chl in
        List.iter begin fun ch ->
          if List.memq u ch.members then raise (UserOnChannel ch.name);
          ch.members <- u :: ch.members;
          u.joined <- ch :: u.joined;
          List.iter (fun u' -> Act.join u'.io u ~channel:ch.name) ch.members;
          Rpl.topic u.io ?topic:ch.topic u.nick ~channel:ch.name;
          let nicks = List.map (fun u -> u.nick) ch.members in
          Rpl.namereply u.io u.nick ~channel:ch.name ~nicks
        end chl

  let part s u = function
    | [] ->
        raise (NeedMoreParams "PART")
    | chl :: msg ->
        let chl = tok Lexer.channel_list chl in
        let msg = match msg with [] -> u.nick | msg :: _ -> msg in
        List.iter begin fun ch ->
          if not (H.mem s.channels ch) then raise (NoSuchChannel ch);
          let ch = H.find s.channels ch in
          if not (List.memq ch u.joined) then raise (NotOnChannel ch.name);
          List.iter (fun u' -> Act.part u'.io u ~channel:ch.name ~msg) ch.members;
          u.joined <- List.filter (fun ch' -> ch' != ch) u.joined;
          ch.members <- List.filter (fun u' -> u' != u) ch.members
        end chl

  let privmsg s u = function
    | [] ->
        raise (NoRecipient "PRIVMSG")
    | _ :: [] ->
        raise (NoTextToSend)
    | msgtarget :: msg :: _ ->
        let targets = tok Lexer.msgtarget msgtarget in
        List.iter begin function
          | `Channel chan ->
              if not (H.mem s.channels chan) then raise (NoSuchNick chan);
              let ch = H.find s.channels chan in
              List.iter (fun u' -> if u != u' then Act.privmsg u'.io u ~target:chan ~msg) ch.members
          | `Nick n ->
              if not (H.mem s.users n) then raise (NoSuchNick n);
              let u' = H.find s.users n in
              Act.privmsg u'.io u ~target:u'.nick ~msg
        end targets

  let quit s u params =
    let msg = match params with [] -> "" | msg :: _ -> msg in
    List.iter begin fun ch ->
      ch.members <- List.filter (fun u' -> u' != u) ch.members;
      List.iter (fun u' -> Act.quit u'.io u ~msg) ch.members
    end u.joined;
    Act.error u.io u.nick "Bye!";
    raise Quit

  let user _ u _ =
    raise (AlreadyRegistered)

  let topic s u = function
    | [] ->
        raise (NeedMoreParams "TOPIC")
    | ch :: [] ->
        let ch = tok Lexer.channel ch in
        if not (H.mem s.channels ch) then raise (NotOnChannel ch);
        let c = H.find s.channels ch in
        if not (List.memq c u.joined) then raise (NotOnChannel ch);
        Rpl.topic u.io ?topic:c.topic u.nick ~channel:ch
    | ch :: topic :: _ ->
        let ch = tok Lexer.channel ch in
        let topic = match topic with "" -> None | _ -> Some topic in
        if not (H.mem s.channels ch) then raise (NotOnChannel ch);
        let c = H.find s.channels ch in
        if not (List.memq c u.joined) then raise (NotOnChannel ch);
        c.topic <- topic; (* FIXME perms *)
        List.iter (fun u' -> Act.set_topic u'.io u ?topic ~channel:ch) c.members

  let ping s u = function
    | [] ->
        raise (NoOrigin)
    | origin :: _ ->
        Act.pong u.io u.nick ~msg:origin

  let ison s u = function
    | [] ->
        raise (NeedMoreParams "ISON")
    | nicks ->
        let nicks = List.map nickname nicks in
        let nicks = List.filter (H.mem s.users) nicks in
        Rpl.ison u.io u.nick nicks

  let commands : (S.server -> S.user -> string list -> unit) H.t = H.create 0

  let _ =
    List.iter (fun (k, v) -> H.add commands k v)
      [ "JOIN",    join;
        "PART",    part;
        "PRIVMSG", privmsg;
        "QUIT",    quit;
        "USER",    user;
        "TOPIC",   topic;
        "PING",    ping;
        "ISON",    ison ]
end

module Main (Con : V1_LWT.CONSOLE) (SV4 : V1_LWT.STACKV4) = struct

  module C = Channel.Make (SV4.TCPV4)
  module S = State (C)
  module Commands = Commands (C) (S)
  module Err = Commands.Err
  module Act = Commands.Act
  module Rpl = Commands.Rpl
  module Dns = Dns_resolver_mirage.Make (OS.Time) (SV4)

  open S

  let (>>=) = Lwt.(>>=)

  let read_line io =
    lwt cs = C.read_line io in
    let str = String.create (Cstruct.lenv cs) in
    let rec loop off = function
      | [] -> ()
      | cs :: rest ->
          Cstruct.blit_to_string cs 0 str off (Cstruct.len cs);
          loop (off + Cstruct.len cs) rest
    in
    loop 0 cs;
    Lwt.return str

  let log con fmt =
    Printf.ksprintf (fun s -> ignore (Con.log_s con s)) fmt

  let handle_registration con dns s io =
    let rec try_register n u =
      match n, u with
      | Some n, Some (u, r) ->
          Lwt.return (n, u, r)
      | _ ->
          wait_register n u
    and wait_register n u =
      lwt () = C.flush io in
      lwt l = read_line io in
      log con "<- %S\n" l;
      match parse_message l with
      | "NICK", [] ->
          Err.nonicknamegiven io (match n with None -> "*" | Some n -> n);
          try_register n u
      | "NICK", nick :: _ ->
          if H.mem s.users nick then begin
            Err.nicknameinuse io "*" ~nick;
            try_register n u
          end else
            try_register (Some nick) u
      | "USER", u :: _ :: _ :: r :: _ ->
          try_register n (Some (u, r))
      | "USER", _ ->
          Err.needmoreparams io (match n with None -> "*" | Some n -> n) ~cmd:"USER";
          try_register n u
      | _ ->
          Err.notregistered io (match n with None -> "*" | Some n -> n);
          try_register n u
      | exception _ ->
          try_register n u
    in
    (* let addr, _ = SV4.TCPV4.get_dest (C.to_flow io) in *)
    (* lwt sl = Dns.gethostbyaddr dns addr and n, u, r = try_register None None in *)
    lwt sl = Lwt.return [] and n, u, r = try_register None None in
    let u =
      { nick = n; user = u; host = (match sl with n :: _ -> n | [] -> "*");
        realname = r; joined = []; io; last_act = Unix.time () }
    in
    Rpl.welcome io u.nick ~message:"Welcome to the Mirage IRC Server";
    Rpl.motd io u.nick ~motd;
    H.add s.users n u;
    Lwt.return u

  let handle_message con s u l =
    log con "<- %S" l;
    let m, params =
      try
        parse_message l
      with exn ->
        log con "Error while parsing: %s" (Printexc.to_string exn);
        raise exn
    in
    try
      let cmd = try H.find Commands.commands m with Not_found -> raise (UnknownCommand m) in
      cmd s u params
    with
    | NoNicknameGiven ->
        Err.nonicknamegiven u.io u.nick
    | NeedMoreParams cmd ->
        Err.needmoreparams u.io u.nick ~cmd
    | ErroneusNickname n ->
        Err.erroneusnickname u.io u.nick
    | UnknownCommand cmd ->
        Err.unknowncommand u.io u.nick ~cmd
    | NoTextToSend ->
        Err.notexttosend u.io u.nick
    | NoOrigin ->
        Err.noorigin u.io u.nick
    | NoRecipient cmd ->
        Err.norecipient u.io u.nick ~cmd
    | NotOnChannel channel ->
        Err.notonchannel u.io u.nick ~channel
    | NoSuchNick target ->
        Err.nosuchnick u.io u.nick ~target
    | NoSuchChannel channel ->
        Err.nosuchchannel u.io u.nick ~channel
    | AlreadyRegistered ->
        Err.alreadyregistered u.io u.nick
    | UserOnChannel channel ->
        Err.useronchannel u.io u.nick ~channel
    | exn ->
        log con "Error while handling: %s" (Printexc.to_string exn)

  let handle_client con dns s flow =
    let io = C.create flow in
    lwt u = handle_registration con dns s io in
    let rec loop () =
      lwt () = C.flush io in
      lwt l = read_line io in
      handle_message con s u l;
      loop ()
    in
    try_lwt
      loop ()
    with
    | Quit ->
        H.remove s.users u.nick;
        List.iter (fun ch -> ch.members <- List.filter (fun u' -> u' != u) ch.members) u.joined;
        SV4.TCPV4.close flow
    | _ ->
        H.remove s.users u.nick;
        List.iter (fun ch -> ch.members <- List.filter (fun u' -> u' != u) ch.members) u.joined;
        List.iter begin fun ch ->
          List.iter begin fun u' ->
            if u' != u then Act.quit u'.io u ~msg:"Connection closed by peer"
          end ch.members
        end u.joined;
        SV4.TCPV4.close flow

  let start con sv4 =
    let s = { channels = H.create 0; users = H.create 0 } in
    let dns = Dns.create sv4 in
    SV4.listen_tcpv4 sv4 ~port:6667 (handle_client con dns s);
    SV4.listen sv4
end

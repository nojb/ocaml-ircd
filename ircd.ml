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

exception IOError

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

module State = struct
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
      mutable last_act : float;
      out : string -> unit }

  type server =
    { channels : channel H.t;
      users    : user H.t }
end

let motd = [ "Welcome to the Mirage IRC Server";
             "Enjoy!" ]

let my_hostname =
  Unix.gethostname ()

module type RPL = sig
  val welcome : string -> message:string -> string
  val motd : string -> motd:string list -> string
  val topic : string -> ?topic:string -> channel:string -> string
  val namereply : string -> channel:string -> nicks:string list -> string
  val ison : string -> nicks:string list -> string
end

module Rpl : RPL = struct
  let welcome nick ~message =
    Printf.sprintf ":%s 001 %s :%s\r\n" my_hostname nick message

  let motd nick ~motd =
    let b = Buffer.create 0 in
    Printf.bprintf b ":%s 375 %s :- Mirage IRC Message of the day - \r\n" my_hostname nick;
    List.iter (Printf.bprintf b ":%s 372 %s :- %s\r\n" my_hostname nick) motd;
    Printf.bprintf b ":%s 376 %s :End of /MOTD command\r\n" my_hostname nick;
    Buffer.contents b

  let topic nick ?topic ~channel =
    match topic with
    | None ->
        Printf.sprintf ":%s 331 %s %s :No topic is set\r\n" my_hostname nick channel
    | Some topic ->
        Printf.sprintf ":%s 332 %s %s :%s\r\n" my_hostname nick channel topic

  let namereply nick ~channel ~nicks =
    let b = Buffer.create 0 in
    let rec loop start nicks =
      Printf.bprintf b ":%s 353 %s = %s :" my_hostname nick channel;
      loop' start nicks
    and loop' start = function
      | nick :: nicks ->
          if Buffer.length b - start + String.length nick + 1 <= 510 then begin
            Buffer.add_char b ' ';
            Buffer.add_string b nick;
            loop' start nicks
          end else begin
            Buffer.add_string b "\r\n";
            loop (Buffer.length b) nicks
          end
      | [] ->
          Buffer.add_string b "\r\n";
          Printf.bprintf b ":%s 366 %s %s :End of /NAMES list\r\n" my_hostname nick channel
    in
    loop 0 nicks;
    Buffer.contents b

    (* Printf.sprintf ":%s 353 %s = %s :%s\r\n" my_hostname nick channel (String.concat " " nicks); *)
    (* Printf.sprintf ":%s 366 %s %s :End of /NAMES list\r\n" my_hostname nick channel *)

  let ison nick ~nicks =
    Printf.sprintf ":%s 303 %s :%s\r\n" my_hostname nick (String.concat " " nicks)
end

module type ERR = sig
  val notregistered     : string -> string
  val useronchannel     : string -> channel:string -> string
  val notexttosend      : string -> string
  val nonicknamegiven   : string -> string
  val needmoreparams    : string -> cmd:string -> string
  val erroneusnickname  : string -> string
  val unknowncommand    : string -> cmd:string -> string
  val nosuchnick        : string -> target:string -> string
  val alreadyregistered : string -> string
  val nosuchchannel     : string -> channel:string -> string
  val notonchannel      : string -> channel:string -> string
  val noorigin          : string -> string
  val norecipient       : string -> cmd:string -> string
  val nicknameinuse     : string -> nick:string -> string
end

module Err : ERR = struct
  let notregistered nick =
    Printf.sprintf ":%s 451 %s :You have not registered\r\n" nick my_hostname

  let useronchannel nick ~channel =
    Printf.sprintf ":%s 443 %s %s :is already on channel\r\n" my_hostname nick channel

  let notexttosend nick =
    Printf.sprintf ":%s 412 %s :No text to send\r\n" my_hostname nick

  let nonicknamegiven nick =
    Printf.sprintf ":%s 431 %s :No nickname given\r\n" my_hostname nick

  let needmoreparams nick ~cmd =
    Printf.sprintf ":%s 461 %s %s :Not enough parameters\r\n" my_hostname nick cmd

  let erroneusnickname nick =
    Printf.sprintf ":%s 432 %s :Erroneus nickname\r\n" my_hostname nick

  let unknowncommand nick ~cmd =
    Printf.sprintf ":%s 421 %s %s :Unknown command\r\n" my_hostname nick cmd

  let nosuchnick nick ~target =
    Printf.sprintf ":%s 401 %s %s :No such nick/channel\r\n" my_hostname nick target

  let alreadyregistered nick =
    Printf.sprintf ":%s 462 %s :Unauthorized command (already registered)\r\n" my_hostname nick

  let nosuchchannel nick ~channel =
    Printf.sprintf ":%s 403 %s %s :No such channel\r\n" my_hostname nick channel

  let notonchannel nick ~channel =
    Printf.sprintf ":%s 442 %s %s :You're not on that channel\r\n" my_hostname nick channel

  let noorigin nick =
    Printf.sprintf ":%s 409 %s :No origin specified\r\n" my_hostname nick

  let norecipient nick ~cmd =
    Printf.sprintf ":%s 411 %s :No recipient given (%s)\r\n" my_hostname nick cmd

  let nicknameinuse n ~nick =
    Printf.sprintf ":%s 433 %s %s :Nickname is already in use\r\n" my_hostname n nick
end

module type ACT = sig
  val join    : State.user -> channel:string -> string
  val quit    : State.user -> msg:string -> string
  val error   : string -> msg:string -> string
  val privmsg : State.user -> target:string -> msg:string -> string
  val topic   : State.user -> ?topic:string -> channel:string -> string
  val part    : State.user -> channel:string -> msg:string -> string
  val pong    : string -> msg:string -> string
end

module Act : ACT = struct
  open State

  let join u ~channel =
    Printf.sprintf ":%s!%s@%s JOIN %s\r\n" u.nick u.user u.host channel

  let quit u ~msg =
    Printf.sprintf ":%s!%s@%s QUIT :%s\r\n" u.nick u.user u.host msg

  let error nick ~msg =
    Printf.sprintf ":%s ERROR %s :%s\r\n" my_hostname nick msg

  let privmsg u ~target ~msg =
    Printf.sprintf ":%s!%s@%s PRIVMSG %s :%s\r\n" u.nick u.user u.host target msg

  let topic u ?topic ~channel =
    match topic with
    | None ->
        Printf.sprintf ":%s!%s@%s TOPIC %s :\r\n" u.nick u.user u.host channel
    | Some topic ->
        Printf.sprintf ":%s!%s@%s TOPIC %s :%s\r\n" u.nick u.user u.host channel topic

  let part u ~channel ~msg =
    Printf.sprintf ":%s!%s@%s PART %s %s\r\n" u.nick u.user u.host channel msg

  let pong nick ~msg =
    Printf.sprintf ":%s PONG %s\r\n" my_hostname msg
end

exception Quit

let parse_message l =
  let cmd, params = Lexer.message (Lexing.from_string l) in
  String.uppercase cmd, params

module Commands = struct
  open State

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
          List.iter (fun u' -> u'.out (Act.part u ~channel:ch.name ~msg)) ch.members;
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
          List.iter (fun u' -> u'.out (Act.join u ~channel:ch.name)) ch.members;
          u.out (Rpl.topic ?topic:ch.topic u.nick ~channel:ch.name);
          let nicks = List.map (fun u -> u.nick) ch.members in
          u.out (Rpl.namereply u.nick ~channel:ch.name ~nicks)
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
          List.iter (fun u' -> u'.out (Act.part u ~channel:ch.name ~msg)) ch.members;
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
              List.iter (fun u' -> if u != u' then u'.out (Act.privmsg u ~target:chan ~msg)) ch.members
          | `Nick n ->
              if not (H.mem s.users n) then raise (NoSuchNick n);
              let u' = H.find s.users n in
              u'.out (Act.privmsg u ~target:u'.nick ~msg)
        end targets

  let quit s u params =
    let msg = match params with [] -> "" | msg :: _ -> msg in
    H.remove s.users u.nick;
    List.iter begin fun ch ->
      List.iter (fun u' -> u'.out (Act.quit u ~msg)) ch.members;
      ch.members <- List.filter (fun u' -> u' != u) ch.members
    end u.joined;
    u.out (Act.error u.nick "Bye!");
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
        u.out (Rpl.topic ?topic:c.topic u.nick ~channel:ch)
    | ch :: topic :: _ ->
        let ch = tok Lexer.channel ch in
        let topic = match topic with "" -> None | _ -> Some topic in
        if not (H.mem s.channels ch) then raise (NotOnChannel ch);
        let c = H.find s.channels ch in
        if not (List.memq c u.joined) then raise (NotOnChannel ch);
        c.topic <- topic; (* FIXME perms *)
        List.iter (fun u' -> u'.out (Act.topic u ?topic ~channel:ch)) c.members

  let ping s u = function
    | [] ->
        raise (NoOrigin)
    | origin :: _ ->
        u.out (Act.pong u.nick ~msg:origin)

  let ison s u = function
    | [] ->
        raise (NeedMoreParams "ISON")
    | nicks ->
        let nicks = List.map nickname nicks in
        let nicks = List.filter (H.mem s.users) nicks in
        u.out (Rpl.ison u.nick nicks)

  let commands : (State.server -> State.user -> string list -> unit) H.t = H.create 0

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

  module Ch = Channel.Make (SV4.TCPV4)
  module Dns = Dns_resolver_mirage.Make (OS.Time) (SV4)

  open State

  let (>>=) = Lwt.(>>=)

  let logger = ref (fun _ -> ())

  let log fmt = Printf.ksprintf !logger fmt

  let read_line io =
    lwt cs = Ch.read_line io in
    let str = Bytes.create (Cstruct.lenv cs) in
    let rec loop off = function
      | [] -> ()
      | cs :: rest ->
          Cstruct.blit_to_string cs 0 str off (Cstruct.len cs);
          loop (off + Cstruct.len cs) rest
    in
    loop 0 cs;
    Lwt.return str

  let handle_registration dns s ip inps out =
    let rec try_register n u =
      match n, u with
      | Some n, Some (u, r) ->
          Lwt.return (n, u, r)
      | _ ->
          wait_register n u
    and wait_register n u =
      lwt l = Lwt_stream.next inps in
      match parse_message l with
      | "NICK", [] ->
          out (Err.nonicknamegiven (match n with None -> "*" | Some n -> n));
          try_register n u
      | "NICK", nick :: _ ->
          if H.mem s.users nick then begin
            out (Err.nicknameinuse "*" ~nick);
            try_register n u
          end else
            try_register (Some nick) u
      | "USER", u :: _ :: _ :: r :: _ ->
          try_register n (Some (u, r))
      | "USER", _ ->
          out (Err.needmoreparams (match n with None -> "*" | Some n -> n) ~cmd:"USER");
          try_register n u
      | _ ->
          out (Err.notregistered (match n with None -> "*" | Some n -> n));
          try_register n u
      | exception _ ->
          try_register n u
    in
    (* lwt sl = Dns.gethostbyaddr dns ip and n, u, r = try_register None None in *)
    lwt sl = Lwt.return [] and n, u, r = try_register None None in
    let u =
      { nick = n; user = u; host = (match sl with n :: _ -> n | [] -> Ipaddr.V4.to_string ip);
        realname = r; joined = []; out; last_act = Unix.time () }
    in
    u.out (Rpl.welcome u.nick ~message:"Welcome to the Mirage IRC Server");
    u.out (Rpl.motd u.nick ~motd);
    H.add s.users n u;
    Lwt.return u

  let handle_message s u l =
    let m, params =
      try
        parse_message l
      with exn ->
        log "Error while parsing: %s (%s)" l (Printexc.to_string exn);
        raise exn
    in
    try
      let cmd = try H.find Commands.commands m with Not_found -> raise (UnknownCommand m) in
      cmd s u params
    with
    | NoNicknameGiven ->
        u.out (Err.nonicknamegiven u.nick)
    | NeedMoreParams cmd ->
        u.out (Err.needmoreparams u.nick ~cmd)
    | ErroneusNickname n ->
        u.out (Err.erroneusnickname u.nick)
    | UnknownCommand cmd ->
        u.out (Err.unknowncommand u.nick ~cmd)
    | NoTextToSend ->
        u.out (Err.notexttosend u.nick)
    | NoOrigin ->
        u.out (Err.noorigin u.nick)
    | NoRecipient cmd ->
        u.out (Err.norecipient u.nick ~cmd)
    | NotOnChannel channel ->
        u.out (Err.notonchannel u.nick ~channel)
    | NoSuchNick target ->
        u.out (Err.nosuchnick u.nick ~target)
    | NoSuchChannel channel ->
        u.out (Err.nosuchchannel u.nick ~channel)
    | AlreadyRegistered ->
        u.out (Err.alreadyregistered u.nick)
    | UserOnChannel channel ->
        u.out (Err.useronchannel u.nick ~channel)

  let handle_quit s u ~msg =
    H.remove s.users u.nick;
    List.iter begin fun ch ->
      List.iter (fun u' -> u'.out (Act.quit u ~msg)) ch.members;
      ch.members <- List.filter (fun u' -> u' != u) ch.members
    end u.joined

  let handle_client dns s flow =
    let inps, inp = Lwt_stream.create () in
    let outs, out = Lwt_stream.create () in
    let io_err, io_err_signal = let t, u = Lwt.wait () in (t >> raise_lwt IOError), u in
    let ch = Ch.create flow in
    let rec read_loop () =
      let rec loop () =
        lwt l = read_line ch in
        log "<- %S" l;
        inp (Some l);
        loop ()
      in
      try_lwt
        loop ()
      with
      | exn ->
          log "Error during reading: %s" (Printexc.to_string exn);
          inp None;
          Lwt.return_unit
    in
    let rec write_loop () =
      let rec loop () =
        lwt str = Lwt_stream.next outs in
        Ch.write_string ch str 0 (String.length str);
        lwt () = Ch.flush ch in
        log "-> %S" str;
        loop ()
      in
      try_lwt
        loop ()
      with
      | exn ->
          log "Error during writing: %s" (Printexc.to_string exn);
          out None;
          Lwt.return_unit
    in
    let client_loop () = Lwt.pick [read_loop (); write_loop ()] >> Lwt.wrap1 Lwt.wakeup io_err_signal in
    Lwt.async client_loop;
    lwt u = handle_registration dns s (fst (SV4.TCPV4.get_dest flow)) inps (fun x -> out (Some x)) in
    try_lwt
      Lwt.pick [ Lwt_stream.iter (handle_message s u) inps ; io_err ]
    with
    | Quit ->
        SV4.TCPV4.close flow
    | IOError ->
        handle_quit s u ~msg:"Connection closed by peer";
        SV4.TCPV4.close flow
    | exn ->
        log "Error while handling: %s" (Printexc.to_string exn);
        handle_quit s u ~msg:"Unhandled exception";
        SV4.TCPV4.close flow
    (* with *)
    (* | Quit -> *)
    (*     H.remove s.users u.nick; *)
    (*     List.iter (fun ch -> ch.members <- List.filter (fun u' -> u' != u) ch.members) u.joined; *)
    (*     SV4.TCPV4.close flow *)
    (* | _ -> *)
    (*     H.remove s.users u.nick; *)
    (*     List.iter (fun ch -> ch.members <- List.filter (fun u' -> u' != u) ch.members) u.joined; *)
    (*     List.iter begin fun ch -> *)
    (*       List.iter begin fun u' -> *)
    (*         if u' != u then u'.out (Act.quit u ~msg:"Connection closed by peer") *)
    (*       end ch.members *)
    (*     end u.joined; *)
    (*     SV4.TCPV4.close flow *)

  let start con sv4 =
    logger := Con.log con;
    let s = { channels = H.create 0; users = H.create 0 } in
    let dns = Dns.create sv4 in
    SV4.listen_tcpv4 sv4 ~port:6667 (handle_client dns s);
    SV4.listen sv4
end

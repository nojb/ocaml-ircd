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

module Rpl = struct
  let welcome nick ~message =
    Printf.sprintf ":%s 001 %s :%s" my_hostname nick message

  let motd nick ~motd =
    let first = Printf.sprintf ":%s 375 %s :- Mirage IRC Message of the day - " my_hostname nick in
    first :: List.fold_right (fun l xs -> Printf.sprintf ":%s 372 %s :- %s" my_hostname nick l :: xs) motd
      [Printf.sprintf ":%s 376 %s :End of /MOTD command" my_hostname nick]

  let topic nick ?topic ~channel =
    match topic with
    | None ->
        Printf.sprintf ":%s 331 %s %s :No topic is set" my_hostname nick channel
    | Some topic ->
        Printf.sprintf ":%s 332 %s %s :%s" my_hostname nick channel topic

  let namereply nick ~channel ~nicks =
    let b = Buffer.create 0 in
    let rec loop nicks rest =
      match nicks with
      | [] ->
          rest
      | nick :: nicks ->
          Printf.bprintf b ":%s 353 %s = %s :%s" my_hostname nick channel nick;
          loop' nicks rest
    and loop' nicks rest =
      match nicks with
      | nick :: nicks ->
          if Buffer.length b + String.length nick + 1 <= 510 then begin
            Buffer.add_char b ' ';
            Buffer.add_string b nick;
            loop' nicks rest
          end else begin
            let l = Buffer.contents b in
            Buffer.clear b;
            l :: loop nicks rest
          end
      | [] ->
          Buffer.contents b :: rest
    in
    loop nicks [Printf.sprintf ":%s 366 %s %s :End of /NAMES list" my_hostname nick channel]

  (* val split_lines : header:string -> sep:string -> string list -> string list *)

  let ison nick ~nicks =
    [Printf.sprintf ":%s 303 %s :%s" my_hostname nick (String.concat " " nicks)]

  let list nick ~channel ~visible ?topic =
    Printf.sprintf "%s 322 %s %s %d :%s" my_hostname nick channel visible
      (match topic with None -> "No topic is set" | Some t -> t)

  let listend nick =
    Printf.sprintf "%s 323 %s :End of /LIST" my_hostname nick

  let whoisuser n ~nick ~user ~host ~realname =
    Printf.sprintf "%s 311 %s %s %s %s * :%s" my_hostname n nick user host realname

  let whoisidle n ~nick ~idle =
    Printf.sprintf "%s 317 %s %s %.0f :seconds idle" my_hostname n nick idle

  let whoischannels n ~nick ~channels =
    Printf.sprintf "%s 319 %s %s :%s" my_hostname n nick (String.concat " " channels)

  let endofwhois nick =
    Printf.sprintf "%s 318 %s :End of /WHOIS list" my_hostname nick
end

module Err = struct
  let notregistered nick =
    Printf.sprintf ":%s 451 %s :You have not registered" my_hostname nick

  let useronchannel nick ~channel =
    Printf.sprintf ":%s 443 %s %s :is already on channel" my_hostname nick channel

  let notexttosend nick =
    Printf.sprintf ":%s 412 %s :No text to send" my_hostname nick

  let nonicknamegiven nick =
    Printf.sprintf ":%s 431 %s :No nickname given" my_hostname nick

  let needmoreparams nick ~cmd =
    Printf.sprintf ":%s 461 %s %s :Not enough parameters" my_hostname nick cmd

  let erroneusnickname nick =
    Printf.sprintf ":%s 432 %s :Erroneus nickname" my_hostname nick

  let unknowncommand nick ~cmd =
    Printf.sprintf ":%s 421 %s %s :Unknown command" my_hostname nick cmd

  let nosuchnick nick ~target =
    Printf.sprintf ":%s 401 %s %s :No such nick/channel" my_hostname nick target

  let alreadyregistered nick =
    Printf.sprintf ":%s 462 %s :Unauthorized command (already registered)" my_hostname nick

  let nosuchchannel nick ~channel =
    Printf.sprintf ":%s 403 %s %s :No such channel" my_hostname nick channel

  let notonchannel nick ~channel =
    Printf.sprintf ":%s 442 %s %s :You're not on that channel" my_hostname nick channel

  let noorigin nick =
    Printf.sprintf ":%s 409 %s :No origin specified" my_hostname nick

  let norecipient nick ~cmd =
    Printf.sprintf ":%s 411 %s :No recipient given (%s)" my_hostname nick cmd

  let nicknameinuse n ~nick =
    Printf.sprintf ":%s 433 %s %s :Nickname is already in use" my_hostname n nick
end

module Act = struct
  open State

  let join u ~channel =
    Printf.sprintf ":%s!%s@%s JOIN %s" u.nick u.user u.host channel

  let quit u ~msg =
    Printf.sprintf ":%s!%s@%s QUIT :%s" u.nick u.user u.host msg

  let error ~msg =
    Printf.sprintf "ERROR :%s" msg

  let privmsg u ~target ~msg =
    Printf.sprintf ":%s!%s@%s PRIVMSG %s :%s" u.nick u.user u.host target msg

  let topic u ?topic ~channel =
    match topic with
    | None ->
        Printf.sprintf ":%s!%s@%s TOPIC %s :" u.nick u.user u.host channel
    | Some topic ->
        Printf.sprintf ":%s!%s@%s TOPIC %s :%s" u.nick u.user u.host channel topic

  let part u ~channel ~msg =
    Printf.sprintf ":%s!%s@%s PART %s %s" u.nick u.user u.host channel msg

  let pong nick ~msg =
    Printf.sprintf ":%s PONG %s" my_hostname msg
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
        u.out (Err.needmoreparams u.nick ~cmd:"JOIN")
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
          if List.memq u ch.members then
            u.out (Err.useronchannel u.nick ~channel:ch.name)
          else begin
            ch.members <- u :: ch.members;
            u.joined <- ch :: u.joined;
            List.iter (fun u' -> u'.out (Act.join u ~channel:ch.name)) ch.members;
            u.out (Rpl.topic ?topic:ch.topic u.nick ~channel:ch.name);
            let nicks = List.map (fun u -> u.nick) ch.members in
            List.iter u.out (Rpl.namereply u.nick ~channel:ch.name ~nicks)
          end
        end chl

  let part s u = function
    | [] ->
        u.out (Err.needmoreparams u.nick ~cmd:"PART")
    | chl :: msg ->
        let chl = tok Lexer.channel_list chl in
        let msg = match msg with [] -> u.nick | msg :: _ -> msg in
        List.iter begin fun ch ->
          if H.mem s.channels ch then begin
            let ch = H.find s.channels ch in
            if List.memq ch u.joined then begin
              List.iter (fun u' -> u'.out (Act.part u ~channel:ch.name ~msg)) ch.members;
              u.joined <- List.filter (fun ch' -> ch' != ch) u.joined;
              ch.members <- List.filter (fun u' -> u' != u) ch.members
            end else
              u.out (Err.notonchannel u.nick ~channel:ch.name)
          end else
            u.out (Err.nosuchchannel u.nick ~channel:ch)
        end chl

  let privmsg s u = function
    | [] ->
        u.out (Err.norecipient u.nick ~cmd:"PRIVMSG")
    | _ :: [] ->
        u.out (Err.notexttosend u.nick)
    | msgtarget :: msg :: _ ->
        let targets = tok Lexer.msgtarget msgtarget in
        List.iter begin function
          | `Channel chan ->
              if H.mem s.channels chan then
                let ch = H.find s.channels chan in
                List.iter (fun u' -> if u != u' then u'.out (Act.privmsg u ~target:chan ~msg)) ch.members
              else
                u.out (Err.nosuchnick u.nick ~target:chan)
          | `Nick n ->
              if H.mem s.users n then
                let u' = H.find s.users n in
                u'.out (Act.privmsg u ~target:u'.nick ~msg)
              else
                u.out (Err.nosuchnick u.nick ~target:n)
        end targets

  let quit s u params =
    let msg = match params with [] -> "Client Quit" | msg :: _ -> msg in
    H.remove s.users u.nick;
    List.iter begin fun ch ->
      List.iter (fun u' -> u'.out (Act.quit u ~msg)) ch.members;
      ch.members <- List.filter (fun u' -> u' != u) ch.members
    end u.joined;
    u.out (Act.error ~msg:(Printf.sprintf "Closing link: %s (Client Quit)" u.host));
    raise Quit

  let user _ u _ =
    u.out (Err.alreadyregistered u.nick)

  let topic s u = function
    | [] ->
        u.out (Err.needmoreparams u.nick ~cmd:"TOPIC")
    | ch :: [] ->
        let ch = tok Lexer.channel ch in
        if H.mem s.channels ch then
          let c = H.find s.channels ch in
          if List.memq c u.joined then
            u.out (Rpl.topic ?topic:c.topic u.nick ~channel:ch)
          else
            u.out (Err.notonchannel u.nick ~channel:ch)
        else
          u.out (Err.notonchannel u.nick ~channel:ch)
    | ch :: topic :: _ ->
        let ch = tok Lexer.channel ch in
        let topic = match topic with "" -> None | _ -> Some topic in
        if H.mem s.channels ch then
          let c = H.find s.channels ch in
          if List.memq c u.joined then begin
            c.topic <- topic; (* FIXME perms *)
            List.iter (fun u' -> u'.out (Act.topic u ?topic ~channel:ch)) c.members
          end else
            u.out (Err.notonchannel u.nick ~channel:ch)
        else
          u.out (Err.notonchannel u.nick ~channel:ch)

  let ping s u = function
    | [] ->
        u.out (Err.noorigin u.nick)
    | origin :: _ ->
        u.out (Act.pong u.nick ~msg:origin)

  let ison s u = function
    | [] ->
        u.out (Err.needmoreparams u.nick ~cmd:"ISON")
    | nicks ->
        let nicks = List.map nickname nicks in
        let nicks = List.filter (H.mem s.users) nicks in
        List.iter u.out (Rpl.ison u.nick nicks)

  (* FIXME handle LIST STOP *)
  let list s u = function
    | [] ->
        H.iter begin fun _ ch ->
          u.out (Rpl.list u.nick ~channel:ch.name ~visible:(List.length ch.members) ?topic:ch.topic)
        end s.channels;
        u.out (Rpl.listend u.nick)
    | chl :: _ ->
        let chl = tok Lexer.channel_list chl in
        List.iter begin fun ch ->
          if H.mem s.channels ch then
            let ch = H.find s.channels ch in
            u.out (Rpl.list u.nick ~channel:ch.name ~visible:(List.length ch.members) ?topic:ch.topic)
          else
            u.out (Err.nosuchnick u.nick ~target:ch)
        end chl;
        u.out (Rpl.listend u.nick)

  let whois s u = function
    | [] ->
        u.out (Err.needmoreparams u.nick ~cmd:"WHOIS")
    | _ :: masks :: _
    | masks :: [] ->
        let masks = Stringext.split masks ~on:',' in
        let masks = List.map (fun m s -> Lexer.mask (Lexing.from_string m) s 0) masks in
        H.iter begin fun _ u' ->
          if List.exists (fun m -> m u'.nick) masks then begin
            u.out (Rpl.whoisuser u.nick ~nick:u'.nick ~user:u'.user ~host:u'.host ~realname:u'.realname);
            u.out (Rpl.whoisidle u.nick ~nick:u'.nick ~idle:(Unix.time () -. u'.last_act));
            u.out (Rpl.whoischannels u.nick ~nick:u'.nick ~channels:(List.map (fun ch -> ch.name) u.joined))
          end
        end s.users;
        u.out (Rpl.endofwhois u.nick)

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
        "ISON",    ison;
        "LIST",    list;
        "WHOIS",   whois ]

  let find name =
    try
      H.find commands name
    with
    | Not_found -> fun _ u _ -> u.out (Err.unknowncommand u.nick ~cmd:name)
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
    List.iter u.out (Rpl.motd u.nick ~motd);
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
    u.last_act <- Unix.time ();
    Commands.find m s u params

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
        log "<- %s" l;
        inp (Some l);
        loop ()
      in
      try_lwt
        loop ()
      with
      | exn ->
          log "Error during reading: %s" (Printexc.to_string exn);
          Lwt.wakeup io_err_signal ();
          Lwt.return_unit
    in
    let rec write_loop () =
      let rec loop () =
        lwt str = Lwt_stream.next outs in
        Ch.write_string ch str 0 (String.length str);
        Ch.write_char ch '\r';
        Ch.write_char ch '\n';
        lwt () = Ch.flush ch in
        log "-> %s" str;
        loop ()
      in
      try_lwt
        loop ()
      with
      | exn ->
          log "Error during writing: %s" (Printexc.to_string exn);
          Lwt.wakeup io_err_signal ();
          Lwt.return_unit
    in
    Lwt.async (fun () -> Lwt.pick [read_loop (); write_loop ()]);
    lwt u =
      try_lwt
        lwt u = handle_registration dns s (fst (SV4.TCPV4.get_dest flow)) inps (fun x -> out (Some x)) in
        Lwt.return (`Ok u)
      with
      | _ -> Lwt.return `Fail
    in
    lwt () =
      match u with
      | `Ok u ->
          begin
            try_lwt
              Lwt.pick [ Lwt_stream.iter (handle_message s u) inps ; io_err ]
            with
            | Quit ->
                Ch.flush ch
            | IOError ->
                handle_quit s u ~msg:"Remote host closed the connection";
                Lwt.return_unit
            | exn ->
                log "Error while handling: %s" (Printexc.to_string exn);
                handle_quit s u ~msg:(Printf.sprintf "Unhandled exception: %s" (Printexc.to_string exn));
                Lwt.return_unit
          end
      | `Fail ->
          Lwt.return_unit
    in
    SV4.TCPV4.close flow

  let start con sv4 =
    logger := Con.log con;
    let s = { channels = H.create 0; users = H.create 0 } in
    let dns = Dns.create sv4 in
    SV4.listen_tcpv4 sv4 ~port:6667 (handle_client dns s);
    SV4.listen sv4
end

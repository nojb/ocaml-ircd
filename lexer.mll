{
}

let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']
let hexdigit = ['0'-'9''A'-'F']
let special = ['['']''\\''`''_''^''{''|''}']
let nospcrlfcl = [^'\000''\r''\n'' '':']
let trailing = (':' | ' ' | nospcrlfcl)*
let nickname = (letter | special) (letter | digit | special | '-')*
let user = [^'\000''\r''\n'' ''@']+
let shortname = (letter | digit) ((letter | digit | '-')* (letter | digit))?
let hostname = shortname ('.' shortname)*
let ip4part = (digit | digit digit | digit digit digit)
let ip4addr = ip4part '.' ip4part '.' ip4part '.' ip4part
let ip6addr = (hexdigit+ ':' hexdigit+ ':' hexdigit+ ':' hexdigit+ ':' hexdigit+ ':' hexdigit+ ':' hexdigit+ ':' hexdigit+) |
              ("0:0:0:0:" ('0' | "FFFF") ':' ip4addr)
let hostaddr = ip4addr | ip6addr
let host = (hostname | hostaddr)
let servername = hostname
let middle = nospcrlfcl (':' | nospcrlfcl)*
let channelid = ['A'-'Z''0'-'9']['A'-'Z''0'-'9']['A'-'Z''0'-'9']['A'-'Z''0'-'9']['A'-'Z''0'-'9']
let chanstring = [^'\000''\r''\n'' ' ','':''\007']+
let channel = ('+' | '#' | '!' channelid | '&') chanstring (':' chanstring)?
let key = ['\x01'-'\x05''\x07''\x08''\x0c''\x0e''\x1f''\x21'-'\x7f']+
let prefix = nickname (('!' user)? '@' host)?

rule trailing = parse
  | ' ' ':'? (trailing as t) eof { t :: [] }
  | ' '? eof { [] }

and params i = parse
  | ' ' ':' (trailing as t) eof { t :: [] }
  | ' ' (middle as m)
      { m :: (if i = 14 then trailing lexbuf else params (i+1) lexbuf) }
  | ' '? eof { [] }

and message = parse
  | (':' prefix ' ')? (letter+ as cmd) { cmd, params 0 lexbuf }

and channel = parse
  | channel as c eof { c }

and channel_list = parse
  | (channel as c) ',' { c :: channel_list lexbuf }
  | channel as c eof { c :: [] }

and nickname = parse
  | nickname as n eof { n }

and user = parse
  | user as u eof { u }

and key_list = parse
  | (key as k) ',' { k :: key_list lexbuf }
  | key as k eof { k :: [] }

and msgtarget2 = parse
  | ',' { msgtarget lexbuf }
  | eof { [] }

and msgtarget = parse
  | (channel as c) { `Channel c :: msgtarget2 lexbuf }
  | nickname as n ('!' user '@' host)? { `Nick n :: msgtarget2 lexbuf }

and servername = parse
  | servername as s { s }

and mask = parse
  | '?'
      { let g = mask lexbuf in fun s i -> g s (i+1) }
  | '*'
      { let g = mask lexbuf in
        let rec tryit s i =
          if i >= String.length s then
            g s i
          else
            g s i || tryit s (i+1)
        in
        tryit }
  | [^'\000''?''*']+ as str
      { let g = mask lexbuf in
        fun s i ->
          i + String.length str <= String.length s &&
          String.lowercase str = String.lowercase (String.sub s i (String.length str)) &&
          g s (i + String.length str) }
  | '\\' ('?' | '*' as c)
      { let g = mask lexbuf in
        fun s i -> i < String.length s && s.[i] = c && g s (i+1) }
  | eof
      { fun _ _ -> true }

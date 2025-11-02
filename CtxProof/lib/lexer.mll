{
  open Parser
  exception Error of string
}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }           (* skip whitespace *)
  | "%" [^ '\n' ]*       { token lexbuf }           (* skip TPTP comments *)
  | "fof"                { FOF }
  | "("                  { LPAREN }
  | ")"                  { RPAREN }
  | ","                  { COMMA }
  | "."                  { DOT }
  | ":"                  { COLON }
  | "=>"                 { IMPLIES }
  | "<=>"                { IFF }
  | "~|"                 { NOR }
  | "&"                  { AND }
  | "|"                  { OR }
  | "~"                  { NOT }
  | "!"                  { FORALL }
  | "?"                  { EXISTS }
  | "="                  { EQUAL }
  | '['                  { LBRACK }
  | ']'                  { RBRACK }
  | '\''                 { QUOTE }
  | "$false"             { FALSE }
  | "$true"              { TRUE }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']* as cname { LWORD(cname) }
  | ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as vname { UWORD(vname) }
  | ['0'-'9']+ as num        { INT (int_of_string num) }
  | "\"" ([^ '\"'])* "\"" as str { STRING (String.sub str 1 (String.length str - 2)) }
  | eof                  { EOF }
  | _ as c               { raise (Error ("Illegal character: " ^ String.make 1 c)) }

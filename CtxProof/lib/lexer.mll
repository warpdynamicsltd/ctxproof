{
  open Parser
  exception Error of string
}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }           (* skip whitespace *)
  | "%" [^ '\n' ]*       { token lexbuf }           (* skip TPTP comments *)
  | "("                  { LPAREN }
  | ")"                  { RPAREN }
  | ","                  { COMMA }
  | ":"                  { COLON }
  | "=>"                 { IMPLIES }
  | "<=>"                { IFF }
  | "&"                  { AND }
  | "|"                  { OR }
  | "~"                  { NOT }
  | "!"                  { FORALL }
  | "?"                  { EXISTS }
  | "="                  { EQUAL }
  | '['                  { LBRACK }
  | ']'                  { RBRACK }
  | "$false"             { FALSE }
  | "$true"              { TRUE }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']* as cname { LWORD(cname) }
  | ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as vname { UWORD(vname) }
  | eof                  { EOF }
  | _ as c               { raise (Error ("Illegal character: " ^ String.make 1 c)) }

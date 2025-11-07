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
  | "."                  { DOT }
  | ":"                  { COLON }
  | ";"                  { SEMICOLON }
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
  | "{"                  { LCURL }
  | "}"                  { RCURL }
  | "$false"             { FALSE }
  | "$true"              { TRUE }
  | "sk."                { SK }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']* as cname { LWORD(cname) }
  | ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as vname { UWORD(vname) }
  | ['0'-'9']+ as lxm    { INT(Z.of_string lxm) }
  | eof                  { EOF }
  | _ as c               { raise (Error ("Illegal character: " ^ String.make 1 c)) }

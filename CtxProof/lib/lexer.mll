{
  open Parser
  exception Error of string

  let new_line (lexbuf : Lexing.lexbuf) =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- {
      pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;  (* not lex_curr_pos *)
    }
}

rule token = parse
  | [' ' '\t' '\r']+     { token lexbuf }
  | ['\n']               { new_line lexbuf; token lexbuf }
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

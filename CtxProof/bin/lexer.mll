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
(*  | "<~>"                { NIFF }*)
  | "~|"                 { NOR }
  | "~&"                 { NAND }
  | "&"                  { AND }
  | "|"                  { OR }
  | "~"                  { NOT }
  | "!"                  { FORALL }
  | "?"                  { EXISTS }
  | "="                  { EQUAL }
  | "!="                 { NEQ }
  | '['                  { LBRACK }
  | ']'                  { RBRACK }
  | '\''                 { QUOTE }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as id {
      match id with
      | "true"  -> TRUE
      | "false" -> FALSE
      | _       -> IDENT id
    }
  | ['0'-'9']+ as num        { INT (int_of_string num) }
  | "\"" ([^ '\"'])* "\"" as str { STRING (String.sub str 1 (String.length str - 2)) }
  | eof                  { EOF }
  | _ as c               { raise (Error ("Illegal character: " ^ String.make 1 c)) }

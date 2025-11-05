let parse_string s =
    let lexbuf =  Lexing.from_string s in
    Parser.wff Lexer.token lexbuf

let statement_of_string s =
    let lexbuf =  Lexing.from_string s in
    Parser.line Lexer.token lexbuf
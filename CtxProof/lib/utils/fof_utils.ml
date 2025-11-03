let parse_string s =
    let lexbuf =  Lexing.from_string s in
    Parser.wff Lexer.token lexbuf
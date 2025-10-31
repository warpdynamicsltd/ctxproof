open Ctxproof

let parse_string s = 
  let lexbuf = Lexing.from_string s in
  Parser.input Lexer.token lexbuf != []

let () =
  assert (parse_string("fof(r, axiom, $false, )."));
  assert (parse_string("fof(r_0, axiom, $false, )."));
  assert (parse_string("fof(r_0_0, axiom, $false, )."));
  assert (parse_string("fof(r_0_1, axiom, $false, )."));
  print_endline "All tests passed!" 
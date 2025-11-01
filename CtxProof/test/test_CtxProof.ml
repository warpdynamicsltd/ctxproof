open Ctxproof

let parse_string s =
  let lexbuf =  Lexing.from_string s in
  Parser.input Lexer.token lexbuf

let () =
  assert (parse_string("$true") = True);
  assert (parse_string("$false") = False);
  assert (parse_string("p") = Pred ("p", []));
  assert (parse_string("p(X)") = Pred ("p", [Var "X"]));
  assert (parse_string("p(X,Y)") = Pred ("p", [Var "X"; Var "Y"]));
  assert (parse_string("p(c)") = Pred ("p", [Const "c"]));
  assert (parse_string("p(f(X))") = Pred ("p", [Func("f", [Var "X"])]));
  assert (parse_string("p(f(X),g(X))") = Pred ("p", [Func("f", [Var "X"]); Func("g", [Var "X"])]));
  print_endline "All tests passed!" 
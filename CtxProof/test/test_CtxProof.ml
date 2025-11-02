open Ctxproof
open Fof_utils

let not_wff s = 
  try
    let _ = parse_string s in
    false
  with _ -> true

let () =
  assert (parse_string("$true") = True);
  assert (parse_string("$false") = False);
  assert (parse_string("p") = Pred ("p", []));
  assert (parse_string("(p)") = Pred ("p", []));
  assert (parse_string("p(X)") = Pred ("p", [Var "X"]));
  assert (parse_string("p(X,Y)") = Pred ("p", [Var "X"; Var "Y"]));
  assert (parse_string("p(c)") = Pred ("p", [Const "c"]));
  assert (parse_string("p(f(X))") = Pred ("p", [Func("f", [Var "X"])]));
  assert (parse_string("p(f(c))") = Pred ("p", [Func("f", [Const "c"])]));
  assert (parse_string("p(f(X),g(X))") = Pred ("p", [Func("f", [Var "X"]); Func("g", [Var "X"])]));

  assert (not_wff "$truth");
  assert (not_wff "$falsity");
  assert (not_wff "X");
  assert (not_wff "p(");
  assert (not_wff "p)");
  assert (not_wff "p(X");
  assert (not_wff "P & P");
  assert (not_wff "p & p & p");
  assert (not_wff "p & p | p");
  assert (not_wff "p => p | p");
  print_endline "All tests passed!" 
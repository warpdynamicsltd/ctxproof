open Ctxproof
open Types
open Kernel
open Fof_utils
open Tutils

let (@) f g x = f(g(x))

let sub (v, t, s) = substitute_in_formula v t (formula_of_string s)

let not_admissible (v, t, s) = 
  try
    let _ = sub (v, t, s)
    in false
  with
    | ErrorNotAdmissible -> true

let run() = 
  assert (free_vars_term (Func ("P", [])) = StringSet.of_list []);
  assert (free_vars_term (Func ("P", [Var "X"])) = StringSet.of_list ["X"]);
  assert (free_vars_term (Func ("P", [Var "X"; Var "Y"])) = StringSet.of_list ["X"; "Y"]);
  assert (free_vars_term (Func ("P", [Var "X"; Var "X"])) = StringSet.of_list ["X"]);
  assert (free_vars_term (SkolemFunc ([Z.of_int 0], [])) = StringSet.of_list []);
  assert (free_vars_term (SkolemFunc ([Z.of_int 0], [Var "X"])) = StringSet.of_list ["X"]);
  assert (free_vars_term (SkolemFunc ([Z.of_int 0], [Var "X"; Var "Y"])) = StringSet.of_list ["X"; "Y"]);
  assert (free_vars_term (Var "X") = StringSet.of_list ["X"]);


  assert_eq __LINE__  (free_vars_formula @ formula_of_string) "p(X)" (StringSet.of_list ["X"]);
  assert_eq __LINE__  (free_vars_formula @ formula_of_string) "![X] : p(X)" (StringSet.of_list []);
  assert_eq __LINE__  (free_vars_formula @ formula_of_string) "![X] : ( p(X) & q(X) )" (StringSet.of_list []);
  assert_eq __LINE__  (free_vars_formula @ formula_of_string) "( ![X] : p(X) ) & q(X)" (StringSet.of_list ["X"]);
  assert_eq __LINE__  (free_vars_formula @ formula_of_string) "?[X] : p(X)" (StringSet.of_list []);
  assert_eq __LINE__  (free_vars_formula @ formula_of_string) "?[X] : ( p(X) & q(X) )" (StringSet.of_list []);
  assert_eq __LINE__  (free_vars_formula @ formula_of_string) "( ?[X] : p(X) ) & q(X)" (StringSet.of_list ["X"]);
  assert_eq __LINE__  (free_vars_formula @ formula_of_string) "![X] : p(X, Y)" (StringSet.of_list ["Y"]);
  assert_eq __LINE__  (free_vars_formula @ formula_of_string) "( ![X, Y] : p(X, Y, Z) ) & q(Y)" (StringSet.of_list ["Z"; "Y"]);

  assert_eq __LINE__ sub ("X", Var "Y", "p(X)") (formula_of_string "p(Y)");
  assert_eq __LINE__ sub ("X", Var "Y", "p(t(X))") (formula_of_string "p(t(Y))");
  assert_eq __LINE__ sub ("X", term_of_string "g(X, Y)", "p(t(X))") (formula_of_string "p( t(g(X, Y)) )");
  assert_eq __LINE__ sub ("X", Var "Y", "![X] : p(X)") (formula_of_string "![X] : p(X)");

  assert (not_admissible ("Y", Var "X", "![X] : p(Y)"));
  assert (not_admissible ("Y", Var "X", "![X] : p(X, Y)"));
  assert (not_admissible ("Y", Var "X", "?[X] : p(Y)"));
  assert (not_admissible ("Y", Var "X", "?[X] : p(X, Y)"));
  assert (not_admissible ("Y", term_of_string "f(X)", "![X] : p(Y)"));
  assert (not_admissible ("Y", term_of_string "f(g(X, Y), Z)", "![X] : p(Y)"));
  assert (not_admissible ("Y", term_of_string "f(X)", "?[X] : p(Y)"));
  assert (not_admissible ("Y", term_of_string "f(g(X, Y), Z)", "?[X] : p(Y)"));

  (* Additional not_admissible tests *)
  assert (not_admissible ("Y", Var "X", "![X] : (p(Y) & q(X))"));
  assert (not_admissible ("Y", Var "X", "![X] : (p(X) | r(Y))"));
  assert (not_admissible ("Y", Var "X", "![X] : (p(Y) => q(X))"));
  assert (not_admissible ("Y", Var "X", "![X] : (p(X) <=> r(Y))"));
  assert (not_admissible ("Y", Var "X", "![X] : ~p(Y)"));
  assert (not_admissible ("Y", Var "X", "?[X] : (p(Y) & q(X))"));
  assert (not_admissible ("Y", Var "X", "?[X] : (p(X) | r(Y))"));
  assert (not_admissible ("Y", Var "X", "?[X] : (p(Y) => q(X))"));
  assert (not_admissible ("Y", Var "X", "?[X] : (p(X) <=> r(Y))"));
  assert (not_admissible ("Y", Var "X", "?[X] : ~p(Y)"));

  assert (not_admissible ("Y", term_of_string "f(X)", "![X] : (p(Y) & q(X))"));
  assert (not_admissible ("Y", term_of_string "f(X)", "?[X] : (p(Y) | q(X))"));
  assert (not_admissible ("Y", term_of_string "f(g(X), h(Y))", "![X] : p(Y)"));
  assert (not_admissible ("Y", term_of_string "f(g(X), h(Y))", "?[X] : p(Y)"));
  assert (not_admissible ("Y", term_of_string "f(X, Z)", "![X] : p(Y, Z)"));
  assert (not_admissible ("Y", term_of_string "f(X, Z)", "?[X] : p(Z, Y)"));

  assert (Ref [Z.of_int 1] >> Ref [Z.of_int 0]);
  assert (Ref [Z.of_int 1; Z.of_int 1] >> Ref [Z.of_int 0]);
  assert (Ref [Z.of_int 1; Z.of_int 0] >> Ref [Z.of_int 0]);

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
    | KernelError _ -> true

let test_not_admissible_batch map formula =
    try
      let _ = batch_substitute_in_formula map (formula_of_string formula) in
      false
    with
      | KernelError NotAdmissible -> true
      | _ -> false

let check_sub pred repl formula1_str formula2_str =
    let formula1 = formula_of_string formula1_str in
    let formula2 = formula_of_string formula2_str in
    (substitute_predicate pred repl formula1 = formula2)

let test_not_admissible_pred pred repl formula_str =
    try
      let _ = substitute_predicate pred repl (formula_of_string formula_str) in
      false
    with
      | KernelError NotAdmissible -> true
      | _ -> false

let run() = 
  assert (free_vars_term (Func ("P", [])) = StringSet.of_list []);
  assert (free_vars_term (Func ("P", [Var "X"])) = StringSet.of_list ["X"]);
  assert (free_vars_term (Func ("P", [Var "X"; Var "Y"])) = StringSet.of_list ["X"; "Y"]);
  assert (free_vars_term (Func ("P", [Var "X"; Var "X"])) = StringSet.of_list ["X"]);
  assert (free_vars_term (SkolemFunc ([0], [])) = StringSet.of_list []);
  assert (free_vars_term (SkolemFunc ([0], [Var "X"])) = StringSet.of_list ["X"]);
  assert (free_vars_term (SkolemFunc ([0], [Var "X"; Var "Y"])) = StringSet.of_list ["X"; "Y"]);
  assert (free_vars_term (Var "X") = StringSet.of_list ["X"]);

  assert (var_occurs_free_in_formula "X" (formula_of_string "p(X)"));
  assert (var_occurs_free_in_formula "X" (formula_of_string "p(X) & q(Y)"));
  assert (var_occurs_free_in_formula "Y" (formula_of_string "![X]: (p(X) & q(Y))"));
  assert (not (var_occurs_free_in_formula "X" (formula_of_string "p")));


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

  (* Test sub_map function *)
  let map1 = sub_map [] [] in
  assert (StringMap.is_empty map1);

  let map2 = sub_map [Var "X"] [Var "Y"] in
  assert (StringMap.find "X" map2 = Var "Y");
  assert (StringMap.cardinal map2 = 1);

  let map3 = sub_map [Var "X"; Var "Y"; Var "Z"] [Var "A"; Const "c"; term_of_string "f(B)"] in
  assert (StringMap.find "X" map3 = Var "A");
  assert (StringMap.find "Y" map3 = Const "c");
  assert (StringMap.find "Z" map3 = term_of_string "f(B)");
  assert (StringMap.cardinal map3 = 3);

  let map4 = sub_map [Var "V1"; Var "V2"] [SkolemConst [0]; SkolemFunc ([1], [Var "X"; Var "Y"])] in
  assert (StringMap.find "V1" map4 = SkolemConst [0]);
  assert (StringMap.find "V2" map4 = SkolemFunc ([1], [Var "X"; Var "Y"]));

  (* Test duplicate variable detection *)
  let test_duplicate () =
    try
      let _ = sub_map [Var "X"; Var "Y"; Var "X"] [Var "A"; Var "B"; Var "C"] in
      false
    with
      | KernelError DuplicateVarInSubMap -> true
      | _ -> false
  in
  assert (test_duplicate ());

  (* Test term_of_map function *)
  let map5 = sub_map [Var "X"; Var "Y"] [Const "c"; term_of_string "f(A)"] in
  assert (term_of_map map5 (Var "X") = Const "c");
  assert (term_of_map map5 (Var "Y") = term_of_string "f(A)");
  assert (term_of_map map5 (Var "Z") = Var "Z");
  assert (term_of_map map5 (Const "c") = Const "c");
  assert (term_of_map map5 (term_of_string "g(B)") = term_of_string "g(B)");

  let empty_map = sub_map [] [] in
  assert (term_of_map empty_map (Var "X") = Var "X");
  assert (term_of_map empty_map (Const "c") = Const "c");

  (* Test batch_substitute_in_term function *)
  let map6 = sub_map [Var "X"; Var "Y"] [Const "a"; Const "b"] in
  assert (batch_substitute_in_term map6 (Var "X") = Const "a");
  assert (batch_substitute_in_term map6 (Var "Y") = Const "b");
  assert (batch_substitute_in_term map6 (Var "Z") = Var "Z");
  assert (batch_substitute_in_term map6 (Const "c") = Const "c");
  assert (batch_substitute_in_term map6 (term_of_string "f(X, Y)") = term_of_string "f(a, b)");
  assert (batch_substitute_in_term map6 (term_of_string "f(X, g(Y, Z))") = term_of_string "f(a, g(b, Z))");
  assert (batch_substitute_in_term map6 (term_of_string "h(W)") = term_of_string "h(W)");

  let map7 = sub_map [Var "A"] [term_of_string "f(B, C)"] in
  assert (batch_substitute_in_term map7 (term_of_string "g(A, D)") = term_of_string "g(f(B, C), D)");
  assert (batch_substitute_in_term map7 (term_of_string "g(h(A), A)") = term_of_string "g(h(f(B, C)), f(B, C))");

  (* Test simultaneous variable swap: X <- Y and Y <- X *)
  let swap_map = sub_map [Var "X"; Var "Y"] [Var "Y"; Var "X"] in
  assert (batch_substitute_in_term swap_map (Var "X") = Var "Y");
  assert (batch_substitute_in_term swap_map (Var "Y") = Var "X");
  assert (batch_substitute_in_term swap_map (term_of_string "f(X, Y)") = term_of_string "f(Y, X)");
  assert (batch_substitute_in_term swap_map (term_of_string "g(X, g(Y, X))") = term_of_string "g(Y, g(X, Y))");

  (* Test batch_substitute_in_formula function *)
  let map8 = sub_map [Var "X"; Var "Y"] [Const "a"; Const "b"] in
  assert (batch_substitute_in_formula map8 (formula_of_string "p(X)") = formula_of_string "p(a)");
  assert (batch_substitute_in_formula map8 (formula_of_string "p(X, Y)") = formula_of_string "p(a, b)");
  assert (batch_substitute_in_formula map8 (formula_of_string "p(X) & q(Y)") = formula_of_string "p(a) & q(b)");
  assert (batch_substitute_in_formula map8 (formula_of_string "p(X) | q(Y, Z)") = formula_of_string "p(a) | q(b, Z)");
  assert (batch_substitute_in_formula map8 (formula_of_string "p(X) => q(Y)") = formula_of_string "p(a) => q(b)");
  assert (batch_substitute_in_formula map8 (formula_of_string "~p(X)") = formula_of_string "~p(a)");

  (* Test quantifiers - bound variable blocks substitution *)
  assert (batch_substitute_in_formula map8 (formula_of_string "![X] : p(X)") = formula_of_string "![X] : p(X)");
  assert (batch_substitute_in_formula map8 (formula_of_string "?[Y] : p(Y)") = formula_of_string "?[Y] : p(Y)");
  assert (batch_substitute_in_formula map8 (formula_of_string "![X] : p(X, Y)") = formula_of_string "![X] : p(X, b)");
  assert (batch_substitute_in_formula map8 (formula_of_string "![Z] : p(X, Y, Z)") = formula_of_string "![Z] : p(a, b, Z)");

  let map9 = sub_map [Var "Y"] [term_of_string "f(X)"] in
  assert (test_not_admissible_batch map9 "![X] : p(Y)");
  assert (test_not_admissible_batch map9 "?[X] : p(Y, Z)");

  let map10 = sub_map [Var "Z"] [term_of_string "g(X, Y)"] in
  assert (test_not_admissible_batch map10 "![X] : p(Z)");
  assert (test_not_admissible_batch map10 "?[Y] : q(Z)");

  (* Test variable swap in formulas *)
  let swap_map2 = sub_map [Var "A"; Var "B"] [Var "B"; Var "A"] in
  assert (batch_substitute_in_formula swap_map2 (formula_of_string "p(A, B)") = formula_of_string "p(B, A)");
  assert (batch_substitute_in_formula swap_map2 (formula_of_string "p(A) & q(B)") = formula_of_string "p(B) & q(A)");

  (* Test substitute_predicate function *)
  (* Basic substitution: p(X) -> q(X, a) *)
  let pred = formula_of_string "p(X)" in
  let repl = formula_of_string "q(X, a)" in
  assert (check_sub pred repl "p(b)" "q(b, a)");
  assert (check_sub pred repl "p(f(c))" "q(f(c), a)");
  assert (check_sub pred repl "p(X)" "q(X, a)");
  assert (check_sub pred repl "p(Y)" "q(Y, a)");

  (* Multiple occurrences *)
  assert (check_sub pred repl "p(a) & p(b)" "q(a, a) & q(b, a)");
  assert (check_sub pred repl "p(X) | p(Y)" "q(X, a) | q(Y, a)");

  (* Binary predicate: p(X, Y) -> r(X, Y, X) *)
  let pred = formula_of_string "p(X, Y)" in
  let repl = formula_of_string "r(X, Y, X)" in
  assert (check_sub pred repl "p(a, b)" "r(a, b, a)");
  assert (check_sub pred repl "p(f(c), g(d))" "r(f(c), g(d), f(c))");

  (* Predicate doesn't match - no substitution *)
  let pred = formula_of_string "p(X)" in
  let repl = formula_of_string "q(X, a)" in
  assert (check_sub pred repl "qq(a)" "qq(a)");
  assert (check_sub pred repl "r(a, b)" "r(a, b)");

  (* Predicate with different arity - no substitution *)
  assert (check_sub pred repl "p(a, b)" "p(a, b)");
  let pred = formula_of_string "p(X, Y)" in
  let repl = formula_of_string "r(X, Y, X)" in
  assert (check_sub pred repl "p(a)" "p(a)");

  (* Logical connectives *)
  let pred = formula_of_string "p(X)" in
  let repl = formula_of_string "q(X, a)" in
  assert (check_sub pred repl "~p(a)" "~q(a, a)");
  assert (check_sub pred repl "p(a) => p(b)" "q(a, a) => q(b, a)");
  assert (check_sub pred repl "p(a) <=> p(b)" "q(a, a) <=> q(b, a)");

  (* Quantifiers - no bound variable conflict *)
  let pred = formula_of_string "p(U)" in
  let repl = formula_of_string "q(U)" in
  assert (check_sub pred repl "![X] : p(X)" "![X] : q(X)");
  assert (check_sub pred repl "![X] : p(a)" "![X] : q(a)");
  assert (check_sub pred repl "?[Z] : p(b)" "?[Z] : q(b)");

  (* Test: Variables in the predicate pattern CAN match bound variables - this is OK*)
  let pred = formula_of_string "p(X)" in
  let repl = formula_of_string "q(X)" in
  (* X in p(X) matches bound X, but after substitution q(X) still has X bound - this is admissible *)
  assert (check_sub pred repl "![X] : p(X)" "![X] : q(X)");


  (* p(Y) -> q(X) where X is bound and X is NOT a variable in the predicate pattern - should fail *)
  let pred = formula_of_string "p(Y)" in
  let repl = formula_of_string "q(X)" in
  assert (test_not_admissible_pred pred repl "![X] : p(a)");
  assert (test_not_admissible_pred pred repl "?[X] : p(b)");

  (* p(Z) -> r(X, Y) where X and Y are bound and NOT in predicate pattern - should fail *)
  let pred = formula_of_string "p(Z)" in
  let repl = formula_of_string "r(X, Y)" in
  assert (test_not_admissible_pred pred repl "![X] : p(c)");
  assert (test_not_admissible_pred pred repl "?[Y] : p(d)");
  assert (test_not_admissible_pred pred repl "![X] : (![Y] : p(e))");

  (* p(X, Y) -> r(X, Y, Z) where Z is bound but X and Y are in pattern - should fail only for Z *)
  let pred = formula_of_string "p(X, Y)" in
  let repl = formula_of_string "r(X, Y, Z)" in
  assert (test_not_admissible_pred pred repl "![Z] : p(a, b)");

  (* More tricky quantifier tests *)
  (* Nested quantifiers - should work when no conflicts *)
  let pred = formula_of_string "p(A)" in
  let repl = formula_of_string "q(A)" in
  assert (check_sub pred repl "![X] : (![Y] : p(Z))" "![X] : (![Y] : q(Z))");

  (* Pattern variable matches nested bound variable - should work *)
  let pred = formula_of_string "p(U)" in
  let repl = formula_of_string "r(U, U)" in
  assert (check_sub pred repl "![X] : (![Y] : p(Y))" "![X] : (![Y] : r(Y, Y))");

  (* Mixed: some occurrences under quantifiers, some not *)
  let pred = formula_of_string "p(V)" in
  let repl = formula_of_string "s(V)" in
  assert (check_sub pred repl "p(a) & (![X] : p(b))" "s(a) & (![X] : s(b))");

  (* Replacement with multiple free variables, one gets bound *)
  let pred = formula_of_string "p(W)" in
  let repl = formula_of_string "q(W, M)" in
  assert (check_sub pred repl "![X] : p(c)" "![X] : q(c, M)");
  assert (test_not_admissible_pred pred repl "![M] : p(d)");

  (* Existential quantifiers *)
  let pred = formula_of_string "p(T)" in
  let repl = formula_of_string "q(T, N)" in
  assert (check_sub pred repl "?[X] : p(e)" "?[X] : q(e, N)");
  assert (test_not_admissible_pred pred repl "?[N] : p(f)");

  (* Combination of universal and existential *)
  let pred = formula_of_string "p(S)" in
  let repl = formula_of_string "r(S)" in
  assert (check_sub pred repl "![X] : (?[Y] : p(g))" "![X] : (?[Y] : r(g))");

  (* Pattern with multiple variables, replacement uses some of them *)
  let pred = formula_of_string "p(A, B)" in
  let repl = formula_of_string "q(A, B, A)" in
  assert (check_sub pred repl "![X] : p(h(X), i)" "![X] : q(h(X), i, h(X))");

  (* Should fail: pattern variables get bound, but there's an additional free var *)
  let pred = formula_of_string "p(A, B)" in
  let repl = formula_of_string "r(A, B, C)" in
  assert (test_not_admissible_pred pred repl "![C] : p(j, k)");

  (* Should work: all free vars in replacement are pattern variables *)
  let pred = formula_of_string "p(A, B, C)" in
  let repl = formula_of_string "q(A, B, C)" in
  assert (check_sub pred repl "![A] : p(A, m, n)" "![A] : q(A, m, n)");

  let pred = formula_of_string "p(X, Y)" in
  let repl = formula_of_string "a(X) & (b(Y) & c(Z))" in
  assert (test_not_admissible_pred pred repl "![Z] : p(X, Y)");
  assert (test_not_admissible_pred pred repl "?[X] : (![Z] : p(X, Y))");
  assert (test_not_admissible_pred pred repl "?[Z] : (![X] : p(X, Y))");
  assert (test_not_admissible_pred pred repl "! [Y]: ( ?[Z] : (![X] : p(X, Y)) )");

  let pred = formula_of_string "p(X, Y)" in
  let repl = formula_of_string "a(X) & (b(Y) & c(Z))" in
  assert (check_sub pred repl "![A] : p(A, Y)" "![A] : ( a(A) & (b(Y) & c(Z)) )");
  assert (check_sub pred repl "![A] : p(X, Y)" "![A] : ( a(X) & (b(Y) & c(Z)) )");
  assert (check_sub pred repl "![A] : p(Z, Y)" "![A] : ( a(Z) & (b(Y) & c(Z)) )");

  (* Replacement with universal quantifier *)
  let pred = formula_of_string "p(X)" in
  let repl = formula_of_string "![Y] : q(X, Y)" in
  assert (check_sub pred repl "p(a)" "![Y] : q(a, Y)");
  assert (check_sub pred repl "p(b) & p(c)" "(![Y] : q(b, Y)) & (![Y] : q(c, Y))");
  assert (check_sub pred repl "p(Z)" "![Y] : q(Z, Y)");

  (* Replacement with existential quantifier *)
  let pred = formula_of_string "p(X)" in
  let repl = formula_of_string "?[Y] : r(X, Y)" in
  assert (check_sub pred repl "p(a)" "?[Y] : r(a, Y)");
  assert (check_sub pred repl "p(b) | p(c)" "(?[Y] : r(b, Y)) | (?[Y] : r(c, Y))");

  (* Replacement with nested quantifiers *)
  let pred = formula_of_string "p(X)" in
  let repl = formula_of_string "![Y] : (?[Z] : q(X, Y, Z))" in
  assert (check_sub pred repl "p(a)" "![Y] : (?[Z] : q(a, Y, Z))");
  assert (check_sub pred repl "p(W)" "![Y] : (?[Z] : q(W, Y, Z))");

  (* Replacement with quantifier over pattern variable *)
  let pred = formula_of_string "p(X)" in
  let repl = formula_of_string "![X] : q(X)" in
  assert (check_sub pred repl "p(a)" "![X] : q(X)");
  assert (check_sub pred repl "p(b)" "![X] : q(X)");

  (* Replacement with quantifier and free variable *)
  let pred = formula_of_string "p(X)" in
  let repl = formula_of_string "![Y] : r(X, Y, Z)" in
  assert (check_sub pred repl "p(a)" "![Y] : r(a, Y, Z)");
  assert (test_not_admissible_pred pred repl "![Z] : p(b)");

  (* Multiple pattern variables with quantifier in replacement *)
  let pred = formula_of_string "p(X, Y)" in
  let repl = formula_of_string "![Z] : q(X, Y, Z)" in
  assert (check_sub pred repl "p(a, b)" "![Z] : q(a, b, Z)");
  assert (check_sub pred repl "p(f(c), g(d))" "![Z] : q(f(c), g(d), Z)");

  (* Replacement with quantifier that binds a free variable *)
  let pred = formula_of_string "p(A)" in
  let repl = formula_of_string "![B] : (q(A) & r(B))" in
  assert (check_sub pred repl "p(x)" "![B] : (q(x) & r(B))");
  assert (check_sub pred repl "?[X]: p(X)" "?[X]: ( ![B] : (q(X) & r(B)) )");
  assert (test_not_admissible_pred pred repl "![B] : p(B)");

  (* Replacement with existential over pattern variable *)
  let pred = formula_of_string "p(X)" in
  let repl = formula_of_string "?[X] : q(X, X)" in
  assert (check_sub pred repl "p(a)" "?[X] : q(X, X)");
  assert (check_sub pred repl "p(Y)" "?[X] : q(X, X)");

  (* Complex: replacement with multiple quantifiers and pattern variables *)
  let pred = formula_of_string "p(X, Y)" in
  let repl = formula_of_string "![A] : (?[B] : q(X, Y, A, B))" in
  assert (check_sub pred repl "p(m, n)" "![A] : (?[B] : q(m, n, A, B))");
  assert (check_sub pred repl "![C] : p(m, n)" "![C] : (![A] : (?[B] : q(m, n, A, B)))");

  assert (Ref [1] >> Ref [0]);
  assert (Ref [1; 1] >> Ref [0]);
  assert (Ref [1; 0] >> Ref [0]);

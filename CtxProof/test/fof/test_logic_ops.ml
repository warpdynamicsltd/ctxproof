open Ctxproof
open Kernel

let run() = 
  assert (free_vars_term (Func ("P", [])) = StringSet.of_list []);
  assert (free_vars_term (Func ("P", [Var "X"])) = StringSet.of_list ["X"]);
  assert (free_vars_term (Func ("P", [Var "X"; Var "Y"])) = StringSet.of_list ["X"; "Y"]);
  assert (free_vars_term (Func ("P", [Var "X"; Var "X"])) = StringSet.of_list ["X"]);
  assert (free_vars_term (SkolemFunc ([Z.of_int 0], [])) = StringSet.of_list []);
  assert (free_vars_term (SkolemFunc ([Z.of_int 0], [Var "X"])) = StringSet.of_list ["X"]);
  assert (free_vars_term (SkolemFunc ([Z.of_int 0], [Var "X"; Var "Y"])) = StringSet.of_list ["X"; "Y"]);
  assert (free_vars_term (Var "X") = StringSet.of_list ["X"]);
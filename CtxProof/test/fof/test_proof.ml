open Ctxproof
open Kernel
open Fof_utils
open Tutils

let run () =
    let proof = statement_of_file "../data/hello" in
    assert (formula_of_proof proof (make_ref ["1"]) = formula_of_string "p(Y) | ~p(Y)");
    assert (formula_of_proof proof (make_ref []) = formula_of_string "$true");

    let proof = statement_of_file "../data/proof1" in
    assert (formula_of_proof proof (make_ref ["0"]) = formula_of_string "p");
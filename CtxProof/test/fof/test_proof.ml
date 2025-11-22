open Ctxproof
open Kernel
open Fof_utils
open Tutils

let run () =
    assert (List.length (statements_of_file "../data/hello") = 2);

    assert (List.length (statements_of_file "../data/proof1") = 1);

    let proof = proof_of_statements (statements_of_file "../data/hello") in
    match proof with Proof { statements; _ } -> assert (Array.length statements = 2);

    let proof = proof_of_statements (statements_of_file "../data/hello") in
    assert (formula_of_proof proof (make_ref ["1"]) = formula_of_string "p(Y) | ~p(Y)")
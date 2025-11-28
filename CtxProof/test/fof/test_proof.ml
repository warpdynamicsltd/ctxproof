open Ctxproof
open Kernel
open Proof
open Fof_utils
open Tutils

let not_valid proof = 
    try
        valid proof
    with
        | ProofError _ 
        | Failure _ -> true

let run () =
    let proof = statement_of_file "../data/hello" in
    assert (formula_of_proof proof (make_ref ["1"]) = formula_of_string "p(Y) | ~p(Y)");
    assert (formula_of_proof proof (make_ref []) = formula_of_string "$true");

    let proof = statement_of_file "../data/correct/proof1" in
    assert (formula_of_proof proof (make_ref ["0"]) = formula_of_string "p");

    let proof = statement_of_file "../data/incorrect/nproof7" in
    assert (var_accurs_free_in_assumptions proof (make_ref ["1"]) "X");

    assert (statement_of_file "../data/correct/proof1" |> valid);
    assert (statement_of_file "../data/correct/proof2" |> valid);
    assert (statement_of_file "../data/correct/proof3" |> valid);
    assert (statement_of_file "../data/correct/proof4" |> valid);
    assert (statement_of_file "../data/correct/proof5" |> valid);
    assert (statement_of_file "../data/correct/proof6" |> valid);
    assert (statement_of_file "../data/correct/proof7" |> valid);
    assert (statement_of_file "../data/correct/proof8" |> valid);

    assert (statement_of_file "../data/incorrect/nproof1" |> not_valid);
    assert (statement_of_file "../data/incorrect/nproof2" |> not_valid);
    assert (statement_of_file "../data/incorrect/nproof3" |> not_valid);
    assert (statement_of_file "../data/incorrect/nproof4" |> not_valid);
    assert (statement_of_file "../data/incorrect/nproof5" |> not_valid);
    assert (statement_of_file "../data/incorrect/nproof6" |> not_valid);
    assert (statement_of_file "../data/incorrect/nproof7" |> not_valid);
    assert (statement_of_file "../data/incorrect/nproof8" |> not_valid);
    assert (statement_of_file "../data/incorrect/nproof9" |> not_valid);
    assert (statement_of_file "../data/incorrect/nproof10" |> not_valid);
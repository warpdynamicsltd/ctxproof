open Ctxproof
open Kernel
open Proof
open Fof_utils
open Parser_utils
open Tutils
open Errors

let not_valid proof = 
    try
        valid proof
    with
        | ProofError _
        | ProofPosError _ 
        | Failure _ -> true

let read_error proof = 
    try
        let _ = valid proof in
        ("null", 0, 0)
    with
        | ProofError msg -> (msg, 0, 0)
        | ProofPosError (msg, pos) 
            -> let sl, sc = position pos in (msg, sl, sc + 1)

let expect file code line col =
  assert (statement_of_file file |> read_error = (kernel_error_message code, line, col))

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
    assert (statement_of_file "../data/correct/proof9" |> valid);
    assert (statement_of_file "../data/correct/proof10" |> valid);

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
    assert (statement_of_file "../data/incorrect/nproof11" |> not_valid);

    expect "../data/incorrect/nproof1"  InvalidReference        1  1;
    expect "../data/incorrect/nproof2"  InvalidReference        3  5;
    expect "../data/incorrect/nproof3"  InvalidReference        4  5;
    expect "../data/incorrect/nproof4"  InvalidReference       10 13;
    expect "../data/incorrect/nproof5"  InvalidReference        4  5;
    expect "../data/incorrect/nproof6"  InvalidReference        3  5;
    expect "../data/incorrect/nproof7"  RuleConstraintViolation 4  5;
    expect "../data/incorrect/nproof8"  RuleConstraintViolation 4  5;
    expect "../data/incorrect/nproof9"  RuleConstraintViolation 7  5;
    expect "../data/incorrect/nproof10" NotAllowedSkolemTerm    9  9;
    expect "../data/incorrect/nproof11" AxiomViolation          4  5;


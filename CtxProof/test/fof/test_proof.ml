open Ctxproof
open Fof_utils
open Tutils

let run () =
    assert (List.length (statements_of_string (read_file "../data/hello")) = 2);
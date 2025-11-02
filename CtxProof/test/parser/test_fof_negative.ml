open Ctxproof
open Fof_utils

let not_wff s = 
  try
    let _ = parse_string s in
    false
  with _ -> true

let run () =
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
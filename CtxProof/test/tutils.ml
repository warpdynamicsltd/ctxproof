open Ctxproof
open Types

let make_ref l = Ref (List.map Z.of_string l)

let assert_eq number f arg expected =
  let actual =
    try 
       f arg
    with 
      | Failure msg -> failwith (Printf.sprintf "Assertion failed at line %d with error: %s" number msg)
      | _ -> failwith (Printf.sprintf "Assertion failed at line %d with unknown error" number) 
    in
  if actual <> expected then
    failwith (Printf.sprintf "Assertion failed at line %d" number)
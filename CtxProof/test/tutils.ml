open Stdlib
open Ctxproof
open Fof_utils
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


let read_file (path : string) : string =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))


let statements_of_file path = statements_of_string (read_file path)



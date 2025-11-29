open Types
open Kernel
(*open Parser_utils*)


exception ProofPosError of string * Lexing.position
exception ProofError of string

exception RefPosError of string * Lexing.position


let append_int_to_ref ref i = 
  match ref with 
    | Ref lst -> Ref (lst @ [Z.of_int i])


let rec ref_system_is_correct statement root_ref = 
  match statement with 
    | Statement {ref; statements; _} when ref = root_ref
        ->
          List.for_all (Fun.id)
          (List.mapi 
            (
              fun i s -> (ref_system_is_correct s (append_int_to_ref root_ref i))
            ) 
          statements)
    | Statement {pos;_} -> raise (RefPosError ("invalid reference", pos))


let proved proof ref = 
  try
    ref_system_is_correct proof (Ref [])
    && prove_thesis proof ref;
  with 
    | RefPosError (msg, pos)
    | KernelPosError (msg, pos)
      -> raise (ProofPosError (msg, pos))
    | KernelError msg
      -> raise (ProofError msg)
    
    
let valid proof = proved proof (Ref [])
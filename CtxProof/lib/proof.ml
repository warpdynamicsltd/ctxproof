open Kernel
open Parser_utils

exception ProofError of string

let proved proof ref = 
  try
    prove_thesis proof ref
  with 
    | KernelPosError (msg, pos)
      -> raise (ProofError (msg ^ " " ^ (location_to_string pos)))
    | KernelError msg
      -> raise (ProofError msg)
    
    
let valid proof = proved proof (Ref [])
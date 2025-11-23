open Kernel
open Parser_utils

let proved proof ref = 
  try
    prove_thesis proof ref
  with KernelError msg ->
    let statement = get_statement proof ref in
    match statement with Statement {pos;_}
      -> failwith (msg ^ " " ^ (location_to_string pos))
    
    
let valid proof = proved proof (Ref [])
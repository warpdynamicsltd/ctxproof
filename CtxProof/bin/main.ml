open Ctxproof
open Proof
open Parser_utils
(*open Printer*)

let () =
      let lexbuf = Lexing.from_channel stdin in
            try
                  
                  let proof = Parser.input Lexer.token lexbuf in
                  let result = valid proof in
                  if result then
                        print_string("QED")
                  else raise (Failure "invalid proof");
                  print_newline();
                  
            with 
             | Parser.Error _ -> prerr_endline ("malformed expression " ^ Parser_utils.location_to_string lexbuf.Lexing.lex_start_p)
             | Lexer.Error _ -> prerr_endline ("illegal character " ^ Parser_utils.location_to_string lexbuf.Lexing.lex_start_p)
             | Errors.CxError m -> prerr_endline m;
             | ProofError m -> prerr_endline m;
             | ProofPosError (msg, pos) -> prerr_endline (msg ^ " " ^ (location_to_string pos))
             | Failure m -> prerr_endline m;
             
      
            
      
          
(*let () = print_string (string_of_statement (Statement {name="a"; formula_role="a"; formula = True; annotation = "a"}) )*)

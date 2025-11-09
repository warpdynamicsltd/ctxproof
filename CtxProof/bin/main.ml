open Ctxproof
(*open Printer*)

let () =
      let lexbuf = Lexing.from_channel stdin in
            try
                  
                  let _ = Parser.input Lexer.token lexbuf
                  in
                  print_string("parsed OK");
                  print_newline();
                  
            with 
             | Parser.Error _ -> prerr_endline ("malformed expression " ^ Parser_utils.location_to_string lexbuf.Lexing.lex_start_p)
             | Lexer.Error _ -> prerr_endline ("illegal character " ^ Parser_utils.location_to_string lexbuf.Lexing.lex_start_p)
             | Errors.CxError m -> prerr_endline m;
             
      
            
      
          
(*let () = print_string (string_of_statement (Statement {name="a"; formula_role="a"; formula = True; annotation = "a"}) )*)

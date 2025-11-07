open Ctxproof
(*open Printer*)

let () =
      try
            let lexbuf = Lexing.from_channel stdin in
            let _ = Parser.input Lexer.token lexbuf
            in
            print_string("OK");
            print_newline();
            
      with e -> print_string (Printexc.to_string e); print_newline();
      
            
      
          
(*let () = print_string (string_of_statement (Statement {name="a"; formula_role="a"; formula = True; annotation = "a"}) )*)

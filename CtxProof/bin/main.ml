open Ctxproof
open Printer

let () =
      let lexbuf = Lexing.from_channel stdin in
      let result = Parser.input Lexer.token lexbuf in
      (string_of_formula result) |> print_string;
      
          
(*let () = print_string (string_of_statement (Statement {name="a"; formula_role="a"; formula = True; annotation = "a"}) )*)

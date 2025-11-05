open Ctxproof
open Types
open Fof_utils
open Tutils

let parse_ref s =
    let lexbuf =  Lexing.from_string s in
    Parser.ref Lexer.token lexbuf

let make_ref l = Ref (List.map Z.of_string l)


let run () =
  assert_eq __LINE__ parse_ref "0" (make_ref ["0"]);
  assert_eq __LINE__ parse_ref "01" (make_ref ["1"]);
  assert_eq __LINE__ parse_ref "0.0" (make_ref ["0";"0"]); 
  assert_eq __LINE__ parse_ref "0.0.1" (make_ref ["0"; "0"; "1"]);
  assert_eq __LINE__ parse_ref "001.0.121" (make_ref ["1"; "0"; "121"]);
  assert_eq __LINE__ parse_ref "2162517651761523712341723416735127351.0.121" (make_ref ["2162517651761523712341723416735127351"; "0"; "121"]);

  assert_eq __LINE__ statement_of_string "0 $true {MOD} {} {}" 
    (Statement {ref=make_ref ["0"]; formula=True; mode="MOD"; formulas=[]; terms=[]});
  assert_eq __LINE__ statement_of_string "0 $true {MOD} {$true} {}" 
    (Statement {ref=make_ref ["0"]; formula=True; mode="MOD"; formulas=[True]; terms=[]});
  assert_eq __LINE__ statement_of_string "0 $true {MOD} {$true; $false} {}" 
    (Statement {ref=make_ref ["0"]; formula=True; mode="MOD"; formulas=[True; False]; terms=[]});
  assert_eq __LINE__ statement_of_string "0 $true {MOD} {} {X}" 
    (Statement {ref=make_ref ["0"]; formula=True; mode="MOD"; formulas=[]; terms=[Var "X"]});
  assert_eq __LINE__ statement_of_string "0 $true {MOD} {} {f(c)}" 
    (Statement {ref=make_ref ["0"]; formula=True; mode="MOD"; formulas=[]; terms=[Func ("f", [Const "c"])]});


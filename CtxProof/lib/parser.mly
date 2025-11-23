%{
  open Types
  open Errors

  let cx_error msg pos =
    (CxError (msg ^ " " ^ Parser_utils.location_to_string pos))

%}

%token <string> UWORD
%token <string> LWORD
%token <Z.t> INT
%token COMMA DOT COLON LPAREN RPAREN SEMICOLON SK
%token LCURL RCURL
%token AND OR NOT IMPLIES IFF
%token FORALL EXISTS
%token TRUE FALSE
%token EQUAL
%token LBRACK RBRACK
%token EOF

%start input
%start wff
%start wft
%start ref
%start statement
%start statements
%type <Types.statement>      input
%type <Types.first_order_formula> wff
%type <Types.term>                wft
%type <Types.reference>           ref
%type <Types.statement>           statement
%type <Types.generalized_formula> generalized_formula
%type <Types.statement list>      statements
%%

input:
  statement EOF {$1}

wff:
  fof_formula EOF {$1}

wft:
  term EOF {$1}

ref:
  reference EOF {$1}

statements:
| statement { [$1] }
| statement statements { $1 :: $2 }

statement:
| ref=reference formula=fof_formula mode=mode_arg gformulas=formulas_arg terms=terms_arg SEMICOLON
        { Statement {ref; formula; statements=[]; inference=Inference{ mode; gformulas; terms}; pos=($startpos) } }
| ref=reference formula=fof_formula LCURL statements=statements RCURL
        { Statement {ref; formula; statements; inference=Inference{ mode=Context; gformulas=[]; terms=[]}; pos=($startpos) } }

(*| error { raise (cx_error "expected statement" $startpos) }*)

reference:
  integers { Ref $1 }

integers:
| DOT {[]}
| INT { [$1] }
| INT DOT integers { $1 :: $3 }

mode_arg:
| LCURL mode_type=UWORD COLON mode_value=UWORD RCURL 
 
  { 
    match  mode_type with
      | "A" -> Axiom(mode_value)
      | "R" -> Rule(mode_value)
      | _ -> raise (cx_error "expected mode type A or R" $startpos)

  }
| LCURL mode_type=UWORD RCURL
  { 
    match  mode_type with
      | "ASM" -> Assumption
      | _ -> raise (cx_error "expected assumption" $startpos)

  }



terms_arg:
  LCURL RCURL { [] }
| LCURL terms RCURL { $2 }
(*| error { raise (cx_error "expected terms arg" $startpos) }*)

formulas_arg:
  LCURL RCURL { [] }
| LCURL formulas RCURL { $2 }

formulas:
  generalized_formula { [$1] }
| generalized_formula SEMICOLON formulas { $1 :: $3 }

generalized_formula:
  reference { Reference $1 }
| fof_formula { Formula $1 }

fof_formula:
| primary AND primary               { And($1, $3) }
| primary OR primary                { Or($1, $3) }
| primary IMPLIES primary           { Implies($1, $3) }
| primary IFF primary               { Iff($1, $3) }
| FORALL LBRACK vars RBRACK COLON primary
    { List.fold_right (fun v acc -> Forall(v, acc)) $3 $6 }
| EXISTS LBRACK vars RBRACK COLON primary
    { List.fold_right (fun v acc -> Exists(v, acc)) $3 $6 }
| primary                           { $1 }
(*| error { raise (cx_error "expected formula" $startpos) }*)

primary:
| atom { $1 }
| NOT primary                       { Not $2 }
| LPAREN fof_formula RPAREN { $2 }

atomic_name:
  LWORD                        { $1 }

predicate:
  atomic_name LPAREN terms RPAREN { Pred($1, $3) }
| atomic_name                      { Pred($1, []) }

atom:
  TRUE                                     { True }
| FALSE                                    { False }
| predicate                                { $1 }
| term EQUAL term                          { Pred("=", [$1; $3]) }

vars:
  var COMMA vars             { $1 :: $3 }
| var                        { [$1] }

var:
  UWORD                        { $1 }

const:
  atomic_name                 { $1 }

terms:
  term COMMA terms             { $1 :: $3 }
| term                         { [$1] }
(*| error { raise (cx_error "expected terms" $startpos) }*)

term:
  atomic_name LPAREN terms RPAREN     { Func($1, $3) }
| SK integers LPAREN terms RPAREN     { SkolemFunc($2, $4) }
| var                                 { Var $1 }
| const                               { Const $1 }
| SK integers                         { SkolemConst $2 }
(*| error { raise (cx_error "expected term" $startpos) }*)

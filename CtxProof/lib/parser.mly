%{
  open Types
  exception CxError of string

  let cx_error msg pos =
    (CxError ("syntax error: " ^msg ^ " at " ^ Parser_utils.location_to_string pos))
%}

%token <string> UWORD
%token <string> LWORD
%token <int> INT
%token <string> STRING
%token FOF DOT COMMA COLON LPAREN RPAREN
%token AND OR NOT IMPLIES IFF NOR NAND
%token FORALL EXISTS
%token TRUE FALSE
%token EQUAL NEQ
%token LBRACK RBRACK
%token QUOTE
%token EOF

%start input
/*%type <Types.fof_statement list> input*/
%type <Types.first_order_formula>  input
%%

input:
  fof_formula EOF {$1}
/*  statements EOF { $1 }
| error { raise (cx_error "expected proof" $startpos) }*/

statements:
  statement statements { $1 :: $2 }
| /* empty */         { [] }
| error { raise (cx_error "expected statement list" $startpos) }

statement:
  FOF LPAREN LWORD COMMA LWORD COMMA fof_formula annotation_opt RPAREN DOT
    { Statement {name = $3; formula_role = $5; formula = $7; annotation = $8} }

  | error { raise (cx_error "expected statement" $startpos) }

annotation_opt:
  COMMA STRING { $2 }
|        { "" }
| error { raise (cx_error "expected annotation" $startpos)}

fof_formula:
  TRUE                         { True }
| FALSE                        { False }
| NOT fof_formula              { Not $2 }
| fof_formula AND fof_formula  { And($1, $3) }
| fof_formula OR fof_formula   { Or($1, $3) }
| fof_formula IMPLIES fof_formula { Implies($1, $3) }
| fof_formula IFF fof_formula     { Iff($1, $3) }
| FORALL LBRACK vars RBRACK COLON fof_formula
    { List.fold_right (fun v acc -> Forall(v, acc)) $3 $6 }
| EXISTS LBRACK vars RBRACK COLON fof_formula
    { List.fold_right (fun v acc -> Exists(v, acc)) $3 $6 }
| atom                         { $1 }
| LPAREN fof_formula RPAREN    { $2 }
| error { raise (cx_error "expected formula" $startpos) }

pred_symbol:
  LWORD                        { $1 }
| error { raise (cx_error "expected predicate symbol" $startpos) }

func_symbol:
  LWORD                        { $1 }
| error { raise (cx_error "expected function symbol" $startpos) }

atom:
  pred_symbol LPAREN terms RPAREN          { Pred($1, $3) }
| pred_symbol                              { Pred($1, []) }
| term EQUAL term                          { Pred("=", [$1; $3]) }
| term NEQ term                            { Not(Pred("=", [$1; $3])) }
| error { raise (cx_error "expected atom formula" $startpos) }

vars:
  var COMMA vars             { $1 :: $3 }
| var                        { [$1] }
| error { raise (cx_error "expected var list" $startpos) }

var:
  UWORD                        { $1 }
| error { raise (cx_error "expected variable" $startpos) }

const:
  LWORD                        { $1 }
| error { raise (cx_error "expected constant" $startpos) }

terms:
  term COMMA terms             { $1 :: $3 }
| term                         { [$1] }
| error { raise (cx_error "expected term list" $startpos) }

term:
  func_symbol LPAREN terms RPAREN     { Func($1, $3) }
| var                                 { Var $1 }
| const                               { Const $1 }
| error { raise (cx_error "expected term" $startpos) }

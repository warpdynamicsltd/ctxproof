%{
  open Types
  exception CxError of string

  let cx_error msg pos =
    (CxError ("syntax error: " ^msg ^ " at " ^ Parser_utils.location_to_string pos))
%}

%token <string> UWORD
%token <string> LWORD
%token <Z.t> INT
%token COMMA DOT COLON LPAREN RPAREN SEMICOLON
%token LCURL RCURL
%token AND OR NOT IMPLIES IFF
%token FORALL EXISTS
%token TRUE FALSE
%token EQUAL
%token LBRACK RBRACK
%token EOF

%start input
%start wff
%start ref
%start line
%type <Types.first_order_formula>  input
%type <Types.first_order_formula>  wff
%type <Types.reference>           ref
%type <Types.statement>           line
%%

input:
  fof_formula EOF {$1}

wff:
  fof_formula EOF {$1}

ref:
  reference EOF {$1}

line:
  ref=reference formula=fof_formula LCURL mode=UWORD RCURL formulas=formulas_arg terms=terms_arg    { Statement {ref; formula; mode; formulas; terms} }

reference:
  integers { Ref $1 }

integers:
  INT { [$1] }
| INT DOT integers { $1 :: $3 }

terms_arg:
  LCURL RCURL { [] }
| LCURL terms RCURL { $2 }

formulas_arg:
  LCURL RCURL { [] }
| LCURL formulas RCURL { $2 }

formulas:
  fof_formula { [$1] }
| fof_formula SEMICOLON formulas { $1 :: $3 }

fof_formula:
| NOT primary                       { Not $2 }
| primary AND primary               { And($1, $3) }
| primary OR primary                { Or($1, $3) }
| primary IMPLIES primary           { Implies($1, $3) }
| primary IFF primary               { Iff($1, $3) }
| FORALL LBRACK vars RBRACK COLON primary
    { List.fold_right (fun v acc -> Forall(v, acc)) $3 $6 }
| EXISTS LBRACK vars RBRACK COLON primary
    { List.fold_right (fun v acc -> Exists(v, acc)) $3 $6 }
| primary                           { $1 }
| error { raise (cx_error "expected formula" $startpos) }

primary:
| atom { $1 }
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

term:
  atomic_name LPAREN terms RPAREN     { Func($1, $3) }
| var                                 { Var $1 }
| const                               { Const $1 }

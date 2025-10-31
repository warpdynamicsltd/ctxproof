%{
open Types
%}

%token <string> IDENT
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
%type <Types.fof_statement list> input
%%

input:
  statements EOF { $1 }

statements:
  statement statements { $1 :: $2 }
| /* empty */         { [] }

statement:
  FOF LPAREN IDENT COMMA IDENT COMMA fof_formula COMMA annotation_opt RPAREN DOT
    { Statement {name = $3; formula_role = $5; formula = $7; annotation = $9} }

annotation_opt:
  STRING { $1 }
|        { "" }

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

atom:
  IDENT LPAREN terms RPAREN    { Pred($1, $3) }
| IDENT                        { Pred($1, []) }
| term EQUAL term              { Pred("=", [$1; $3]) }
| term NEQ term                { Not(Pred("=", [$1; $3])) }

vars:
  IDENT COMMA vars             { $1 :: $3 }
| IDENT                        { [$1] }

terms:
  term COMMA terms             { $1 :: $3 }
| term                         { [$1] }

term:
  IDENT LPAREN terms RPAREN    { Func($1, $3) }
| IDENT
    { if String.length $1 > 0 && Char.uppercase_ascii $1.[0] = $1.[0]
      then Var $1 else Const $1 }
| INT                          { Const (string_of_int $1) }

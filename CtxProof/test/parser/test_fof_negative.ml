open Ctxproof
open Fof_utils

let not_wff s = 
  try
    let _ = formula_of_string s in
    false
  with _ -> true

let run () =
  assert (not_wff "$truth");
  assert (not_wff "$falsity");
  assert (not_wff "X");
  assert (not_wff "p(");
  assert (not_wff "p)");
  assert (not_wff "p(X");
  assert (not_wff "P & P");
  assert (not_wff "p & p & p");
  assert (not_wff "p & p | p");
  assert (not_wff "p => p | p");

  (* 100 additional negative tests *)
  assert (not_wff "");
  assert (not_wff "   ");
  assert (not_wff "(");
  assert (not_wff ")");
  assert (not_wff "()");

  (* Unbalanced parentheses *)
  assert (not_wff "(p");
  assert (not_wff "p)");
  assert (not_wff "(p & q");
  assert (not_wff "p & q)");
  assert (not_wff "((p)");
  assert (not_wff "p))");
  assert (not_wff "(p | (q & r)");
  assert (not_wff "((p | q) & r");

  (* Dangling operators *)
  assert (not_wff "&");
  assert (not_wff "|");
  assert (not_wff "=>");
  assert (not_wff "<=>");
  assert (not_wff "~");
  assert (not_wff "p &");
  assert (not_wff "& p");
  assert (not_wff "p |");
  assert (not_wff "| p");
  assert (not_wff "p =>");
  assert (not_wff "=> p");
  assert (not_wff "p <=>");
  assert (not_wff "<=> p");
  assert (not_wff "~");

  (* Double/tandem unary *)
  assert (not_wff "~~");
  assert (not_wff "~& p");
  assert (not_wff "~| p");
  assert (not_wff "~=> p");
  assert (not_wff "~~ => p");

  (* Bad quantifier headers *)
  assert (not_wff "! X: p(X)");
  assert (not_wff "? X: p(X)");
  assert (not_wff "! []: p(X)");
  assert (not_wff "? []: p(X)");
  assert (not_wff "! [: p(X)");
  assert (not_wff "? ]: p(X)");
  assert (not_wff "! [ ]: p(X)");
  assert (not_wff "! [X Y]: p(X)");
  assert (not_wff "? [X,]: p(X)");
  assert (not_wff "! [,X]: p(X)");
  assert (not_wff "! [x]: p(x)");      (* lowercase var not allowed if your lexer enforces UWORD *)
  assert (not_wff "! [X]:");
  assert (not_wff "! : p(X)");
  assert (not_wff "! [X] p(X)");       (* missing colon *)
  assert (not_wff "? [X] p(X)");       (* missing colon *)
  assert (not_wff "! [X]:");
  assert (not_wff "? [X]:");
  assert (not_wff "! [X]: ~");
  assert (not_wff "? [X]: & p(X)");
  (* Malformed terms/predicates *)
  assert (not_wff "p(");
  assert (not_wff "p)");
  assert (not_wff "p(,)");
  assert (not_wff "p(,X)");
  assert (not_wff "p(X,)");
  assert (not_wff "p(X,,Y)");
  assert (not_wff "p(,X,Y)");
  assert (not_wff "p(X,Y,)");
  assert (not_wff "p(()");
  assert (not_wff "p())");
  assert (not_wff "p(()))");
  assert (not_wff "p((X)");
  assert (not_wff "p(X))");
  assert (not_wff "p((X,)Y)");
  assert (not_wff "p(X Y)");
  assert (not_wff "p(X;Y)");
  assert (not_wff "p[X]");
  assert (not_wff "p{X}");
  assert (not_wff "p<X>");

  (* Equality malformed *)
  assert (not_wff "=");
  assert (not_wff "=");
  assert (not_wff "= X");
  assert (not_wff "X =");
  
  assert (not_wff "(=)");
  assert (not_wff "(X=)");
  assert (not_wff "(=Y)");
  assert (not_wff "=(X,Y)");
  assert (not_wff "X == Y");
  assert (not_wff "X = = Y");
  assert (not_wff "X = ( )");

  (* Mixed operator precedence mistakes *)
  assert (not_wff "(p & ) q");
  assert (not_wff "p ( & q )");
  assert (not_wff "( p | )");
  assert (not_wff "( ~ )");
  assert (not_wff "~()");
  assert (not_wff "~( )");
  assert (not_wff "~(p & )");
  assert (not_wff "~( & p)");
  assert (not_wff "(p <=> )");
  assert (not_wff "( => p)");

  (* Nested term errors *)
  assert (not_wff "p(f())");           (* empty arg list to function if not allowed *)
  assert (not_wff "p(f(,X))");
  assert (not_wff "p(f(X,))");
  assert (not_wff "p(f(,))");
  assert (not_wff "p(f(,g(X)))");
  assert (not_wff "p(f(g(),X))");      (* empty g() if functions require args *)
  assert (not_wff "p(,)");
  assert (not_wff "p((,))");

  (* Variable placement mistakes *)
  assert (not_wff "q(1)");             (* numeric constant if banned *)
  assert (not_wff "q('a)");            (* quoted atom if not in spec *)
  assert (not_wff "q(\"a\")");         (* strings not allowed *)
  assert (not_wff "p(X Y Z)");         (* missing commas *)
  assert (not_wff "p(X,,Z)");

  (* Chained binaries without parentheses where grammar forbids *)
  assert (not_wff "p => q => r");
  assert (not_wff "p <=> q <=> r");
  assert (not_wff "p & q |");
  assert (not_wff "| p & q");

  (* Quantifier variable lists malformed *)
  assert (not_wff "! [X,,Y]: p(X,Y)");
  assert (not_wff "! [,]: p()");
  assert (not_wff "? [,,]: p()");
  assert (not_wff "! [X,,]: p(X)");
  assert (not_wff "? [,X,]: p(X)");
  assert (not_wff "! [X,Y,]: p(X,Y)");
  assert (not_wff "? [,X,Y]: p(X,Y)");

  (* Bad tokens around quantifiers *)
  assert (not_wff "! [X]:: p(X)");
  assert (not_wff "? [X]:: p(X)");
  assert (not_wff "! [[X]]: p(X)");
  assert (not_wff "? [[X]]: p(X)");

  (* Misplaced quantifiers in expressions *)
  assert (not_wff "p(X) ! [Y]: q(Y)");
  assert (not_wff "p(X) ? [Y]: q(Y)");
  assert (not_wff "( ! [X]: p(X) ) q(Y)");
  assert (not_wff "( ? [X]: p(X) ) q(Y)");

  (* Operators where terms are expected *)
  assert (not_wff "p(&)");
  assert (not_wff "p(|)");
  assert (not_wff "p(=>)");
  assert (not_wff "p(<=>)");
  assert (not_wff "p(~)");
  assert (not_wff "p(p=>q)");

  (* Trailing commas and separators *)
  assert (not_wff "p(,)");
  assert (not_wff "p(X,,)");
  assert (not_wff "p(,X,)");
  assert (not_wff "p(, ,)");
  assert (not_wff "p(X, ,Y)");

  (* Parentheses misuse with binaries *)
  assert (not_wff "(p) (q)");
  assert (not_wff "(p) q");
  assert (not_wff "(p) &");
  assert (not_wff "& (p)");
  assert (not_wff "(p) |");
  assert (not_wff "| (p)");

  (* Equality with non-terms *)
  assert (not_wff "p() = q()");     

  (* Mixed stray punctuation *)
  assert (not_wff "p, q");
  assert (not_wff "p; q");
  assert (not_wff "p: q");
  assert (not_wff "p@q");
  assert (not_wff "p#q");
  assert (not_wff "p$q");

  (* Bad nesting with quantifiers and parens *)
  assert (not_wff "! [X: p(X))");
  assert (not_wff "! [X): p(X)");
  assert (not_wff "? [X: (p(X))");
  assert (not_wff "? [X): (p(X))");
  assert (not_wff "! X]: p(X)");
  assert (not_wff "? X]: p(X)");
  assert (not_wff "! [X Y Z]: p(X,Y,Z)");
  assert (not_wff "? [X Y Z]: p(X,Y,Z)");
  assert (not_wff "! [X]: p(X) & p(Y) & p(Z)");
  assert (not_wff "? [X,Y]: p(X,Y) & q(Y,X)");
  assert (not_wff "? [X,Y]: p(X) & q(Y)");

  (* Misordered equality *)
  assert (not_wff "=X");
  assert (not_wff "X=");
  assert (not_wff "(= X Y)");
  assert (not_wff "=(X)=Y");

  (* Random tptp-like but invalid in your subset *)
  assert (not_wff "$true &");
  assert (not_wff "$false |");

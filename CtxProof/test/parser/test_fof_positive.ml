open Ctxproof
open Fof_utils
open Tutils

let run () =
  assert_eq __LINE__ parse_string "$true" True;
  assert_eq __LINE__ parse_string "$false" False;
  assert_eq __LINE__ parse_string "p" (Pred ("p", []));
  assert_eq __LINE__ parse_string "(p)" (Pred ("p", []));
  assert_eq __LINE__ parse_string "p(X)" (Pred ("p", [Var "X"]));
  assert_eq __LINE__ parse_string "p(X,Y)" (Pred ("p", [Var "X"; Var "Y"]));
  assert_eq __LINE__ parse_string "p(c)" (Pred ("p", [Const "c"]));
  assert_eq __LINE__ parse_string "p(f(X))" (Pred ("p", [Func ("f", [Var "X"])]));
  assert_eq __LINE__ parse_string "p(f(c))" (Pred ("p", [Func ("f", [Const "c"])]));
  assert_eq __LINE__ parse_string "p(f(X),g(X))" (Pred ("p", [Func ("f", [Var "X"]); Func ("g", [Var "X"])]));
  assert_eq __LINE__ parse_string "q" (Pred ("q", []));
  assert_eq __LINE__ parse_string "q(a)" (Pred ("q", [Const "a"]));
  assert_eq __LINE__ parse_string "q(X,Y,Z)" (Pred ("q", [Var "X"; Var "Y"; Var "Z"]));
  assert_eq __LINE__ parse_string "X=Y" (Pred ("=", [Var "X"; Var "Y"]));
  assert_eq __LINE__ parse_string "f(X)=g(Y)" (Pred ("=", [Func ("f", [Var "X"]); Func ("g", [Var "Y"])]));
  assert_eq __LINE__ parse_string "a=b" (Pred ("=", [Const "a"; Const "b"]));
  assert_eq __LINE__ parse_string "(p) & (q)" (And (Pred ("p", []), Pred ("q", [])));
  assert_eq __LINE__ parse_string "p & q" (And (Pred ("p", []), Pred ("q", [])));
  assert_eq __LINE__ parse_string "p | q" (Or (Pred ("p", []), Pred ("q", [])));
  assert_eq __LINE__ parse_string "p => q" (Implies (Pred ("p", []), Pred ("q", [])));
  assert_eq __LINE__ parse_string "p <=> q" (Iff (Pred ("p", []), Pred ("q", [])));
  assert_eq __LINE__ parse_string "~p" (Not (Pred ("p", [])));
  assert_eq __LINE__ parse_string "~(p)" (Not (Pred ("p", [])));
  assert_eq __LINE__ parse_string "p(X) & q(Y)" (And (Pred ("p", [Var "X"]), Pred ("q", [Var "Y"])));
  assert_eq __LINE__ parse_string "p(X) | q(Y)" (Or (Pred ("p", [Var "X"]), Pred ("q", [Var "Y"])));
  assert_eq __LINE__ parse_string "p(X) => q(X)" (Implies (Pred ("p", [Var "X"]), Pred ("q", [Var "X"])));
  assert_eq __LINE__ parse_string "p(X) <=> q(X)" (Iff (Pred ("p", [Var "X"]), Pred ("q", [Var "X"])));
  assert_eq __LINE__ parse_string "~p(X)" (Not (Pred ("p", [Var "X"])));
  assert_eq __LINE__ parse_string "~(p(X) & q(X))" (Not (And (Pred ("p", [Var "X"]), Pred ("q", [Var "X"]))));
  assert_eq __LINE__ parse_string "! [X]: p(X)" (Forall ("X", Pred ("p", [Var "X"])));
  assert_eq __LINE__ parse_string "? [X]: p(X)" (Exists ("X", Pred ("p", [Var "X"])));
  assert_eq __LINE__ parse_string "! [X]: ~p(X)" (Forall ("X", Not (Pred ("p", [Var "X"]))));
  assert_eq __LINE__ parse_string "? [X]: ~p(X)" (Exists ("X", Not( Pred ("p", [Var "X"]))));
  assert_eq __LINE__ parse_string "! [X,Y]: p(X,Y)" (Forall ("X", Forall ("Y", Pred ("p", [Var "X"; Var "Y"]))));
  assert_eq __LINE__ parse_string "? [X,Y]: p(X,Y)" (Exists ("X", Exists ("Y", Pred ("p", [Var "X"; Var "Y"]))));
  assert_eq __LINE__ parse_string "! [X]: (p(X) & q(X))" (Forall ("X", And (Pred ("p", [Var "X"]), Pred ("q", [Var "X"]))));
  assert_eq __LINE__ parse_string "? [X]: (p(X) | q(X))" (Exists ("X", Or (Pred ("p", [Var "X"]), Pred ("q", [Var "X"]))));
  assert_eq __LINE__ parse_string "! [X]: p(f(X))" (Forall ("X", Pred ("p", [Func ("f", [Var "X"])])));
  assert_eq __LINE__ parse_string "? [X]: p(g(X))" (Exists ("X", Pred ("p", [Func ("g", [Var "X"])])));
  assert_eq __LINE__ parse_string "p(f(g(X)))" (Pred ("p", [Func ("f", [Func ("g", [Var "X"])])]));
  assert_eq __LINE__ parse_string "p(f(g(a)))" (Pred ("p", [Func ("f", [Func ("g", [Const "a"])])]));
  assert_eq __LINE__ parse_string "p(h(X,Y))" (Pred ("p", [Func ("h", [Var "X"; Var "Y"])]));
  assert_eq __LINE__ parse_string "p(h(a,b))" (Pred ("p", [Func ("h", [Const "a"; Const "b"])]));
  assert_eq __LINE__ parse_string "(r(X) & s(X)) | t(X)" (Or (And (Pred ("r", [Var "X"]), Pred ("s", [Var "X"])), Pred ("t", [Var "X"])));
  assert_eq __LINE__ parse_string "r(X) & (s(X) | t(X))" (And (Pred ("r", [Var "X"]), Or (Pred ("s", [Var "X"]), Pred ("t", [Var "X"]))));
  assert_eq __LINE__ parse_string "(r(X))" (Pred ("r", [Var "X"]));
  assert_eq __LINE__ parse_string "p(f(X), g(Y,Z))" (Pred ("p", [Func ("f", [Var "X"]); Func ("g", [Var "Y"; Var "Z"])]));
  assert_eq __LINE__ parse_string "p(a, f(b,c))" (Pred ("p", [Const "a"; Func ("f", [Const "b"; Const "c"])]));
  assert_eq __LINE__ parse_string "p(X, f(Y, g(Z)))" (Pred ("p", [Var "X"; Func ("f", [Var "Y"; Func ("g", [Var "Z"])])]));
  assert_eq __LINE__ parse_string "~(p | q)" (Not (Or (Pred ("p", []), Pred ("q", []))));
  assert_eq __LINE__ parse_string "~(p & q)" (Not (And (Pred ("p", []), Pred ("q", []))));
  assert_eq __LINE__ parse_string "~(p => q)" (Not (Implies (Pred ("p", []), Pred ("q", []))));
  assert_eq __LINE__ parse_string "~(p <=> q)" (Not (Iff (Pred ("p", []), Pred ("q", []))));
  assert_eq __LINE__ parse_string "! [X]: (~(p(X)))" (Forall ("X", Not (Pred ("p", [Var "X"]))));
  assert_eq __LINE__ parse_string "! [X]: ( ~p(X) )" (Forall ("X", Not (Pred ("p", [Var "X"]))));
  assert_eq __LINE__ parse_string "? [X]: (~(p(X)))" (Exists ("X", Not (Pred ("p", [Var "X"]))));
  assert_eq __LINE__ parse_string "? [X]: ( ~p(X) )" (Exists ("X", Not (Pred ("p", [Var "X"]))));
  assert_eq __LINE__ parse_string "! [X]: ( p(X) => q(X) )" (Forall ("X", Implies (Pred ("p", [Var "X"]), Pred ("q", [Var "X"]))));
  assert_eq __LINE__ parse_string "? [X]: ( p(X) <=> q(X) )" (Exists ("X", Iff (Pred ("p", [Var "X"]), Pred ("q", [Var "X"]))));
  assert_eq __LINE__ parse_string "(p(X) & q(Y)) & r(Z)" (And (And (Pred ("p", [Var "X"]), Pred ("q", [Var "Y"])), Pred ("r", [Var "Z"])));
  assert_eq __LINE__ parse_string "(p(X) | q(Y)) | r(Z)" (Or (Or (Pred ("p", [Var "X"]), Pred ("q", [Var "Y"])), Pred ("r", [Var "Z"])));
  assert_eq __LINE__ parse_string "(p(X) & q(Y)) <=> r(Z)" (Iff (And (Pred ("p", [Var "X"]), Pred ("q", [Var "Y"])), Pred ("r", [Var "Z"])));
  assert_eq __LINE__ parse_string "p(X) <=> (q(Y) | r(Z))" (Iff (Pred ("p", [Var "X"]), Or (Pred ("q", [Var "Y"]), Pred ("r", [Var "Z"]))));
  assert_eq __LINE__ parse_string "(p(X) => q(X)) => r(X)" (Implies (Implies (Pred ("p", [Var "X"]), Pred ("q", [Var "X"])), Pred ("r", [Var "X"])));
  assert_eq __LINE__ parse_string "p(X) => (q(X) => r(X))" (Implies (Pred ("p", [Var "X"]), Implies (Pred ("q", [Var "X"]), Pred ("r", [Var "X"]))));
  assert_eq __LINE__ parse_string "(p(X) <=> q(X)) <=> r(X)" (Iff (Iff (Pred ("p", [Var "X"]), Pred ("q", [Var "X"])), Pred ("r", [Var "X"])));
  assert_eq __LINE__ parse_string "p(f(X,Y))" (Pred ("p", [Func ("f", [Var "X"; Var "Y"])]));
  assert_eq __LINE__ parse_string "p(f(a,b))" (Pred ("p", [Func ("f", [Const "a"; Const "b"])]));
  assert_eq __LINE__ parse_string "p(f(f(X)))" (Pred ("p", [Func ("f", [Func ("f", [Var "X"])])]));
  assert_eq __LINE__ parse_string "p(f(f(a)))" (Pred ("p", [Func ("f", [Func ("f", [Const "a"])])]));
  assert_eq __LINE__ parse_string "p(h(f(X),g(Y)))" (Pred ("p", [Func ("h", [Func ("f", [Var "X"]); Func ("g", [Var "Y"])])]));
  assert_eq __LINE__ parse_string "p(h(f(a),g(b)))" (Pred ("p", [Func ("h", [Func ("f", [Const "a"]); Func ("g", [Const "b"])])]));
  assert_eq __LINE__ parse_string "p(X) & (~(q(X)))" (And (Pred ("p", [Var "X"]), Not (Pred ("q", [Var "X"]))));
  assert_eq __LINE__ parse_string "( ~p(X) ) | q(X)" (Or (Not (Pred ("p", [Var "X"])), Pred ("q", [Var "X"])));
  assert_eq __LINE__ parse_string "~p(X) | q(X)" (Or (Not (Pred ("p", [Var "X"])), Pred ("q", [Var "X"])));
  assert_eq __LINE__ parse_string "( ~p ) | ( ~q )" (Or (Not (Pred ("p", [])), Not (Pred ("q", []))));
  assert_eq __LINE__ parse_string "~p | ~q" (Or (Not (Pred ("p", [])), Not (Pred ("q", []))));
  assert_eq __LINE__ parse_string "( ~(p & q) ) | r" (Or (Not (And (Pred ("p", []), Pred ("q", []))), Pred ("r", [])));
  assert_eq __LINE__ parse_string "p & ( ~(q | r) )" (And (Pred ("p", []), Not (Or (Pred ("q", []), Pred ("r", [])))));
  assert_eq __LINE__ parse_string "p & ~(q | r)" (And (Pred ("p", []), Not (Or (Pred ("q", []), Pred ("r", [])))));
  assert_eq __LINE__ parse_string "! [X]: ( p(X) & ( ~q(X) ) )" (Forall ("X", And (Pred ("p", [Var "X"]), Not (Pred ("q", [Var "X"])))));
  assert_eq __LINE__ parse_string "! [X]: ( p(X) & ~q(X) )" (Forall ("X", And (Pred ("p", [Var "X"]), Not (Pred ("q", [Var "X"])))));
  assert_eq __LINE__ parse_string "? [X]: ( (~p(X)) | q(X) )" (Exists ("X", Or (Not (Pred ("p", [Var "X"])), Pred ("q", [Var "X"]))));
  assert_eq __LINE__ parse_string "? [X]: ( ~p(X) | q(X) )" (Exists ("X", Or (Not (Pred ("p", [Var "X"])), Pred ("q", [Var "X"]))));
  assert_eq __LINE__ parse_string "? [X,Y]: ( p(X,Y) & q(Y,X) )" (Exists ("X", Exists ("Y", And (Pred ("p", [Var "X"; Var "Y"]), Pred ("q", [Var "Y"; Var "X"])))));
  assert_eq __LINE__ parse_string "! [X,Y]: p(f(X),g(Y))" (Forall ("X", Forall ("Y", Pred ("p", [Func ("f", [Var "X"]); Func ("g", [Var "Y"])]))));
  assert_eq __LINE__ parse_string "! [X]: ( ? [Y]: p(X,Y) )" (Forall ("X", Exists ("Y", Pred ("p", [Var "X"; Var "Y"]))));
  assert_eq __LINE__ parse_string "? [Y]: ( ! [X]: p(X,Y) )" (Exists ("Y", Forall ("X", Pred ("p", [Var "X"; Var "Y"]))));
  assert_eq __LINE__ parse_string "( p(a) & q(b) ) & r(c)" (And (And (Pred ("p", [Const "a"]), Pred ("q", [Const "b"])), Pred ("r", [Const "c"])));
  assert_eq __LINE__ parse_string "(p(a) & q(b)) & r(c)" (And (And (Pred ("p", [Const "a"]), Pred ("q", [Const "b"])), Pred ("r", [Const "c"])));
  assert_eq __LINE__ parse_string "p(a) & (q(b) & r(c))" (And (Pred ("p", [Const "a"]), And (Pred ("q", [Const "b"]), Pred ("r", [Const "c"]))));
  assert_eq __LINE__ parse_string "(p(a) | q(b)) | r(c)" (Or (Or (Pred ("p", [Const "a"]), Pred ("q", [Const "b"])), Pred ("r", [Const "c"])));
  assert_eq __LINE__ parse_string "p(a) | (q(b) | r(c))" (Or (Pred ("p", [Const "a"]), Or (Pred ("q", [Const "b"]), Pred ("r", [Const "c"]))));
  assert_eq __LINE__ parse_string "p(X,X)" (Pred ("p", [Var "X"; Var "X"]));
  assert_eq __LINE__ parse_string "p(f(X),X)" (Pred ("p", [Func ("f", [Var "X"]); Var "X"]));
  assert_eq __LINE__ parse_string "p(X,f(X))" (Pred ("p", [Var "X"; Func ("f", [Var "X"])]));
  assert_eq __LINE__ parse_string "p(f(X),f(Y))" (Pred ("p", [Func ("f", [Var "X"]); Func ("f", [Var "Y"])]));
  assert_eq __LINE__ parse_string "p(f(g(X)),h(k(Y)))" (Pred ("p", [Func ("f", [Func ("g", [Var "X"])]); Func ("h", [Func ("k", [Var "Y"])])]));
  assert_eq __LINE__ parse_string "p(h(f(g(X)),k(Y)))" (Pred ("p", [Func ("h", [Func ("f", [Func ("g", [Var "X"])]); Func ("k", [Var "Y"])])]));
  assert_eq __LINE__ parse_string "p(a,b,c)" (Pred ("p", [Const "a"; Const "b"; Const "c"]));
  assert_eq __LINE__ parse_string "p(f(a),b,c)" (Pred ("p", [Func ("f", [Const "a"]); Const "b"; Const "c"]));
  assert_eq __LINE__ parse_string "p(a,f(b),c)" (Pred ("p", [Const "a"; Func ("f", [Const "b"]); Const "c"]));
  assert_eq __LINE__ parse_string "p(a,b,f(c))" (Pred ("p", [Const "a"; Const "b"; Func ("f", [Const "c"])]));
  assert_eq __LINE__ parse_string "p(f(a),f(b),f(c))" (Pred ("p", [Func ("f", [Const "a"]); Func ("f", [Const "b"]); Func ("f", [Const "c"])]));
  assert_eq __LINE__ parse_string "p(f(f(a)),g(g(b)))" (Pred ("p", [Func ("f", [Func ("f", [Const "a"])]); Func ("g", [Func ("g", [Const "b"])])]));
  assert_eq __LINE__ parse_string "p(h(a),h(b))" (Pred ("p", [Func ("h", [Const "a"]); Func ("h", [Const "b"])]));
  assert_eq __LINE__ parse_string "p(h(f(a)),h(f(b)))" (Pred ("p", [Func ("h", [Func ("f", [Const "a"])]); Func ("h", [Func ("f", [Const "b"])])]));
  assert_eq __LINE__ parse_string "p(f(X,a),g(Y,b))" (Pred ("p", [Func ("f", [Var "X"; Const "a"]); Func ("g", [Var "Y"; Const "b"])]));
  assert_eq __LINE__ parse_string "p(f(X,Y),g(Y,X))" (Pred ("p", [Func ("f", [Var "X"; Var "Y"]); Func ("g", [Var "Y"; Var "X"])]));
  assert_eq __LINE__ parse_string "p(f(X,Y,Z))" (Pred ("p", [Func ("f", [Var "X"; Var "Y"; Var "Z"])]));
  assert_eq __LINE__ parse_string "p(f(a,b,c))" (Pred ("p", [Func ("f", [Const "a"; Const "b"; Const "c"])]));
  assert_eq __LINE__ parse_string "p(f(X,f(Y,Z)))" (Pred ("p", [Func ("f", [Var "X"; Func ("f", [Var "Y"; Var "Z"])])]));
  assert_eq __LINE__ parse_string "p(f(f(X),Y))" (Pred ("p", [Func ("f", [Func ("f", [Var "X"]); Var "Y"])]));
  assert_eq __LINE__ parse_string "p(f(g(X),h(Y,Z)))" (Pred ("p", [Func ("f", [Func ("g", [Var "X"]); Func ("h", [Var "Y"; Var "Z"])])]));
  assert_eq __LINE__ parse_string "( p(X) & q(X) ) => r(X)" (Implies (And (Pred ("p", [Var "X"]), Pred ("q", [Var "X"])), Pred ("r", [Var "X"])));
  assert_eq __LINE__ parse_string "p(X) => ( q(X) & r(X) )" (Implies (Pred ("p", [Var "X"]), And (Pred ("q", [Var "X"]), Pred ("r", [Var "X"]))));
  assert_eq __LINE__ parse_string "(p(X) | q(X)) => r(X)" (Implies (Or (Pred ("p", [Var "X"]), Pred ("q", [Var "X"])), Pred ("r", [Var "X"])));
  assert_eq __LINE__ parse_string "p(X) => (q(X) | r(X))" (Implies (Pred ("p", [Var "X"]), Or (Pred ("q", [Var "X"]), Pred ("r", [Var "X"]))));
  assert_eq __LINE__ parse_string "(p(X) <=> q(X)) & r(X)" (And (Iff (Pred ("p", [Var "X"]), Pred ("q", [Var "X"])), Pred ("r", [Var "X"])));
  assert_eq __LINE__ parse_string "p(X) & (q(X) <=> r(X))" (And (Pred ("p", [Var "X"]), Iff (Pred ("q", [Var "X"]), Pred ("r", [Var "X"]))));
  assert_eq __LINE__ parse_string "~(p(X) <=> q(X))" (Not (Iff (Pred ("p", [Var "X"]), Pred ("q", [Var "X"]))));
  assert_eq __LINE__ parse_string "( ~(p(X)) ) <=> q(X)" (Iff (Not (Pred ("p", [Var "X"])), Pred ("q", [Var "X"])));
  assert_eq __LINE__ parse_string "p(a) <=> ( ~(q(a)) )" (Iff (Pred ("p", [Const "a"]), Not (Pred ("q", [Const "a"]))));
  assert_eq __LINE__ parse_string "! [X]: ( p(X) <=> q(X) )" (Forall ("X", Iff (Pred ("p", [Var "X"]), Pred ("q", [Var "X"]))));
  assert_eq __LINE__ parse_string "? [X]: ( p(X) => q(X) )" (Exists ("X", Implies (Pred ("p", [Var "X"]), Pred ("q", [Var "X"]))));
  assert_eq __LINE__ parse_string "! [X,Y]: ( p(X) => q(Y) )" (Forall ("X", Forall ("Y", Implies (Pred ("p", [Var "X"]), Pred ("q", [Var "Y"])))));
  assert_eq __LINE__ parse_string "? [X,Y]: ( p(X) | q(Y) )" (Exists ("X", Exists ("Y", Or (Pred ("p", [Var "X"]), Pred ("q", [Var "Y"])))));
  assert_eq __LINE__ parse_string "! [X]: ( p(f(X)) => q(g(X)) )" (Forall ("X", Implies (Pred ("p", [Func ("f", [Var "X"])]), Pred ("q", [Func ("g", [Var "X"])]))));
  assert_eq __LINE__ parse_string "? [X]: ( p(f(X)) & q(g(X)) )" (Exists ("X", And (Pred ("p", [Func ("f", [Var "X"])]), Pred ("q", [Func ("g", [Var "X"])]))));
  assert_eq __LINE__ parse_string "! [X]: ( f(X)=X )" (Forall ("X", Pred ("=", [Func ("f", [Var "X"]); Var "X"])));
  assert_eq __LINE__ parse_string "? [X]: X=f(X)" (Exists ("X", Pred ("=", [Var "X"; Func ("f", [Var "X"])])));
  assert_eq __LINE__ parse_string "! [X]: ( p(X) | ( ~p(X) ) )" (Forall ("X", Or (Pred ("p", [Var "X"]), Not (Pred ("p", [Var "X"])))));
  assert_eq __LINE__ parse_string "? [X]: ( ~(p(X) & q(X)) )" (Exists ("X", Not (And (Pred ("p", [Var "X"]), Pred ("q", [Var "X"])))));
  assert_eq __LINE__ parse_string "p(f(X,Y), h(a, b))" (Pred ("p", [Func ("f", [Var "X"; Var "Y"]); Func ("h", [Const "a"; Const "b"])]));
  assert_eq __LINE__ parse_string "p(h(f(a),g(b)), k(c))" (Pred ("p", [Func ("h", [Func ("f", [Const "a"]); Func ("g", [Const "b"])]); Func ("k", [Const "c"])]));
  assert_eq __LINE__ parse_string "(p(X) & q(Y)) => (r(Z) | s(W))" (Implies (And (Pred ("p", [Var "X"]), Pred ("q", [Var "Y"])), Or (Pred ("r", [Var "Z"]), Pred ("s", [Var "W"]))));
  assert_eq __LINE__ parse_string "( ~(p(a) & q(b)) ) => r(c)" (Implies (Not (And (Pred ("p", [Const "a"]), Pred ("q", [Const "b"]))), Pred ("r", [Const "c"])));
  assert_eq __LINE__ parse_string "p(a) => ( ~(q(b) | r(c)) )" (Implies (Pred ("p", [Const "a"]), Not (Or (Pred ("q", [Const "b"]), Pred ("r", [Const "c"])))));
  assert_eq __LINE__ parse_string "! [X]: ( ~(p(X) => q(X)) )" (Forall ("X", Not (Implies (Pred ("p", [Var "X"]), Pred ("q", [Var "X"])))));
  assert_eq __LINE__ parse_string "? [X]: ( ~(p(X) <=> q(X)) )" (Exists ("X", Not (Iff (Pred ("p", [Var "X"]), Pred ("q", [Var "X"])))));
  assert_eq __LINE__ parse_string "! [X,Y]:( ~(p(X) & q(Y)) )" (Forall ("X", Forall ("Y", Not (And (Pred ("p", [Var "X"]), Pred ("q", [Var "Y"]))))));
  assert_eq __LINE__ parse_string "? [X,Y]: ( ~(p(f(X)) | q(g(Y))) )" (Exists ("X", Exists ("Y", Not (Or (Pred ("p", [Func ("f", [Var "X"])]), Pred ("q", [Func ("g", [Var "Y"])]))))));
  assert_eq __LINE__ parse_string "p(f(g(h(X))))" (Pred ("p", [Func ("f", [Func ("g", [Func ("h", [Var "X"])])])]));
  assert_eq __LINE__ parse_string "p(f(g(h(a))))" (Pred ("p", [Func ("f", [Func ("g", [Func ("h", [Const "a"])])])]));
  assert_eq __LINE__ parse_string "p(f(g(h(X))), f(g(h(Y))))" (Pred ("p", [Func ("f", [Func ("g", [Func ("h", [Var "X"])])]); Func ("f", [Func ("g", [Func ("h", [Var "Y"])])])]));
  assert_eq __LINE__ parse_string "( p(X) & p(Y) ) => p(Z)" (Implies (And (Pred ("p", [Var "X"]), Pred ("p", [Var "Y"])), Pred ("p", [Var "Z"])));
  assert_eq __LINE__ parse_string "( p(a) & p(b) ) => p(c)" (Implies (And (Pred ("p", [Const "a"]), Pred ("p", [Const "b"])), Pred ("p", [Const "c"])));
  assert_eq __LINE__ parse_string "( p(X) | p(Y) ) <=> p(Z)" (Iff (Or (Pred ("p", [Var "X"]), Pred ("p", [Var "Y"])), Pred ("p", [Var "Z"])));
  assert_eq __LINE__ parse_string "X = X" (Pred ("=", [Var "X"; Var "X"]));
  assert_eq __LINE__ parse_string "a = a" (Pred ("=", [Const "a"; Const "a"]));
  assert_eq __LINE__ parse_string "p(X) & (q(Y) | (r(Z) & s(W)))" (And (Pred ("p", [Var "X"]), Or (Pred ("q", [Var "Y"]), And (Pred ("r", [Var "Z"]), Pred ("s", [Var "W"])))));
  assert_eq __LINE__ parse_string "(p(X) & q(Y)) | (r(Z) & s(W))" (Or (And (Pred ("p", [Var "X"]), Pred ("q", [Var "Y"])), And (Pred ("r", [Var "Z"]), Pred ("s", [Var "W"]))));
  assert_eq __LINE__ parse_string "~((p) & (q))" (Not (And (Pred ("p", []), Pred ("q", []))));
  assert_eq __LINE__ parse_string "~((p(X)) | (q(Y)))" (Not (Or (Pred ("p", [Var "X"]), Pred ("q", [Var "Y"]))));
  assert_eq __LINE__ parse_string "! [X]: ( ~(~p(X)) )" (Forall ("X", Not (Not (Pred ("p", [Var "X"])))));
  assert_eq __LINE__ parse_string "? [X]: ( ~(~(p(X) & q(X))) )" (Exists ("X", Not (Not (And (Pred ("p", [Var "X"]), Pred ("q", [Var "X"]))))));
  assert_eq __LINE__ parse_string "! [X]: (p(X))" (Forall ("X", Pred ("p", [Var "X"])));
  assert_eq __LINE__ parse_string "? [X]: (p(X))" (Exists ("X", Pred ("p", [Var "X"])));
  assert_eq __LINE__ parse_string "! [X,Y,Z]: p(X,Y,Z)" (Forall ("X", Forall ("Y", Forall ("Z", Pred ("p", [Var "X"; Var "Y"; Var "Z"])))));
  assert_eq __LINE__ parse_string "? [X,Y,Z]: p(X,Y,Z)" (Exists ("X", Exists ("Y", Exists ("Z", Pred ("p", [Var "X"; Var "Y"; Var "Z"])))));
  assert_eq __LINE__ parse_string "! [X]: ( ? [Y,Z]: p(X,Y,Z) )" (Forall ("X", Exists ("Y", Exists ("Z", Pred ("p", [Var "X"; Var "Y"; Var "Z"])))));
  assert_eq __LINE__ parse_string "? [X]: ( ! [Y,Z]: p(X,Y,Z) )" (Exists ("X", Forall ("Y", Forall ("Z", Pred ("p", [Var "X"; Var "Y"; Var "Z"])))));
  assert_eq __LINE__ parse_string "! [X]: ( ( p(X) & q(X) ) <=> r(X) )" (Forall ("X", Iff (And (Pred ("p", [Var "X"]), Pred ("q", [Var "X"])), Pred ("r", [Var "X"]))));
  assert_eq __LINE__ parse_string "? [X]: ( ( p(X) | q(X) ) => r(X) )" (Exists ("X", Implies (Or (Pred ("p", [Var "X"]), Pred ("q", [Var "X"])), Pred ("r", [Var "X"]))));

  (* Equality between function terms variants *)
assert_eq __LINE__ parse_string "f(X)=g(Y)" (Pred ("=", [Func ("f", [Var "X"]); Func ("g", [Var "Y"])]));
assert_eq __LINE__ parse_string "f(X)=g(Y,Z)" (Pred ("=", [Func ("f", [Var "X"]); Func ("g", [Var "Y"; Var "Z"])]));
assert_eq __LINE__ parse_string "f(f(X)) = g(g(Y))" (Pred ("=", [Func ("f", [Func ("f", [Var "X"])]); Func ("g", [Func ("g", [Var "Y"])])]));

(* Equality with variables and constants mix *)
assert_eq __LINE__ parse_string "X = f(X)" (Pred ("=", [Var "X"; Func ("f", [Var "X"])]));
assert_eq __LINE__ parse_string "f(X) = X" (Pred ("=", [Func ("f", [Var "X"]); Var "X"]));
assert_eq __LINE__ parse_string "a = f(a)" (Pred ("=", [Const "a"; Func ("f", [Const "a"])]));
assert_eq __LINE__ parse_string "f(a) = a" (Pred ("=", [Func ("f", [Const "a"]); Const "a"]));

(* Equality inside larger formulas *)
assert_eq __LINE__ parse_string "(f(X)=g(Y)) & p(X)" (And (Pred ("=", [Func ("f", [Var "X"]); Func ("g", [Var "Y"])]), Pred ("p", [Var "X"])));
assert_eq __LINE__ parse_string "p(X) => (f(X)=f(X))" (Implies (Pred ("p", [Var "X"]), Pred ("=", [Func ("f", [Var "X"]); Func ("f", [Var "X"])])));
assert_eq __LINE__ parse_string "(a=b) <=> q(a,b)" (Iff (Pred ("=", [Const "a"; Const "b"]), Pred ("q", [Const "a"; Const "b"])));

(* Function vs constant disambiguation in terms *)
assert_eq __LINE__ parse_string "p(f(a), a)" (Pred ("p", [Func ("f", [Const "a"]); Const "a"]));
assert_eq __LINE__ parse_string "p(a, f(a))" (Pred ("p", [Const "a"; Func ("f", [Const "a"])]));
assert_eq __LINE__ parse_string "p(f(a,b), g(c,d))" (Pred ("p", [Func ("f", [Const "a"; Const "b"]); Func ("g", [Const "c"; Const "d"])]));
assert_eq __LINE__ parse_string "p(f(g(a)), h(k(b)))" (Pred ("p", [Func ("f", [Func ("g", [Const "a"])]); Func ("h", [Func ("k", [Const "b"])])]));

(* Quantifiers around equalities *)
assert_eq __LINE__ parse_string "! [X]: f(X)=X" (Forall ("X", Pred ("=", [Func ("f", [Var "X"]); Var "X"])));
assert_eq __LINE__ parse_string "? [X]: X=f(X)" (Exists ("X", Pred ("=", [Var "X"; Func ("f", [Var "X"])])));
assert_eq __LINE__ parse_string "! [X,Y]: f(X)=g(Y)" (Forall ("X", Forall ("Y", Pred ("=", [Func ("f", [Var "X"]); Func ("g", [Var "Y"])]))));

(* Additional Skolem-constant parsing examples *)
assert_eq __LINE__ parse_string "p(sk.0.1)" (Pred ("p", [SkolemConst [Z.of_int 0; Z.of_int 1]]));
assert_eq __LINE__ parse_string "q(sk.2)" (Pred ("q", [SkolemConst [Z.of_int 2]]));
assert_eq __LINE__ parse_string "q(sk.3.4)" (Pred ("q", [SkolemConst [Z.of_int 3; Z.of_int 4]]));
assert_eq __LINE__ parse_string "p(sk.1, sk.2)" (Pred ("p", [SkolemConst [Z.of_int 1]; SkolemConst [Z.of_int 2]]));
assert_eq __LINE__ parse_string "p(f(sk.5))" (Pred ("p", [Func ("f", [SkolemConst [Z.of_int 5]])]));
assert_eq __LINE__ parse_string "p(f(sk.1.2), g(sk.3))" (Pred ("p", [Func ("f", [SkolemConst [Z.of_int 1; Z.of_int 2]]); Func ("g", [SkolemConst [Z.of_int 3]])]));
assert_eq __LINE__ parse_string "sk.7 = sk.7" (Pred ("=", [SkolemConst [Z.of_int 7]; SkolemConst [Z.of_int 7]]));
assert_eq __LINE__ parse_string "f(sk.8) = g(sk.9)" (Pred ("=", [Func ("f", [SkolemConst [Z.of_int 8]]); Func ("g", [SkolemConst [Z.of_int 9]])]));
assert_eq __LINE__ parse_string "a = sk.10.0" (Pred ("=", [Const "a"; SkolemConst [Z.of_int 10; Z.of_int 0]]));
assert_eq __LINE__ parse_string "p(sk.0.2) & q(sk.0.2)" (And (Pred ("p", [SkolemConst [Z.of_int 0; Z.of_int 2]]), Pred ("q", [SkolemConst [Z.of_int 0; Z.of_int 2]])));
assert_eq __LINE__ parse_string "p(sk.4) | q(sk.4)" (Or (Pred ("p", [SkolemConst [Z.of_int 4]]), Pred ("q", [SkolemConst [Z.of_int 4]])));
assert_eq __LINE__ parse_string "p(sk.6) => q(sk.6)" (Implies (Pred ("p", [SkolemConst [Z.of_int 6]]), Pred ("q", [SkolemConst [Z.of_int 6]])));
assert_eq __LINE__ parse_string "p(sk.1) <=> q(sk.1)" (Iff (Pred ("p", [SkolemConst [Z.of_int 1]]), Pred ("q", [SkolemConst [Z.of_int 1]])));
assert_eq __LINE__ parse_string "~p(sk.2.2)" (Not (Pred ("p", [SkolemConst [Z.of_int 2; Z.of_int 2]])));
assert_eq __LINE__ parse_string "p(h(sk.3, sk.4))" (Pred ("p", [Func ("h", [SkolemConst [Z.of_int 3]; SkolemConst [Z.of_int 4]])]));
assert_eq __LINE__ parse_string "p(h(f(sk.5)), g(sk.6.7))" (Pred ("p", [Func ("h", [Func ("f", [SkolemConst [Z.of_int 5]])]); Func ("g", [SkolemConst [Z.of_int 6; Z.of_int 7]])]));
assert_eq __LINE__ parse_string "(p(sk.8) & q(sk.9)) | r(sk.8.9)" (Or (And (Pred ("p", [SkolemConst [Z.of_int 8]]), Pred ("q", [SkolemConst [Z.of_int 9]])), Pred ("r", [SkolemConst [Z.of_int 8; Z.of_int 9]])));
assert_eq __LINE__ parse_string "p(f(g(sk.1.0)))" (Pred ("p", [Func ("f", [Func ("g", [SkolemConst [Z.of_int 1; Z.of_int 0]])])]));
assert_eq __LINE__ parse_string "p(sk.11) & ( ~(q(sk.11)) )" (And (Pred ("p", [SkolemConst [Z.of_int 11]]), Not (Pred ("q", [SkolemConst [Z.of_int 11]]))));
assert_eq __LINE__ parse_string "(f(sk.2.3)=f(sk.2.3)) <=> p(sk.2.3)" (Iff (Pred ("=", [Func ("f", [SkolemConst [Z.of_int 2; Z.of_int 3]]); Func ("f", [SkolemConst [Z.of_int 2; Z.of_int 3]])]), Pred ("p", [SkolemConst [Z.of_int 2; Z.of_int 3]])));
assert_eq __LINE__ parse_string "p(sk.12.34)" (Pred ("p", [SkolemConst [Z.of_int 12; Z.of_int 34]]));


(* Additional Skolem-function parsing examples using "sk" prefix *)

assert_eq __LINE__ parse_string "p(sk.1(a))"
  (Pred ("p", [SkolemFunc ([Z.of_int 1], [Const "a"])]));
assert_eq __LINE__ parse_string "p(sk.2.3(X,Y))"
  (Pred ("p", [SkolemFunc ([Z.of_int 2; Z.of_int 3], [Var "X"; Var "Y"])]));
assert_eq __LINE__ parse_string "f(sk.4(b,c)) = g(sk.4(b,c))"
  (Pred ("=", [Func ("f", [SkolemFunc ([Z.of_int 4], [Const "b"; Const "c"])]);
               Func ("g", [SkolemFunc ([Z.of_int 4], [Const "b"; Const "c"])])]));
assert_eq __LINE__ parse_string "p(sk.7(X), sk.7(Y))"
  (Pred ("p", [SkolemFunc ([Z.of_int 7], [Var "X"]); SkolemFunc ([Z.of_int 7], [Var "Y"])]));
assert_eq __LINE__ parse_string "p(sk.8.0(f(a)))"
  (Pred ("p", [SkolemFunc ([Z.of_int 8; Z.of_int 0], [Func ("f", [Const "a"])])]));
assert_eq __LINE__ parse_string "p(f(sk.9(Z)), g(sk.9(Z)))"
  (Pred ("p", [Func ("f", [SkolemFunc ([Z.of_int 9], [Var "Z"])]);
               Func ("g", [SkolemFunc ([Z.of_int 9], [Var "Z"])])]));
assert_eq __LINE__ parse_string "~q(sk.10.11(a,b))"
  (Not (Pred ("q", [SkolemFunc ([Z.of_int 10; Z.of_int 11], [Const "a"; Const "b"])])));
assert_eq __LINE__ parse_string "p(sk.12(X)) & q(sk.12(Y))"
  (And (Pred ("p", [SkolemFunc ([Z.of_int 12], [Var "X"])]),
       Pred ("q", [SkolemFunc ([Z.of_int 12], [Var "Y"])])));
assert_eq __LINE__ parse_string "p(sk.13.14(X)) | r(sk.13.14(f(X)))"
  (Or (Pred ("p", [SkolemFunc ([Z.of_int 13; Z.of_int 14], [Var "X"])]),
      Pred ("r", [SkolemFunc ([Z.of_int 13; Z.of_int 14], [Func ("f", [Var "X"])])])));
assert_eq __LINE__ parse_string "p(sk.16.1(a,b)) <=> q(sk.16.1(b,a))"
  (Iff (Pred ("p", [SkolemFunc ([Z.of_int 16; Z.of_int 1], [Const "a"; Const "b"])]),
       Pred ("q", [SkolemFunc ([Z.of_int 16; Z.of_int 1], [Const "b"; Const "a"])])));
assert_eq __LINE__ parse_string "f(sk.17(X,Y), sk.18(Z))"
  (Pred ("f", [SkolemFunc ([Z.of_int 17], [Var "X"; Var "Y"]);
               SkolemFunc ([Z.of_int 18], [Var "Z"])]));
assert_eq __LINE__ parse_string "! [X]: p(sk.24(X))"
  (Forall ("X", Pred ("p", [SkolemFunc ([Z.of_int 24], [Var "X"])])));
assert_eq __LINE__ parse_string "? [X]: ( r(sk.25.26(X,X)) & s(sk.25.26(a,b)) )"
  (Exists ("X", And (Pred ("r", [SkolemFunc ([Z.of_int 25; Z.of_int 26], [Var "X"; Var "X"])]),
                    Pred ("s", [SkolemFunc ([Z.of_int 25; Z.of_int 26], [Const "a"; Const "b"])]))));
assert_eq __LINE__ parse_string "p(f(sk.27(g(X))))"
  (Pred ("p", [Func ("f", [SkolemFunc ([Z.of_int 27], [Func ("g", [Var "X"])])])]));
assert_eq __LINE__ parse_string "((p(sk.28(X)) => q(sk.28(X))) & (q(sk.28(X)) => r(sk.28(X)))) => (p(sk.28(X)) => r(sk.28(X)))"
  (Implies (And (Implies (Pred ("p", [SkolemFunc ([Z.of_int 28], [Var "X"])]),
                         Pred ("q", [SkolemFunc ([Z.of_int 28], [Var "X"])])),
                     Implies (Pred ("q", [SkolemFunc ([Z.of_int 28], [Var "X"])]),
                              Pred ("r", [SkolemFunc ([Z.of_int 28], [Var "X"])]))),
           Implies (Pred ("p", [SkolemFunc ([Z.of_int 28], [Var "X"])]),
                    Pred ("r", [SkolemFunc ([Z.of_int 28], [Var "X"])]))));
()

open Types

exception ErrorNotAdmissible
exception ErrorMalformedAxiom
exception ErrorMalformedRule
exception ErrorUnknownAxiom
exception ErrorUnknownRule

module StringSet = Set.Make(String)

let rec var_occurs_in_term var = function
  | Var v -> v = var
  | Const _ -> false
  | SkolemConst _ -> false
  | SkolemFunc (_, terms)
  | Func (_, terms) -> List.exists (var_occurs_in_term var) terms

let rec var_occurs_free_in_formula var = function
  | True -> false
  | False -> false
  | Pred(_, args) -> List.exists (var_occurs_in_term var) args
  | Not f -> var_occurs_free_in_formula var f
  | And(a, b) -> (var_occurs_free_in_formula var a) || (var_occurs_free_in_formula var b)
  | Or(a, b) -> (var_occurs_free_in_formula var a) || (var_occurs_free_in_formula var b)
  | Implies(a, b) -> (var_occurs_free_in_formula var a) || (var_occurs_free_in_formula var b)
  | Iff(a, b) -> (var_occurs_free_in_formula var a) || (var_occurs_free_in_formula var b)
  | Exists(v, f) -> not (v = var) && var_occurs_free_in_formula var f
  | Forall(v, f) -> not (v = var) && var_occurs_free_in_formula var f

let rec free_vars_term term : StringSet.t =
  match term with
  | Var x -> StringSet.singleton x
  | Const _ | SkolemConst _ -> StringSet.empty
  | Func (_, args) | SkolemFunc (_, args) ->
      List.fold_left
        (fun vars arg -> StringSet.union vars (free_vars_term arg))
        StringSet.empty args


let rec free_vars_formula formula : StringSet.t =
  match formula with
  | True | False -> StringSet.empty
  | Pred (_, terms) ->
      List.fold_left
        (fun vars t -> StringSet.union vars (free_vars_term t))
        StringSet.empty terms
  | Not f -> free_vars_formula f
  | And (f1, f2)
  | Or (f1, f2)
  | Implies (f1, f2)
  | Iff (f1, f2) ->
      StringSet.union (free_vars_formula f1) (free_vars_formula f2)
  | Forall (x, f)
  | Exists (x, f) ->
      let vars = free_vars_formula f in
      StringSet.remove x vars

let rec substitute_in_term var replacement t = 
  match t with
    | Var v -> if v = var then replacement else Var v
    | Const _ -> t
    | SkolemConst _ -> t
    | Func (f, args) -> Func (f, List.map (substitute_in_term var replacement) args)
    | SkolemFunc (f, args) -> SkolemFunc (f, List.map (substitute_in_term var replacement) args)


let rec substitute_in_formula var replacement = function
    | True -> True
    | False -> False
    | Pred (p, args) -> Pred (p, List.map (substitute_in_term var replacement) args)
    | Not f -> Not (substitute_in_formula var replacement f)
    | And(a, b) -> And (substitute_in_formula var replacement a, substitute_in_formula var replacement b)
    | Or(a, b) -> Or (substitute_in_formula var replacement a, substitute_in_formula var replacement b)
    | Implies(a, b) -> Implies (substitute_in_formula var replacement a, substitute_in_formula var replacement b)
    | Iff(a, b) -> Iff (substitute_in_formula var replacement a, substitute_in_formula var replacement b)
    | Exists(v, f) when v = var -> Exists(v, f)
    | Exists(v, f) when not (var_occurs_in_term v replacement) && v != var -> Exists(v, substitute_in_formula var replacement f)
    | Exists(_, _) -> raise ErrorNotAdmissible
    | Forall(v, f) when v = var -> Forall(v, f)
    | Forall(v, f) when not (var_occurs_in_term v replacement) && v != var -> Forall(v, substitute_in_formula var replacement f)
    | Forall(_, _) -> raise ErrorNotAdmissible


let axiom_error = function () -> raise ErrorMalformedAxiom

let axiom = function
  | "LEM" -> (function [a], [] -> Or(a, Not a) | _, _ -> axiom_error())
  | "IMP" -> (function [a; b], [] -> Implies(a, (Implies(b, a))) | _ -> axiom_error())
  | "ANL" -> (function [a; b], [] -> Implies(And(a, b), a) | _ -> axiom_error())
  | "ANR" -> (function [a; b], [] -> Implies(And(a, b), b) | _ -> axiom_error())
  | "AND" -> (function [a; b], [] -> Implies(a, Implies(b, And(a, b))) | _ -> axiom_error())
  | "ORL" -> (function [a; b], [] -> Implies(a, Or(a, b)) | _ -> axiom_error())
  | "ORR" -> (function [a; b], [] -> Implies(b, Or(a, b)) | _ -> axiom_error())
  | "DIS" -> (function [a; b; c], [] -> Implies(Implies(a, c), Implies(Implies(b, c), Implies(Or(a, b), c))) | _ -> axiom_error())
  | "CON" -> (function [a; b], [] -> Implies(Not a, Implies(a, b)) | _ -> axiom_error())
  | "IFI" -> (function [a; b], [] -> Implies(Implies(a, b), Implies(Implies(b, a), Iff(a, b))) | _ -> axiom_error())
  | "IFO" -> (function [a; b], [] -> Implies(Iff(a, b), And(Implies(a, b), Implies(b, a))) | _ -> axiom_error())
  | "ALL" -> (function [a], [t; Var(v)] -> Implies(Forall(v, a), substitute_in_formula v t a) | _ -> axiom_error())
  | "EXT" -> (function [a], [t; Var(v)] -> Implies(substitute_in_formula v t a, Exists(v, a)) | _ -> axiom_error())
  | _ -> raise ErrorUnknownAxiom

let rule = function
  | "IDN" -> (function [a], [] -> a | _ -> raise ErrorMalformedRule)
  | "MOD" -> (function [Implies(a, b); c], [] when c=a -> b | _ -> raise ErrorMalformedRule)
  | "GEN" -> (function [a], [Var(v)] -> Forall(v, a) | _ -> raise ErrorMalformedRule)
  | "SKO" -> 
    (function 
      | [Exists(v, a)], [SkolemConst(ref_seq); Var v1] when v=v1 -> substitute_in_formula v (SkolemConst ref_seq) a
      | [Exists(v, a)], [SkolemFunc(ref_seq, args); Var v1] when v=v1 -> substitute_in_formula v (SkolemFunc (ref_seq, args)) a
      | _ -> raise ErrorMalformedRule)
  | _ -> raise ErrorUnknownRule

let rec (>>) current_ref ref = 
  match current_ref, ref with 
    | Ref [k], Ref [i] when i < k -> true
    | Ref (head::_), Ref [i] when i < head -> true
    | Ref (head::tail), Ref(head_ref::tail_ref) when head = head_ref -> Ref tail >> Ref tail_ref
    | _, _ -> false;;

let (>>=) r1 r2 = r1 >> r2 || r1 = r2

let rec is_suffix r1 r2 =
  match r1, r2 with
    | Ref [], Ref _ -> true
    | Ref (head1::tail1), Ref (head2::tail2) when head1 = head2 -> is_suffix (Ref tail1) (Ref tail2)
    | _, _ -> false

let rec get_statement proof = 
  match proof with 
    | Statement {statements; _} -> 
          (fun ref -> 
            match ref with
            | Ref [] -> proof
            | Ref (head::tail) -> get_statement (List.nth statements (Z.to_int head)) (Ref tail))

let formula_of_statement s = 
  match s with Statement {formula; _} -> formula

let formula_of_proof proof ref = get_statement proof ref |> formula_of_statement

let derive_formula proof rule_label refs terms = 
  rule rule_label (List.map (formula_of_proof proof) refs, terms)
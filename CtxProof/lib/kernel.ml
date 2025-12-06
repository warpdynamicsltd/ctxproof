open Types
open Errors

exception KernelError of Errors.kernel_error_code
exception KernelPosError of Errors.kernel_error_code * Lexing.position

module StringSet = Set.Make(String)

(* Map module for reference type to enable efficient caching *)
module RefMap = Map.Make(struct
  type t = reference
  let compare r1 r2 =
    match r1, r2 with
    | Ref l1, Ref l2 ->
        let rec compare_lists lst1 lst2 =
          match lst1, lst2 with
          | [], [] -> 0
          | [], _ -> -1
          | _, [] -> 1
          | h1::t1, h2::t2 ->
              let c = Z.compare h1 h2 in
              if c = 0 then compare_lists t1 t2 else c
        in
        compare_lists l1 l2
end)

(* Cache type for storing statement and formula lookups *)
type proof_cache = {
  mutable statement_cache: statement RefMap.t;
  mutable formula_cache: first_order_formula RefMap.t;
  mutable assumption_cache: first_order_formula RefMap.t;
  mutable validation_cache: bool RefMap.t;
}

(* Create a new cache *)
let create_cache () = {
  statement_cache = RefMap.empty;
  formula_cache = RefMap.empty;
  assumption_cache = RefMap.empty;
  validation_cache = RefMap.empty;
}

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
    | Exists(_, _) -> raise (KernelError NotAdmissible)
    | Forall(v, f) when v = var -> Forall(v, f)
    | Forall(v, f) when not (var_occurs_in_term v replacement) && v != var -> Forall(v, substitute_in_formula var replacement f)
    | Forall(_, _) -> raise (KernelError NotAdmissible)

let axiom_error = function () -> raise (KernelError MalformedAxiom)

let axiom = function
  | "TRU" -> (function [], [] -> True | _, _ -> axiom_error())
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
  | _ -> raise (KernelError UnknownAxiom)

let rule = function
  | "IDN" -> (function [a], [] -> a | _ -> raise (KernelError MalformedRule))
  | "MOD" -> (function [Implies(a, b); c], [] when c=a -> b | _ -> raise (KernelError MalformedRule))
  | "GEN" -> (function [a], [Var(v)] -> Forall(v, a) | _ -> raise (KernelError MalformedRule))
  | "SKO" ->
    (function
      | [Exists(v, a)], [SkolemConst(ref_seq); Var v1] when v=v1 -> substitute_in_formula v (SkolemConst ref_seq) a
      | [Exists(v, a)], [SkolemFunc(ref_seq, args); Var v1] when v=v1 -> substitute_in_formula v (SkolemFunc (ref_seq, args)) a
      | _ -> raise (KernelError MalformedRule))
  | _ -> raise (KernelError MalformedRule)

let rec (>>) current_ref ref =
  match current_ref, ref with
    | Ref [k], Ref [i] when i < k -> true
    | Ref (head::_), Ref [i] when i < head -> true
    | Ref (head::tail), Ref(head_ref::tail_ref) when head = head_ref -> Ref tail >> Ref tail_ref
    | _, _ -> false;;

let (>>=) r1 r2 = r1 >> r2 || r1 = r2


let rec skolem_compatibility_with_ref_in_term cmp ref t =
  match t with
  | Var _ | Const _ -> true
  | SkolemConst seq ->
      if not (cmp ref (Ref seq)) then raise (KernelError NotAllowedSkolemTerm)
      else true
  | SkolemFunc (seq, terms) ->
      if not (cmp ref (Ref seq)) then raise (KernelError NotAllowedSkolemTerm)
      else List.for_all (skolem_compatibility_with_ref_in_term cmp ref) terms
  | Func (_, terms) ->
      List.for_all (skolem_compatibility_with_ref_in_term cmp ref) terms

let rec skolem_compatibility_with_ref_in_formula cmp ref f =
    match f with
      | True -> true
      | False -> true
      | Pred (_, args) -> List.for_all (skolem_compatibility_with_ref_in_term cmp ref) args
      | Not f1 -> skolem_compatibility_with_ref_in_formula cmp ref f1
      | And (f1, f2)
      | Or (f1, f2)
      | Implies (f1, f2)
      | Iff (f1, f2) -> skolem_compatibility_with_ref_in_formula cmp ref f1 && skolem_compatibility_with_ref_in_formula cmp ref f2
      | Exists (_, f1)
      | Forall (_, f1) -> skolem_compatibility_with_ref_in_formula cmp ref f1

let formula_less_than_ref ref f =
  if  skolem_compatibility_with_ref_in_formula (>>) ref f then true
  else raise (KernelError NotAllowedSkolemTerm)

let formula_lesseq_than_ref ref f =
  if skolem_compatibility_with_ref_in_formula (>>=) ref f then true
  else raise (KernelError NotAllowedSkolemTerm)

let rec is_suffix ref r =
  match ref, r with
    | Ref _, Ref [] -> true
    | Ref (head1::tail1), Ref (head2::tail2) when head1 = head2 -> is_suffix (Ref tail1) (Ref tail2)
    | _, _ -> false

let append ref i =
  match ref with
  | Ref lst -> Ref (lst @ [i])

let last_elem lst = Z.to_int (List.nth lst (List.length lst - 1))

let last_of_ref ref = match ref with Ref lst -> last_elem lst

(* Uncached version of get_statement for internal use *)
let rec get_statement_uncached proof ref =
  match proof with
    | Statement {statements; _}
      -> match ref with
          | Ref [] -> proof
          | Ref (head::tail) ->
              let index = Z.to_int head in
              if index < Array.length statements
                then get_statement_uncached statements.(index) (Ref tail)
              else raise (KernelError RefOutOfBound)

(* Cached version of get_statement *)
let get_statement cache proof ref =
  match RefMap.find_opt ref cache.statement_cache with
  | Some stmt -> stmt
  | None ->
      let stmt = get_statement_uncached proof ref in
      cache.statement_cache <- RefMap.add ref stmt cache.statement_cache;
      stmt

let formula_of_statement s =
  match s with Statement {formula; _} -> formula

let ref_of_statement s =
  match s with Statement {ref; _} -> ref

(* Cached version of formula_of_proof *)
let formula_of_proof cache proof ref =
  match RefMap.find_opt ref cache.formula_cache with
  | Some formula -> formula
  | None ->
      let formula = get_statement cache proof ref |> formula_of_statement in
      cache.formula_cache <- RefMap.add ref formula cache.formula_cache;
      formula

(* Cached version of assumption_of_proof *)
let assumption_of_proof cache proof ref =
  match RefMap.find_opt ref cache.assumption_cache with
  | Some assumption -> assumption
  | None ->
      let assumption =
        match get_statement cache proof ref with Statement {formula; _} ->
          match formula with
          | Implies(a, _) -> a
          | _ -> raise (KernelError ImplicationFormExpected)
      in
      cache.assumption_cache <- RefMap.add ref assumption cache.assumption_cache;
      assumption

let rec var_occurs_free_in_assumptions proof ref_ var =
  match proof with
    | Statement {ref; formula=Implies(a, _); statements; inference=Inference {mode=Context;_};_} when is_suffix ref_ ref
        -> (var_occurs_free_in_formula var a) || Array.exists (fun s -> var_occurs_free_in_assumptions s ref_ var) statements
    | _ -> false

let sko_rule_constrain cache ref terms refs proof =
  let formula = formula_of_proof cache proof (List.nth refs 0) in
  let sk_term = List.nth terms 0 in
  if
    List.for_all
      (fun v -> var_occurs_free_in_formula v formula && not (var_occurs_free_in_assumptions proof ref v))
      (StringSet.elements (free_vars_term sk_term))
    &&
    List.for_all
    (fun v -> var_occurs_in_term v sk_term || var_occurs_free_in_assumptions proof ref v)
    (StringSet.elements (free_vars_formula formula))
  then
    match ref, sk_term with
      | Ref seq, SkolemConst seq1 -> seq = seq1
      | Ref seq, SkolemFunc(seq1, _) -> seq = seq1
      | _ -> false
  else false

let gen_rule_constrain ref terms proof =
  match List.nth terms 0 with
    | Var v -> not (var_occurs_free_in_assumptions proof ref v)
    | _ -> false

let derive_formula cache proof rule_label refs terms =
  rule rule_label (List.map (formula_of_proof cache proof) refs, terms)

let pos_of_statement statement =
  match statement with
    | Statement {pos; _} -> pos

let is_formula_derived cache ref formula proof rule_label refs terms =
  match rule_label with
    | "SKO" when sko_rule_constrain cache ref terms refs proof -> formula = derive_formula cache proof rule_label refs terms
    | "GEN" when gen_rule_constrain ref terms proof -> formula = derive_formula cache proof rule_label refs terms
    | "MOD" | "IDN" -> formula = derive_formula cache proof rule_label refs terms
    | _ -> raise (KernelError RuleConstraintViolation)

let formula_of_generalized_formula cache proof gf =
  match gf with
  | Reference ref -> formula_of_proof cache proof ref
  | Formula f -> f

let ref_of_generised_formula gf =
  match gf with
  | Reference ref -> ref
  | Formula _ -> raise (KernelError ReferenceExpected)

let pass b code = if b then b else raise (KernelError code)

(* Cached version of prove_thesis *)
let rec prove_thesis_cached cache proof ref_ =
  (* Check if already validated *)
  match RefMap.find_opt ref_ cache.validation_cache with
  | Some true -> true
  | Some false | None ->
      (* Perform validation *)
      let result =
        match get_statement cache proof ref_ with
        | Statement
          {
            ref;
            formula;
            inference;
            statements;
            pos;
          } when ref = ref_ ->
            (try
              (match inference with
              | Inference {mode = Axiom axiom_label; gformulas; terms}
                ->
                  pass
                  (
                    formula_lesseq_than_ref ref_ formula
                    && axiom axiom_label (List.map (formula_of_generalized_formula cache proof) gformulas, terms) = formula
                  )
                  AxiomViolation

              | Inference {mode = Assumption; gformulas; _}
                ->
                  pass
                  (
                    let refs = List.map ref_of_generised_formula gformulas in
                    let r = (List.nth refs 0) in
                      formula_less_than_ref ref_ formula
                      && is_suffix ref_ r
                      && assumption_of_proof cache proof r = formula
                  )
                  AssumptionViolation

              | Inference {mode = Rule rule_label; gformulas; terms}
                ->
                  pass
                  (
                    let refs = List.map ref_of_generised_formula gformulas in
                      formula_lesseq_than_ref ref_ formula
                      && List.for_all ((>>) ref_) refs
                      && List.for_all (prove_thesis_cached cache proof) refs
                      && is_formula_derived cache ref_ formula proof rule_label refs terms
                  )
                  RuleViolation

              | Inference {mode = Context; _}
                ->
                  match formula with
                  | Implies(_, last_formula)
                    ->
                      pass
                      (
                        let len = Array.length statements in
                        let last_statement = statements.(len - 1) in
                          formula_less_than_ref ref_ last_formula
                          && last_formula = formula_of_statement last_statement
                          && prove_thesis_cached cache proof (ref_of_statement last_statement)
                      )
                      ContextViolation
                  | _ -> raise (KernelError ImplicationFormExpected)
            )
          with
          | KernelPosError (code, p) -> raise (KernelPosError (code, p))
          | KernelError code -> raise (KernelPosError (code, pos))
          | Failure _ -> raise (KernelPosError (UnknownProblem, pos))
        )

        | Statement {pos;_} -> raise (KernelPosError (InvalidReference, pos))
      in
      (* Cache the successful validation result *)
      cache.validation_cache <- RefMap.add ref_ true cache.validation_cache;
      result

(* Public API - creates cache and validates the proof *)
let prove_thesis proof ref_ =
  let cache = create_cache () in
  prove_thesis_cached cache proof ref_

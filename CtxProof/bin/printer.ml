open Types

let rec string_of_term t =
  match t with
  | Var v -> v
  | Const c -> c
  | ContextConst c -> "context_" ^ Int.to_string c
  | SkolemConst lst -> "skolem _" ^ String.concat "_" (List.map Int.to_string lst) 
  | Func (f, args) ->
      f ^ "(" ^ (String.concat ", " (List.map string_of_term args)) ^ ")"

let rec string_of_formula f =
  match f with
  | True -> "⊤"
  | False -> "⊥"
  | Pred (name, args) ->
      name ^ "(" ^ (String.concat ", " (List.map string_of_term args)) ^ ")"
  | Not f1 -> "¬(" ^ string_of_formula f1 ^ ")"
  | And (f1, f2) -> "(" ^ string_of_formula f1 ^ " ∧ " ^ string_of_formula f2 ^ ")"
  | Or (f1, f2) -> "(" ^ string_of_formula f1 ^ " ∨ " ^ string_of_formula f2 ^ ")"
  | Implies (f1, f2) -> "(" ^ string_of_formula f1 ^ " → " ^ string_of_formula f2 ^ ")"
  | Iff (f1, f2) -> "(" ^ string_of_formula f1 ^ " ↔ " ^ string_of_formula f2 ^ ")"
  | Forall (v, f1) -> "∀" ^ v ^ ".(" ^ string_of_formula f1 ^ ")"
  | Exists (v, f1) -> "∃" ^ v ^ ".(" ^ string_of_formula f1 ^ ")"

let string_of_statement s = 
  match s with 
  | Statement {name; formula_role; formula; annotation} ->
      name ^ " : " ^ formula_role ^ " : " ^ string_of_formula formula ^ " : " ^ annotation
;;
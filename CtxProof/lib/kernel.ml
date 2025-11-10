open Types

module StringSet = Set.Make(String)

let rec free_vars_term term : StringSet.t =
  match term with
  | Var x -> StringSet.singleton x
  | Const _ | SkolemConst _ -> StringSet.empty
  | Func (_, args) | SkolemFunc (_, args) ->
      List.fold_left
        (fun vars arg -> StringSet.union vars (free_vars_term arg))
        StringSet.empty args
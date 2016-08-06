
type var_name = string

type untyped_tree =
  | UTContentOf  of var_name
  | UTApply      of untyped_tree * untyped_tree
  | UTLambda     of var_name * untyped_tree
  | UTFixPoint   of var_name * untyped_tree
  | UTIfThenElse of untyped_tree * untyped_tree * untyped_tree
  | UTNumConst   of int
  | UTBoolConst  of bool


let rec string_of_untyped_tree utast =
  let iter = string_of_untyped_tree in
    match utast with
    | UTContentOf(varnm)                   -> varnm
    | UTApply(utast1, utast2)              -> "(" ^ (iter utast1) ^ " " ^ (iter utast2) ^ ")"
    | UTLambda(varnm, utast1)              -> "(\\" ^ varnm ^ ". " ^ (iter utast1) ^ ")"
    | UTFixPoint(varnm, utast1)            -> "(fix " ^ varnm ^ ". " ^ (iter utast1) ^ ")"
    | UTIfThenElse(utast0, utast1, utast2) -> "(if " ^ (iter utast0) ^ " then " ^ (iter utast1) ^ " else " ^ (iter utast2) ^ ")"
    | UTNumConst(nc)                       -> string_of_int nc
    | UTBoolConst(bc)                      -> string_of_bool bc

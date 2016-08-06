
type variable_name = string

type source_type = source_type_main * Range.t
and source_type_main =
  | TypeVariable of int
  | IntType
  | BoolType
  | FuncType     of source_type * source_type

type untyped_tree = untyped_tree_main * Range.t
and untyped_tree_main =
  | SrcContentOf  of variable_name
  | SrcApply      of untyped_tree * untyped_tree
  | SrcLambda     of (variable_name * Range.t) * untyped_tree
  | SrcFixPoint   of (variable_name * Range.t) * untyped_tree
  | SrcIfThenElse of untyped_tree * untyped_tree * untyped_tree
  | SrcNext       of untyped_tree
  | SrcPrev       of untyped_tree
  | SrcBox        of untyped_tree
  | SrcUnbox      of (variable_name * Range.t) * int * untyped_tree * untyped_tree
  | SrcIntConst   of int
  | SrcBoolConst  of bool

type abstract_tree =
  | ContentOf        of variable_name
  | Apply            of abstract_tree * abstract_tree
  | LambdaAbstract   of variable_name * source_type * abstract_tree
  | FixPoint         of variable_name * source_type * abstract_tree
  | IfThenElse       of abstract_tree * abstract_tree * abstract_tree
  | Next             of abstract_tree
  | Prev             of abstract_tree
  | Box              of abstract_tree
  | Unbox            of variable_name * int * abstract_tree * abstract_tree
  | IntConst         of int
  | BoolConst        of bool


let rec string_of_untyped_tree utast =
  let iter = string_of_untyped_tree in
  let (utastmain, _) = utast in
    match utastmain with
    | SrcContentOf(varnm)                     -> varnm
    | SrcApply(utast1, utast2)                -> "(" ^ (iter utast1) ^ " " ^ (iter utast2) ^ ")"
    | SrcLambda((varnm, _), utast1)           -> "(\\" ^ varnm ^ ". " ^ (iter utast1) ^ ")"
    | SrcFixPoint((varnm, _), utast1)         -> "(fix " ^ varnm ^ ". " ^ (iter utast1) ^ ")"
    | SrcIfThenElse(utast0, utast1, utast2)   -> "(if " ^ (iter utast0) ^ " then " ^ (iter utast1) ^ " else " ^ (iter utast2) ^ ")"
    | SrcNext(utast1)                         -> "(next " ^ (iter utast1) ^ ")"
    | SrcPrev(utast1)                         -> "(prev " ^ (iter utast1) ^ ")"
    | SrcBox(utast1)                          -> "(box " ^ (iter utast1) ^ ")"
    | SrcUnbox((varnm, _), i, utast1, utast2) -> "(unbox " ^ varnm ^ " =" ^ (string_of_int i)
                                                ^ " " ^ (iter utast1) ^ " in " ^ (iter utast2) ^ ")"
    | SrcIntConst(nc)                         -> string_of_int nc
    | SrcBoolConst(bc)                        -> string_of_bool bc

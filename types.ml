
let print_for_debug msg = ()


type variable_name = string

type source_type = source_type_main * Range.t
and source_type_main =
  | TypeVariable of int
  | IntType
  | BoolType
  | FuncType     of source_type * source_type
  | CircleType   of source_type
  | BoxType      of source_type

type source_tree = source_tree_main * Range.t
and source_tree_main =
  | SrcOrdContentOf  of variable_name
  | SrcPermContentOf of variable_name
  | SrcApply         of source_tree * source_tree
  | SrcLambda        of (variable_name * Range.t) * source_tree
  | SrcFixPoint      of (variable_name * Range.t) * source_tree
  | SrcIfThenElse    of source_tree * source_tree * source_tree
  | SrcNext          of source_tree
  | SrcPrev          of source_tree
  | SrcBox           of source_tree
  | SrcUnbox         of (variable_name * Range.t) * int * source_tree * source_tree
  | SrcIntConst      of int
  | SrcBoolConst     of bool

type abstract_tree =
  | OrdContentOf  of variable_name
  | PermContentOf of variable_name
  | Apply         of abstract_tree * abstract_tree
  | Lambda        of variable_name * abstract_tree
  | FixPoint      of variable_name * abstract_tree
  | IfThenElse    of abstract_tree * abstract_tree * abstract_tree
  | Next          of abstract_tree
  | Prev          of abstract_tree
  | Box           of abstract_tree
  | Unbox         of variable_name * int * abstract_tree * abstract_tree
  | IntConst      of int
  | BoolConst     of bool


let rec string_of_source_tree sast =
  let iter = string_of_source_tree in
  let (sastmain, _) = sast in
    match sastmain with
    | SrcOrdContentOf(varnm)                -> varnm
    | SrcPermContentOf(varnm)               -> varnm
    | SrcApply(sast1, sast2)                -> "(" ^ (iter sast1) ^ " " ^ (iter sast2) ^ ")"
    | SrcLambda((varnm, _), sast1)          -> "(\\" ^ varnm ^ ". " ^ (iter sast1) ^ ")"
    | SrcFixPoint((varnm, _), sast1)        -> "(fix " ^ varnm ^ ". " ^ (iter sast1) ^ ")"
    | SrcIfThenElse(sast0, sast1, sast2)    -> "(if " ^ (iter sast0) ^ " then " ^ (iter sast1) ^ " else " ^ (iter sast2) ^ ")"
    | SrcNext(sast1)                        -> "(next " ^ (iter sast1) ^ ")"
    | SrcPrev(sast1)                        -> "(prev " ^ (iter sast1) ^ ")"
    | SrcBox(sast1)                         -> "(box " ^ (iter sast1) ^ ")"
    | SrcUnbox((varnm, _), i, sast1, sast2) -> "(unbox " ^ varnm ^ " =" ^ (string_of_int i)
                                                ^ " " ^ (iter sast1) ^ " in " ^ (iter sast2) ^ ")"
    | SrcIntConst(nc)                       -> string_of_int nc
    | SrcBoolConst(bc)                      -> string_of_bool bc


let rec string_of_source_type (srcty : source_type) =
  let iter = string_of_source_type in
  let iter_enclose srcty =
    match srcty with
    | (FuncType(_, _), _) -> "(" ^ (iter srcty) ^ ")"
    | _                   -> iter srcty
  in
  let (srctymain, _) = srcty in
    match srctymain with
    | TypeVariable(i)        -> "'" ^ (string_of_int i)
    | IntType                -> "int"
    | BoolType               -> "bool"
    | CircleType(tyin)       -> "O" ^ (iter_enclose tyin)
    | BoxType(tyin)          -> "B" ^ (iter_enclose tyin)
    | FuncType(tydom, tycod) -> (iter_enclose tydom) ^ " -> " ^ (iter tycod)


let rec erase_range_of_source_type (srcty : source_type) =
  let (srctymain, _) = srcty in
  let iter = erase_range_of_source_type in
  let dr = Range.dummy "erased" in
    match srctymain with
    | FuncType(tydom, tycod) -> (FuncType(iter tydom, iter tycod), dr)
    | CircleType(tyin)       -> (CircleType(iter tyin), dr)
    | BoxType(tyin)          -> (BoxType(iter tyin), dr)
    | _                      -> (srctymain, dr)

open Types

type t = (int * source_type) list

exception UnificationContradictionError of source_type * source_type
exception UnificationInclusionError     of int * source_type * source_type
exception Contradiction
exception Inclusion                     of int


let empty = []


let rec find (theta : t) (tvid : int) =
  match theta with
  | []                             -> raise Not_found
  | (i, ty) :: tail when i = tvid  -> ty
  | (i, ty) :: tail                -> find tail tvid


let rec apply_to_source_type (theta : t) (tystr : source_type) =
  let (tystrmain, rng) = tystr in
    match tystrmain with
    | TypeVariable(tvid) ->
        begin
          try let (tyfoundmain, _) = find theta tvid in (tyfoundmain, rng) with
          | Not_found -> tystr
        end
    | FuncType(tydom, tycod) ->
        let tydomnew = apply_to_source_type theta tydom in
        let tycodnew = apply_to_source_type theta tycod in
          (FuncType(tydomnew, tycodnew), rng)
    | _ -> tystr


let rec apply_to_abstract_tree (theta : t) (ast : abstract_tree) = ast


let rec emerge_in tvid tystr =
  let (tystrmain, _) = tystr in
    match tystrmain with
    | TypeVariable(i)  when i = tvid -> true
    | TypeVariable(_)                -> false
    | FuncType(tydom, tycod)         -> (emerge_in tvid tydom) || (emerge_in tvid tycod)
    | _                              -> false


let rec overwrite_source_type tystrobj tvid tystr =
  let (tystrobjmain, rng) = tystrobj in
    match tystrobjmain with
    | TypeVariable(i)  when i = tvid -> tystr
    | TypeVariable(i)                -> (TypeVariable(i), rng)
    | FuncType(tydom, tycod)         ->
        let tydomnew = overwrite_source_type tydom tvid tystr in
        let tycodnew = overwrite_source_type tycod tvid tystr in
          (FuncType(tydomnew, tycodnew), rng)
    | other                          -> (other, rng)


let rec overwrite theta tvid tystr =
  match theta with
  | []                                            -> []
  | (tvidhd, _      ) :: tail  when tvidhd = tvid -> (tvid, tystr) :: tail
(*  | (tvidhd, _      ) :: tail  when tvidhd = tvid -> (tvid, tystr) :: (overwrite tail tvid tystr) *)
  | (tvidhd, tystrhd) :: tail                     -> (tvidhd, overwrite_source_type tystrhd tvid tystr) :: (overwrite tail tvid tystr)


let rec overwrite_or_add theta tvid tystr =
  match theta with
  | []                                            -> [(tvid, tystr)]
  | (tvidhd, _      ) :: tail  when tvidhd = tvid -> (tvid, tystr) :: tail
  | (tvidhd, tystrhd) :: tail                     -> (tvidhd, overwrite_source_type tystrhd tvid tystr) :: (overwrite_or_add tail tvid tystr)


let rec eliminate theta tvid =
  match theta with
  | [] -> []
  | (tvidhd, _      ) :: tail  when tvidhd = tvid -> tail
  | (tvidhd, tystrhd) :: tail                     -> (tvidhd, tystrhd) :: (eliminate tail tvid)


(*  ---- ---- ---- ----
  make_consistent theta :
    replaces all TypeVariable(tvid) in theta with tystr
    for each (tvid, tystr) in theta
    before checking whether TypeVariable(tvid) occurs inside tystr for each (tvid, tystr)
    ---- ---- ---- ----  *)
let rec make_consistent (theta : t) =
  let rec check_inclusion (theta : t) =
    match theta with
    | []                                               -> ()
    | (tvid, tystr) :: tail  when emerge_in tvid tystr -> raise (Inclusion(tvid))
    | (tvid, tystr) :: tail                            -> check_inclusion tail
  in
  let rec make_consistent_sub (rest : t) (from : t) =
    match rest with
    | []                    -> let _ = check_inclusion from in from
    | (tvid, tystr) :: tail -> make_consistent_sub tail (overwrite from tvid tystr)
  in
    make_consistent_sub theta theta


let rec unify tystr1 tystr2 =
  try unify_sub tystr1 tystr2 with
  | Contradiction   -> raise (UnificationContradictionError(tystr1, tystr2))
  | Inclusion(tvid) -> raise (UnificationInclusionError(tvid, tystr1, tystr2))

and unify_sub tystr1 tystr2 =
  let (tystrmain1, rng1) = tystr1 in
  let (tystrmain2, rng2) = tystr2 in
    match (tystrmain1, tystrmain2) with
    | (IntType, IntType)                                            -> empty
    | (BoolType, BoolType)                                          -> empty
    | (FuncType(tydom1, tycod1), FuncType(tydom2, tycod2))          -> compose (unify_sub tycod1 tycod2) (unify_sub tydom1 tydom2)

    | (TypeVariable(tvid1), TypeVariable(tvid2)) when tvid1 = tvid2 -> empty
    | (TypeVariable(tvid1), TypeVariable(tvid2)) when tvid1 < tvid2 -> if Range.is_dummy rng2 then [(tvid1, (tystrmain2, rng1))]
                                                                                              else [(tvid1, tystr2)]
    | (TypeVariable(tvid1), TypeVariable(tvid2))                    -> unify_sub tystr2 tystr1

    | (TypeVariable(tvid1), _)          when emerge_in tvid1 tystr2 -> raise (Inclusion(tvid1))
    | (TypeVariable(tvid1), _)                                      -> [(tvid1, tystr2)]
    | (_, TypeVariable(tvid2))                                      -> unify_sub tystr2 tystr1

    | _                                                             -> raise Contradiction

and compose (theta2 : t) (theta1 : t) = make_consistent (compose_prim theta2 theta1)

and compose_prim theta2 theta1 =
  match theta2 with
  | []                     -> theta1
  | (tvid, tystr2) :: tail ->
      begin
        try
          let tystr1 = find theta1 tvid in
            (tvid, tystr1) :: (compose_prim (eliminate theta1 tvid) (compose_prim tail (unify tystr1 tystr2)))
        with
        | Not_found -> (compose_prim tail (overwrite_or_add theta1 tvid tystr2))
      end


let compose_list (thetalst : t list) = List.fold_right compose thetalst empty

(*
let show (theta : t) =
  let rec show_sub theta =
    match theta with
    | []                 -> ()
    | (tvid, ty) :: tail ->
       let strtvid = display_source_type (TypeVariable(tvid), Range.dummy "s") in
       let strty   = display_source_type ty in
       begin
         print_for_debug ("  | " ^ strtvid ^ " = " ^ strty) ;
         show_sub tail
       end
  in
  begin
    print_for_debug "  +---- ---- ---- ----" ;
    show_sub theta ;
    print_for_debug "  +---- ---- ---- ----"
  end
*)

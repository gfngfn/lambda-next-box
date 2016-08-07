open Types

type t = (variable_name * int * source_type) list

let fresh_source_type_variable_id : int ref = ref 0


let empty = []


let rec find (tyenv : t) (varnm : variable_name) =
  match tyenv with
  | []             -> raise Not_found
  | (v, l, t) :: tail -> if v = varnm then (l, t) else find tail varnm


let add (tyenv : t) (varnm : variable_name) (layer : int) (tystr : source_type) =
  (varnm, layer, tystr) :: tyenv


let make (lst : (variable_name * int * source_type) list) = lst


let fresh_source_type_variable rng =
  let newtvid = !fresh_source_type_variable_id in
    begin
      fresh_source_type_variable_id := !fresh_source_type_variable_id + 1 ;
      (TypeVariable(newtvid), rng)
    end

open Types

type t

val empty : t

val find : t -> variable_name -> source_type

val add : t -> variable_name -> source_type -> t

val make : (variable_name * source_type) list -> t

val fresh_source_type_variable : Range.t -> source_type

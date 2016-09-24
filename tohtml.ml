open Types


let rec html_of_abstract_tree (ast : abstract_tree) =
  let iter = html_of_abstract_tree in
  let (astmain, layer) = ast in
  let token s = "<span class=\"token\">" ^ s ^ "</span>" in
  let single s = "<span class\"single\">" ^ s ^ "</span>" in
  let strlayer s = "<div class=\"ast" ^ (string_of_int (if layer >= 8 then 8 else layer)) ^ "\">" ^ s ^ "</div>" in
    match astmain with
    | OrdContentOf(ovnm)           -> strlayer (single ovnm)
    | PermContentOf(pvnm)          -> strlayer (single pvnm)
    | Apply(ast1, ast2)            -> strlayer ((iter ast1) ^ " " ^ (iter ast2))
    | Lambda(ovnm, ast1)           -> strlayer ((token "&lambda;") ^ ovnm ^ (token ".") ^ " " ^ (iter ast1))
(*    | FixPoint(ovnm, ast1)         -> strlayer ((token "fix") ^ " " ^ ovnm ^ ". " ^ (iter ast1)) *)
    | FixPoint(ovnm, ast1)         -> strlayer ((token "fix") ^ " " ^ ovnm ^ ". ...")
    | IfThenElse(ast0, ast1, ast2) -> strlayer ((token "if") ^ " " ^ (iter ast0) ^ " " ^ (token "then") ^ " " ^ (iter ast1)
                                                ^ " " ^ (token "else") ^ " " ^ (iter ast2))
    | Next(ast1)                   -> strlayer ((token "next") ^ " " ^ (iter ast1))
    | Prev(ast1)                   -> strlayer ((token "prev") ^ " " ^ (iter ast1))
    | Box(ast1)                    -> strlayer ((token "box") ^ " " ^ (iter ast1))
    | Unbox(pvnm, i, ast1, ast2)   -> strlayer ((token "unbox") ^ " " ^ pvnm ^ " " ^ (token ("=" ^ (string_of_int i)))
                                                ^ " " ^ (iter ast1) ^ " "^ (token "in") ^ " " ^ (iter ast2))
    | IntConst(ic)                 -> strlayer (single (string_of_int ic))
    | BoolConst(bc)                -> strlayer (single (string_of_bool bc))


let main (outputfun : string -> unit) (tracelst : abstract_tree list) =
  begin
    outputfun "<html lang=\"en\">\n" ;
    outputfun "  <head>\n" ;
    outputfun "    <title>A Trace of Reduction (CBV)</title>\n" ;
    outputfun "    <link rel=\"stylesheet\" media=\"all\" href=\"lambda-next-box-trace.css\">\n" ;
    outputfun "  </head>\n" ;
    outputfun "  <body><div class=\"container\">\n" ;
    List.iter (fun ast -> outputfun ("    " ^ (html_of_abstract_tree ast) ^ "<br>\n")) tracelst ;
    outputfun "  </div></body>\n" ;
    outputfun "</html>\n"
  end


open Types

type eval_state = Stable | Changed


let delta_reduction (ast : abstract_tree) =
  let (astmain, layer) = ast in
    match astmain with
    | Apply((Apply((PermContentOf("+"), _), (IntConst(ic1), _)), _), (IntConst(ic2), _))    -> (Changed, (IntConst(ic1 + ic2), layer))
    | Apply((Apply((PermContentOf("-"), _), (IntConst(ic1), _)), _), (IntConst(ic2), _))    -> (Changed, (IntConst(ic1 - ic2), layer))
    | Apply((Apply((PermContentOf("*"), _), (IntConst(ic1), _)), _), (IntConst(ic2), _))    -> (Changed, (IntConst(ic1 * ic2), layer))
    | Apply((Apply((PermContentOf("/"), _), (IntConst(ic1), _)), _), (IntConst(ic2), _))    -> (Changed, (IntConst(ic1 / ic2), layer))
    | Apply((Apply((PermContentOf("=="), _), (IntConst(ic1), _)), _), (IntConst(ic2), _))   -> (Changed, (BoolConst(ic1 == ic2), layer))
    | Apply((Apply((PermContentOf(">="), _), (IntConst(ic1), _)), _), (IntConst(ic2), _))   -> (Changed, (BoolConst(ic1 >= ic2), layer))
    | Apply((Apply((PermContentOf("<="), _), (IntConst(ic1), _)), _), (IntConst(ic2), _))   -> (Changed, (BoolConst(ic1 <= ic2), layer))
    | Apply((Apply((PermContentOf(">"), _), (IntConst(ic1), _)), _), (IntConst(ic2), _))    -> (Changed, (BoolConst(ic1 > ic2), layer))
    | Apply((Apply((PermContentOf("<"), _), (IntConst(ic1), _)), _), (IntConst(ic2), _))    -> (Changed, (BoolConst(ic1 < ic2), layer))
    | Apply((Apply((PermContentOf("&&"), _), (BoolConst(bc1), _)), _), (BoolConst(bc2), _)) -> (Changed, (BoolConst(bc1 && bc2), layer))
    | Apply((Apply((PermContentOf("||"), _), (BoolConst(bc1), _)), _), (BoolConst(bc2), _)) -> (Changed, (BoolConst(bc1 || bc2), layer))
    | _                                                                                     -> (Stable, ast)


let rec replace (ast1 : abstract_tree) (varnm : variable_name) (ast2 : abstract_tree) =
  let iter ast = replace ast varnm ast2 in
  let (astmain1, layer) = ast1 in
    match astmain1 with
    | OrdContentOf(ovnm)          when ovnm = varnm -> ast2
    | OrdContentOf(_)                               -> ast1
    | PermContentOf(pvnm)         when pvnm = varnm -> ast2
    | PermContentOf(_)                              -> ast1
    | Apply(sub1, sub2)                             -> (Apply(iter sub1, iter sub2), layer)
    | Lambda(ovnm, sub1)          when ovnm = varnm -> ast1
    | Lambda(ovnm, sub1)                            -> (Lambda(ovnm, iter sub1), layer)
    | FixPoint(ovnm, sub1)        when ovnm = varnm -> ast1
    | FixPoint(ovnm, sub1)                          -> (FixPoint(ovnm, iter sub1), layer)
    | IfThenElse(sub0, sub1, sub2)                  -> (IfThenElse(iter sub0, iter sub1, iter sub2), layer)
    | Next(sub1)                                    -> (Next(iter sub1), layer)
    | Prev(sub1)                                    -> (Prev(iter sub1), layer)
    | Box(sub1)                                     -> (Box(iter sub1), layer)
    | Unbox(pvnm, i, sub1, sub2)  when pvnm = varnm -> (Unbox(pvnm, i, iter sub1, sub2), layer)
    | Unbox(pvnm, i, sub1, sub2)                    -> (Unbox(pvnm, i, iter sub1, iter sub2), layer)
    | IntConst(_)                                   -> ast1
    | BoolConst(_)                                  -> ast1


let rec emerge_free (varnm : variable_name) (ast : abstract_tree) =
  let iter = emerge_free varnm in
  let (astmain, _) = ast in
    match astmain with
    | OrdContentOf(ovnm)                            -> ovnm = varnm
    | PermContentOf(pvnm)                           -> pvnm = varnm
    | Apply(ast1, ast2)                             -> (iter ast1) || (iter ast2)
    | Lambda(ovnm, ast1)          when ovnm = varnm -> false
    | Lambda(_, ast1)                               -> iter ast1
    | FixPoint(ovnm, ast1)        when ovnm = varnm -> false
    | FixPoint(_, ast1)                             -> iter ast1
    | IfThenElse(ast0, ast1, ast2)                  -> (iter ast0) || (iter ast1) || (iter ast2)
    | Next(ast1)                                    -> iter ast1
    | Prev(ast1)                                    -> iter ast1
    | Box(ast1)                                     -> iter ast1
    | Unbox(pvnm, _, ast1, ast2)  when pvnm = varnm -> iter ast1
    | Unbox(_, _, ast1, ast2)                       -> (iter ast1) || (iter ast2)
    | IntConst(_)                                   -> false
    | BoolConst(_)                                  -> false


let rec collect_free_variable (ast : abstract_tree) =
  let iter = collect_free_variable in
  let omit varnm = List.filter (fun v -> v <> varnm) in
  let (astmain, _) = ast in
    match astmain with
    | OrdContentOf(ovnm)                -> [ovnm]
    | PermContentOf(pvnm)               -> [pvnm]
    | Apply(ast1, ast2)                 -> List.append (iter ast1) (iter ast2)
    | Lambda(ovnm, ast1)                -> omit ovnm (iter ast1)
    | FixPoint(ovnm, ast1)              -> omit ovnm (iter ast1)
    | IfThenElse(ast0, ast1, ast2)      -> List.concat [(iter ast0); (iter ast1); (iter ast2)]
    | Next(ast1)                        -> iter ast1
    | Prev(ast1)                        -> iter ast1
    | Box(ast1)                         -> iter ast1
    | Unbox(pvnm, _, ast1, ast2)        -> List.append (iter ast1) (omit pvnm (iter ast2))
    | IntConst(_)                       -> []
    | BoolConst(_)                      -> []


let fresh_variable_id : int ref = ref 0


let initialize () =
  fresh_variable_id := 0


let fresh_variable_name () =
  let resnm = "_symb" ^ (string_of_int !fresh_variable_id) in
  begin
    fresh_variable_id := !fresh_variable_id + 1 ;
    resnm
  end


let rec alpha_renaming (prohiblst : variable_name list) (ast : abstract_tree) =
  let iter = alpha_renaming prohiblst in
  let (astmain, layer) = ast in
    match astmain with
    | OrdContentOf(_)              -> ast
    | PermContentOf(_)             -> ast
    | Apply(ast1, ast2)            -> (Apply(iter ast1, iter ast2), layer)
    | Lambda(ovnm, ast1)
      when List.mem ovnm prohiblst ->
        let ovnmnew = fresh_variable_name () in
        let ast1new = replace ast1 ovnm (OrdContentOf(ovnmnew), layer) in
          (Lambda(ovnmnew, iter ast1new), layer)
    | Lambda(ovnm, ast1)           -> (Lambda(ovnm, iter ast1), layer)
    | FixPoint(ovnm, ast1)
      when List.mem ovnm prohiblst ->
        let ovnmnew = fresh_variable_name () in
        let ast1new = replace ast1 ovnm (OrdContentOf(ovnmnew), layer) in
          (FixPoint(ovnmnew, iter ast1new), layer)
    | FixPoint(ovnm, ast1)         -> (FixPoint(ovnm, iter ast1), layer)
    | IfThenElse(ast0, ast1, ast2) -> (IfThenElse(iter ast0, iter ast1, iter ast2), layer)
    | Next(ast1)                   -> (Next(iter ast1), layer)
    | Prev(ast1)                   -> (Prev(iter ast1), layer)
    | Box(ast1)                    -> (Box(iter ast1), layer)
    | Unbox(pvnm, i, ast1, ast2)   -> (Unbox(pvnm, i, iter ast1, iter ast2), layer)
    | IntConst(_)                  -> ast
    | BoolConst(_)                 -> ast


let rec beta_reduction (ast1 : abstract_tree) (varnm : variable_name) (ast2 : abstract_tree) =
  let freevarlst = collect_free_variable ast2 in
  let ast1new = alpha_renaming freevarlst ast1 in
    replace ast1new varnm ast2


let rec eval (evlayer : int) (ast : abstract_tree) =
  let (astmain, layer) = ast in
    match astmain with

  (* -- values -- *)
    | OrdContentOf(_)  -> (Stable, ast)
    | PermContentOf(_) -> (Stable, ast)
    | IntConst(_)      -> (Stable, ast)
    | BoolConst(_)     -> (Stable, ast)

  (* -- non-values -- *)

    | Lambda(ovnm, ast1) ->
        if evlayer = layer then
          (Stable, ast)
        else if evlayer > layer then
          (Stable, ast)
        else (* if evlayer < layer *)
          let (state1, res1) = eval evlayer ast1 in
            (state1, (Lambda(ovnm, res1), layer))

    | FixPoint(ovnm, ast1) ->
        if evlayer = layer then
begin print_for_debug "****FIX1****" ; (* test *)
          (Changed, beta_reduction ast1 ovnm ast)
end (* test *)
        else if evlayer > layer then
begin print_for_debug "****FIX2****" ; (* test *)
          (Stable, ast)
end (* test *)
        else (* if evlayer < layer *)
begin print_for_debug "****FIX3****" ; (* test *)
          let (state1, res1) = eval evlayer ast1 in
            (state1, (FixPoint(ovnm, res1), layer))
end(* test *)

    | IfThenElse(ast0, ast1, ast2) ->
        if evlayer = layer then
          let (state0, res0) = eval evlayer ast0 in
          begin
            match (state0, res0) with
            | (Changed, _)                    -> (Changed, (IfThenElse(res0, ast1, ast2), layer))
            | (Stable, (BoolConst(true), _))  -> (Changed, ast1)
            | (Stable, (BoolConst(false), _)) -> (Changed, ast2)
            | (Stable, _)                     -> assert false
          end
        else if evlayer > layer then
          (Stable, ast)
        else (* if evlayer < layer *)
          let (state1, res1) = eval evlayer ast1 in
          begin
            match state1 with
            | Changed -> (Changed, (IfThenElse(ast0, res1, ast2), layer))
            | Stable  ->
                let (state2, res2) = eval evlayer ast2 in
                  (state2, (IfThenElse(ast0, res1, res2), layer))
          end

    | Next(ast1) ->
        let (state1, res1) = eval evlayer ast1 in
          (state1, (Next(res1), layer))

    | Prev(ast1) ->
        let (state1, res1) = eval evlayer ast1 in
        begin
          match (state1, res1) with
          | (Changed, _)                                             -> (Changed, (Prev(res1), layer))
          | (Stable, (Next(res1sub), layer1))  when layer1 = evlayer -> (Changed, res1sub)
          | (Stable, (Unbox(pvnm, i, ast1, ast2), _))                -> (Changed, (Unbox(pvnm, i - 1, ast1, (Prev(ast2), layer)), layer))
          | (Stable, _)                                              -> (Stable, (Prev(res1), layer))
        end

    | Box(ast1) ->
        let (state1, res1) = eval evlayer ast1 in
          (state1, (Box(res1), layer))

    | Unbox(pvnm, i, ast1, ast2) ->
        if evlayer = layer then
          let (state1, res1) = eval evlayer ast1 in
          begin
            match (state1, res1) with
            | (Changed, _)                        -> (Changed, (Unbox(pvnm, i, res1, ast2), layer))
            | (Stable, (Box(res1sub), _))         -> (Changed, replace ast2 pvnm res1sub)
            | (Stable, (Unbox(pvnm1, j, ast11, ast12), _))
                when not (emerge_free pvnm1 ast2) -> (Changed, (Unbox(pvnm1, i + j, ast11, (Unbox(pvnm, i, ast12, ast2), layer)), layer))
            | (Stable, _)                         ->
                let (state2, res2) = eval evlayer ast2 in
                  (state2, (Unbox(pvnm, i, res1, res2), layer))
          end
        else if evlayer > layer then
          (Stable, ast)
        else (* if evlayer < layer *)
          let (state1, res1) = eval evlayer ast1 in
          begin
            match state1 with
            | Changed -> (Changed, (Unbox(pvnm, i, res1, ast2), layer))
            | Stable  ->
                let (state2, res2) = eval evlayer ast2 in
                  (state2, (Unbox(pvnm, i, res1, res2), layer))
          end

    | Apply(ast1, ast2) ->
        if evlayer = layer then
begin print_for_debug "****APPLY1****" ; (* test *)
      print_for_debug ("** " ^ (string_of_abstract_tree ast1)) ; (* test *)
      print_for_debug ("** " ^ (string_of_abstract_tree ast2)) ; (* test *)
          let (state1, res1) = eval evlayer ast1 in
          begin
            match state1 with
            | Changed -> (Changed, (Apply(res1, ast2), layer))
            | Stable  ->
                let (state2, res2) = eval evlayer ast2 in
                begin
                  match state2 with
                  | Changed -> (Changed, (Apply(res1, res2), layer))
                  | Stable  ->
                      begin
                        match res1 with
                        | (Lambda(ovnm, ast1sub), _) -> (Changed, beta_reduction ast1sub ovnm res2)
                        | (Unbox(pvnm, i, ast11, ast12), _)  when not (emerge_free pvnm ast2) ->
                            (Changed, (Unbox(pvnm, i, ast11, (Apply(ast12, ast2), layer)), layer))
                        | _ -> delta_reduction (Apply(res1, res2), layer)
                      end
                end
          end
end (* test *)
        else if evlayer > layer then
begin print_for_debug "****APPLY2****" ; (* test *)
      print_for_debug ("** " ^ (string_of_abstract_tree ast1)) ; (* test *)
      print_for_debug ("** " ^ (string_of_abstract_tree ast2)) ; (* test *)
          (Stable, ast)
end (* test *)
        else (* if evlayer < layer *)
begin print_for_debug "****APPLY3****" ; (* test *)
      print_for_debug ("** " ^ (string_of_abstract_tree ast1)) ; (* test *)
      print_for_debug ("** " ^ (string_of_abstract_tree ast2)) ; (* test *)
          let (state1, res1) = eval evlayer ast1 in
          begin
            match state1 with
            | Changed -> (Changed, (Apply(res1, ast2), layer))
            | Stable  ->
                let (state2, res2) = eval evlayer ast2 in
                  (state2, (Apply(res1, res2), layer))
          end
end (* test *)


type trial = FirstTrial | AfterFirstTrial


let main (ast : abstract_tree) =
  let rec routine (tr : trial) (layer : int) (ast : abstract_tree) =
    let (state, res) = eval layer ast in
      match (tr, state) with
      | (_, Changed)              -> routine AfterFirstTrial layer res
      | (FirstTrial, Stable)      -> res
      | (AfterFirstTrial, Stable) -> routine FirstTrial (layer + 1) res
  in
    routine FirstTrial 0 ast


let trace (ast : abstract_tree) =
  let rec routine (tr : trial) (layer : int) (ast : abstract_tree) =
    let (state, res) = eval layer ast in
      match (tr, state) with
      | (_, Changed)              -> res :: (routine AfterFirstTrial layer res)
      | (FirstTrial, Stable)      -> []
      | (AfterFirstTrial, Stable) -> routine FirstTrial (layer + 1) res
  in
    routine FirstTrial 0 ast

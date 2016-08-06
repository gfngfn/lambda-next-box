open Types

exception Error of string


let rec typecheck (tyenv : Typeenv.t) (sast : source_tree) =
  let (sastmain, rng) = sast in
  match sastmain with
  | SrcIntConst(ic)     -> (IntConst(ic), (IntType, rng), Subst.empty)
  | SrcBoolConst(bc)    -> (BoolConst(bc), (BoolType, rng), Subst.empty)
  | SrcContentOf(varnm) ->
      begin
        try
          let (tyresmain, _) = Typeenv.find tyenv varnm in
            (ContentOf(varnm), (tyresmain, rng), Subst.empty)
        with
        | Not_found ->
           raise (Error
             ("at " ^ (Range.to_string rng) ^ ":\n" ^
               "    undefined variable '" ^ varnm ^ "'"))
      end

  | SrcApply(sast1, sast2) ->
      let (e1, ty1, theta1) = typecheck tyenv sast1 in
      let (e2, ty2, theta2) = typecheck tyenv sast2 in
      begin
        match ty1 with
        | (FuncType(tydom1, tycod1), _) ->
            let thetares = Subst.compose_list [Subst.unify ty2 tydom1; theta2; theta1] in
            let tyres = Subst.apply_to_source_type thetares tycod1 in
              (Apply(e1, e2), tyres, thetares)
        | _ ->
            let alpha = Typeenv.fresh_source_type_variable rng in
            let thetares = Subst.compose_list [Subst.unify ty1 (FuncType(ty2, alpha), Range.dummy "app"); theta2; theta1] in
            let tyres = Subst.apply_to_source_type thetares alpha in
              (Apply(e1, e2), tyres, thetares)
      end

  | SrcLambda((varnm, varrng), sastin) ->
      let alpha = Typeenv.fresh_source_type_variable varrng in
      let tyenvin = Typeenv.add tyenv varnm alpha in
      let (ein, tyin, thetain) = typecheck tyenvin sastin in
      let tydomres = Subst.apply_to_source_type thetain alpha in
      let tyres = (FuncType(tydomres, tyin), rng) in
        (Lambda(varnm, ein), tyres, thetain)

  | SrcFixPoint((varnm, varrng), sastin) ->
      let alpha = Typeenv.fresh_source_type_variable varrng in
      let tyenvnew = Typeenv.add tyenv varnm alpha in
      let (ein, tyin, thetain) = typecheck tyenvnew sastin in
        let thetares = Subst.compose (Subst.unify alpha tyin) thetain in
        let tyres = Subst.apply_to_source_type thetares alpha in
          (FixPoint(varnm, ein), tyres, thetares)

  | SrcIfThenElse(sast0, sast1, sast2) ->
      let (e0, ty0, theta0) = typecheck tyenv sast0 in
      let (e1, ty1, theta1) = typecheck tyenv sast1 in
      let (e2, ty2, theta2) = typecheck tyenv sast2 in
        let thetabool = Subst.unify ty0 (BoolType, Range.dummy "if") in
        let thetabrch = Subst.unify ty1 ty2 in
        let thetares = Subst.compose_list [thetabrch; theta2; theta1; thetabool; theta0] in
        let tyres = Subst.apply_to_source_type thetares ty1 in
          (IfThenElse(e0, e1, e2), tyres, thetares)


let main sast =
    try
      let (e, ty, theta) = typecheck Primitives.type_environment sast in
        Subst.apply_to_abstract_tree theta e
    with
    | Subst.UnificationInclusionError(tvid, ty1, ty2) ->
        let (_, rng1) = ty1 in
        let (_, rng2) = ty2 in
        let strrng1 = Range.to_string rng1 in
        let strrng2 = Range.to_string rng2 in
        let strty1 = string_of_source_type ty1 in
        let strty2 = string_of_source_type ty2 in
        let tvnm = string_of_source_type (TypeVariable(tvid), Range.dummy "inc") in
        let msg =
          match (Range.is_dummy rng1, Range.is_dummy rng2) with
          | (true, true)   -> "something is wrong; (" ^ (Range.message rng1) ^ ", " ^ (Range.message rng2) ^ ")"
          | (true, false)  -> ("at " ^ strrng2 ^ ":\n" ^
                                  "    this expression has types\n" ^
                                  "      " ^ strty1 ^ "\n" ^
                                  "    and\n" ^
                                  "      " ^ strty2 ^ "\n" ^
                                  "    at the same time;\n" ^
                                  "    inconsistent occurence of type variable " ^ tvnm ^ ".")
          | (false, true)  -> ("at " ^ strrng1 ^ ":\n" ^
                                  "    this expression has types\n" ^
                                  "      " ^ strty1 ^ "\n" ^
                                  "    and\n" ^
                                  "      " ^ strty2 ^ "\n" ^
                                  "    at the same time;\n" ^
                                  "    inconsistent occurence of type variable " ^ tvnm ^ ".")
          | (false, false) -> ("at " ^ strrng1 ^ " and " ^ strrng2 ^ ":\n" ^
                                  "    these expressions have types\n" ^
                                  "      " ^ strty1 ^ "\n" ^
                                  "    and\n" ^
                                  "      " ^ strty2 ^ "\n" ^
                                  "    respectively and they should be the same;\n" ^ 
                                  "    inconsistent occurrence of type variable " ^ tvnm ^ ".")
        in
          raise (Error(msg))

    | Subst.UnificationContradictionError(ty1, ty2) ->
        let (_, rng1) = ty1 in
        let (_, rng2) = ty2 in
        let strrng1 = Range.to_string rng1 in
        let strrng2 = Range.to_string rng2 in
        let strty1 = string_of_source_type ty1 in
        let strty2 = string_of_source_type ty2 in
        let msg =
          match (Range.is_dummy rng1, Range.is_dummy rng2) with
          | (true, true)   -> "something is wrong; (" ^ (Range.message rng1) ^ ", " ^ (Range.message rng2) ^ ")"
          | (true, false)  -> ("at " ^ strrng2 ^ ":\n" ^
                                 "    this expression has type\n" ^
                                 "      " ^ strty1 ^ "\n" ^
                                 "    but is expected of type\n" ^
                                 "      " ^ strty2 ^ ".")
          | (false, true)  -> ("at " ^ strrng1 ^ ":\n" ^
                                  "    this expression has type\n" ^
                                  "      " ^ strty1 ^ "\n" ^
                                  "    but is expected of type\n" ^
                                  "      " ^ strty2 ^ ".")
          | (false, false) -> ("at " ^ strrng1 ^ " and " ^ strrng2 ^ ":\n" ^
                                  "    these expressions have type\n" ^
                                  "      " ^ strty1 ^ "\n" ^
                                  "    and\n" ^
                                  "      " ^ strty2 ^ "\n" ^
                                  "    respectively, but should be the same.")
        in
          raise (Error(msg))

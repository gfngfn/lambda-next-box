open Types

exception Error of string


let rec typecheck (tyenvD : Typeenv.t) (tyenvG : Typeenv.t) (layer : int) (sast : source_tree) =
  let (sastmain, rng) = sast in
  match sastmain with
  | SrcIntConst(ic)     -> ((IntConst(ic), layer), (IntType, rng), Subst.empty)
  | SrcBoolConst(bc)    -> ((BoolConst(bc), layer), (BoolType, rng), Subst.empty)
  | SrcOrdContentOf(varnm) ->
      begin
        try
          let (varlayer, varty) = Typeenv.find tyenvG varnm in
            if varlayer = layer then
              let (tyresmain, _) = erase_range_of_source_type varty in
                ((OrdContentOf(varnm), layer), (tyresmain, rng), Subst.empty)
            else
              raise (Error
                       ("at " ^ (Range.to_string rng) ^ ":\n" ^
                           "    ordinary variable '" ^ varnm ^ "' is defined for layer " ^ (string_of_int varlayer) ^ ",\n" ^
                           "    and thereby cannot be used for layer " ^ (string_of_int layer)))
        with
        | Not_found ->
            raise (Error
                     ("at " ^ (Range.to_string rng) ^ ":\n" ^
                         "    undefined variable '" ^ varnm ^ "'"))
      end
  | SrcPermContentOf(varnm) ->
      begin
        try
          let (varlayer, varty) = Typeenv.find tyenvD varnm in
            if varlayer <= layer then
              let (tyresmain, _) = erase_range_of_source_type varty in
                ((PermContentOf(varnm), layer), (tyresmain, rng), Subst.empty)
            else
              raise (Error
                       ("at " ^ (Range.to_string rng) ^ ":\n" ^
                           "    permanent variable '" ^ varnm ^ "' is defined for layer >= " ^ (string_of_int varlayer) ^ ",\n" ^
                           "    and thereby cannot be used for layer " ^ (string_of_int layer)))
        with
        | Not_found ->
            raise (Error
                     ("at " ^ (Range.to_string rng) ^ ":\n" ^
                         "    undefined variable '" ^ varnm ^ "'"))
      end

  | SrcApply(sast1, sast2) ->
      let (e1, ty1, theta1) = typecheck tyenvD tyenvG layer sast1 in
      let (e2, ty2, theta2) = typecheck tyenvD tyenvG layer sast2 in
      begin
        match ty1 with
        | (FuncType(tydom1, tycod1), _) ->
            let thetares = Subst.compose_list [Subst.unify ty2 tydom1; theta2; theta1] in
            let tyres = Subst.apply_to_source_type thetares tycod1 in
              ((Apply(e1, e2), layer), tyres, thetares)
        | _ ->
            let alpha = Typeenv.fresh_source_type_variable rng in
            let thetares = Subst.compose_list [Subst.unify ty1 (FuncType(ty2, alpha), Range.dummy "app"); theta2; theta1] in
            let tyres = Subst.apply_to_source_type thetares alpha in
              ((Apply(e1, e2), layer), tyres, thetares)
      end

  | SrcLambda((varnm, varrng), sastin) ->
      let alpha = Typeenv.fresh_source_type_variable (Range.dummy "lambda") in
      let tyenvGin = Typeenv.add tyenvG varnm layer alpha in
      let (ein, tyin, thetain) = typecheck tyenvD tyenvGin layer sastin in
      let tydomres = Subst.apply_to_source_type thetain alpha in
      let tyres = (FuncType(tydomres, tyin), rng) in
        ((Lambda(varnm, ein), layer), tyres, thetain)

  | SrcFixPoint((varnm, varrng), sastin) ->
      let alpha = Typeenv.fresh_source_type_variable (Range.dummy "fixpoint") in
      let tyenvGin = Typeenv.add tyenvG varnm layer alpha in
      let (ein, tyin, thetain) = typecheck tyenvD tyenvGin layer sastin in
        let thetares = Subst.compose (Subst.unify alpha tyin) thetain in
        let tyres = Subst.apply_to_source_type thetares alpha in
          ((FixPoint(varnm, ein), layer), tyres, thetares)

  | SrcIfThenElse(sast0, sast1, sast2) ->
      let (e0, ty0, theta0) = typecheck tyenvD tyenvG layer sast0 in
      let (e1, ty1, theta1) = typecheck tyenvD tyenvG layer sast1 in
      let (e2, ty2, theta2) = typecheck tyenvD tyenvG layer sast2 in
        let thetabool = Subst.unify ty0 (BoolType, Range.dummy "if") in
        let thetabrch = Subst.unify ty1 ty2 in
        let thetares = Subst.compose_list [thetabrch; theta2; theta1; thetabool; theta0] in
        let tyres = Subst.apply_to_source_type thetares ty1 in
          ((IfThenElse(e0, e1, e2), layer), tyres, thetares)

  | SrcNext(sast1) ->
      let (e1, ty1, theta1) = typecheck tyenvD tyenvG (layer + 1) sast1 in
        ((Next(e1), layer), (CircleType(ty1), rng), theta1)

  | SrcPrev(sast1) ->
      if layer <= 0 then
        raise (Error
                 ("at " ^ (Range.to_string rng) ^ ":\n" ^
                     "    cannot use 'prev ...' at layer 0"))
      else
        let (_, rng1) = sast1 in
        let (e1, ty1, theta1) = typecheck tyenvD tyenvG (layer - 1) sast1 in
        begin
          match ty1 with
          | (CircleType(tyin), _) -> ((Prev(e1), layer), tyin, theta1)
          | _                     ->
              let alpha = Typeenv.fresh_source_type_variable rng1 in
              let thetares = Subst.compose (Subst.unify ty1 (CircleType(alpha), Range.dummy "prev")) theta1 in
                ((Prev(e1), layer), Subst.apply_to_source_type thetares alpha, thetares)
        end

  | SrcBox(sast1) ->
      let (e1, ty1, theta1) = typecheck tyenvD tyenvG layer sast1 in
        ((Box(e1), layer), (BoxType(ty1), rng), theta1)

  | SrcUnbox((pvnm, _), downi, sast1, sast2) ->
      let (_, rng1) = sast1 in
      let (e1, ty1, theta1) = typecheck tyenvD tyenvG (layer + downi) sast1 in
      begin
        match ty1 with
        | (BoxType(tyin), _) ->
            let tyenvDin = Typeenv.add tyenvD pvnm (layer + downi) tyin in
            let (e2, ty2, theta2) = typecheck tyenvDin tyenvG layer sast2 in
            let thetares = Subst.compose theta2 theta1 in
            let tyres = Subst.apply_to_source_type thetares ty2 in
              ((Unbox(pvnm, downi, e1, e2), layer), tyres, thetares)
        | _ ->
            let alpha = Typeenv.fresh_source_type_variable rng1 in
            let tyenvDin = Typeenv.add tyenvD pvnm (layer + downi) alpha in
            let (e2, ty2, theta2) = typecheck tyenvDin tyenvG layer sast2 in
            let thetares = Subst.compose_list [theta2; Subst.unify ty1 (BoxType(alpha), Range.dummy "unbox") ; theta1] in
            let tyres = Subst.apply_to_source_type thetares ty2 in
            ((Unbox(pvnm, downi, e1, e2), layer), tyres, thetares)
      end


let main sast =
    try
      let (e, ty, theta) = typecheck Primitives.permanent_type_environment Primitives.ordinary_type_environment 0 sast in
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

open Types


let permanent_type_environment =
  let (-->) tya tyb = (FuncType(tya, tyb), Range.dummy "prim_func") in
  let i = (IntType, Range.dummy "prim_int") in
  let b = (BoolType, Range.dummy "prim_bool") in
    Typeenv.make [
      ("+",   0,  (i --> (i --> i)));
      ("-",   0,  (i --> (i --> i)));
      ("*",   0,  (i --> (i --> i)));
      ("/",   0,  (i --> (i --> i)));
      ("==",  0,  (i --> (i --> b)));
      (">=",  0,  (i --> (i --> b)));
      ("<=",  0,  (i --> (i --> b)));
      (">",   0,  (i --> (i --> b)));
      ("<",   0,  (i --> (i --> b)));
      ("&&",  0,  (b --> (b --> b)));
      ("||",  0,  (b --> (b --> b)));
      ("not", 0,  (b --> b))
    ]

let ordinary_type_environment = Typeenv.empty

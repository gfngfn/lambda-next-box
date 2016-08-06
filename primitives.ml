open Types


let type_environment =
  let (-->) tya tyb = (FuncType(tya, tyb), Range.dummy "prim_func") in
  let i = (IntType, Range.dummy "prim_int") in
  let b = (BoolType, Range.dummy "prim_bool") in
    Typeenv.make [
      ("+",     (i --> (i --> i)));
      ("-",     (i --> (i --> i)));
      ("*",     (i --> (i --> i)));
      ("/",     (i --> (i --> i)));
      ("=",     (i --> (i --> b)));
      (">=",    (i --> (i --> b)));
      ("<=",    (i --> (i --> b)));
      (">",     (i --> (i --> b)));
      ("<",     (i --> (i --> b)));
      ("&&",    (b --> (b --> b)));
      ("||",    (b --> (b --> b)));
      ("not",   (b --> b))
    ]

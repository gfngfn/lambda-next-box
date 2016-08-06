%{
  open Types

  type range_kind = Token of Range.t | Untyped of untyped_tree


  let make_range rngknd1 rngknd2 =
    let extract_range rngknd =
      match rngknd with
      | (Token rng)        -> rng
      | (Untyped (_, rng)) -> rng
    in
      Range.unite (extract_range rngknd1) (extract_range rngknd2)

  let binary_operator left op right =
    let (opnm, oprng) = op in
    let rng = make_range (Untyped left) (Untyped right) in
      (SrcApply((SrcApply((SrcContentOf(opnm), oprng), left), Range.dummy "lor"), right), rng)

%}

%token EOI
%token<string * Range.t> INTCONST
%token<Types.variable_name * Range.t> OVAR PVAR
%token<Range.t> LET LETREC IN IF THEN ELSE
%token<Range.t> LPAREN RPAREN DEFEQ LAMBDA FIX DOT
%token<Range.t> NEXT PREV BOX UNBOX
%token<int * Range.t> DOWNS
%token<Range.t> PLUS MINUS TIMES DIVIDES
%token<Range.t> EQUAL GT LT GEQ LEQ LAND LOR TRUE FALSE

%start main
%type<Types.untyped_tree> main

%%

main:
  | xplet EOI { $1 }
;
xplet:
  | LET OVAR DEFEQ xplet IN xplet {
        let rng = make_range (Token $1) (Untyped $6) in
          (SrcApply((SrcLambda($2, $6), Range.dummy "let1"), $4), rng)
      }
  | LETREC OVAR DEFEQ xplet IN xplet {
        let rng = make_range (Token $1) (Untyped $6) in
          (SrcApply((SrcLambda($2, $6), Range.dummy "letrec1"), (SrcFixPoint($2, $4), Range.dummy "letrec2")), rng)
      }
  | UNBOX PVAR DEFEQ xplet IN xplet { (SrcUnbox($2, 0, $4, $6), make_range (Token $1) (Untyped $6)) }
  | UNBOX PVAR DEFEQ DOWNS xplet IN xplet {
        let (downi, _) = $4 in
          (SrcUnbox($2, downi, $5, $7), make_range (Token $1) (Untyped $7))
      }
  | xpif { $1 }
;
xpif:
  | IF xplet THEN xplet ELSE xplet { (SrcIfThenElse($2, $4, $6), make_range (Token $1) (Untyped $6)) }
  | xpfun                          { $1 }
;
xpfun:
  | LAMBDA OVAR DOT xplet { (SrcLambda($2, $4), make_range (Token $1) (Untyped $4)) }
  | FIX OVAR DOT xplet    { (SrcFixPoint($2, $4), make_range (Token $1) (Untyped $4)) }
  | xplor                 { $1 }
;
lorop:
  | LOR { ("||", $1) }
;
xplor:
  | xpland lorop xplor { binary_operator $1 $2 $3 }
  | xpland             { $1 }
;
landop:
  | LAND { ("&&", $1) }
;
xpland:
  | xprel landop xpland { binary_operator $1 $2 $3 }
  | xprel               { $1 }
;
relop:
  | EQUAL { ("==", $1) }  | GT { (">", $1) }  | LT { ("<", $1) }  | GEQ { (">=", $1) }  | LEQ { ("<=", $1) }
;
xprel:
  | xptimes relop xprel { binary_operator $1 $2 $3 }
  | xptimes             { $1 }
;
timesop:
  | TIMES { ("*", $1) }  | DIVIDES { ("/", $1) }
;
xptimes:
  | xpplus timesop xptimes { binary_operator $1 $2 $3 }
  | xpplus                 { $1 }
;
plusop:
  | PLUS { ("+", $1) }  | MINUS { ("-", $1) }
xpplus:
  | xpapp plusop xpplus { binary_operator $1 $2 $3 }
  | xpapp               { $1 }
;
xpapp:
  | xpapp xpbot { (SrcApply($1, $2), make_range (Untyped $1) (Untyped $2)) }
  | NEXT xpbot  { (SrcNext($2), make_range (Token $1) (Untyped $2)) }
  | PREV xpbot  { (SrcPrev($2), make_range (Token $1) (Untyped $2)) }
  | BOX xpbot   { (SrcBox($2), make_range (Token $1) (Untyped $2)) }
  | xpbot       { $1 }
;
binop:
  | lorop   { $1 }
  | landop  { $1 }
  | relop   { $1 }
  | timesop { $1 }
  | plusop  { $1 }
;
xpbot:
  | INTCONST              { let (numstr, rng) = $1 in (SrcIntConst(int_of_string numstr), rng) }
  | TRUE                  { (SrcBoolConst(true), $1) }
  | FALSE                 { (SrcBoolConst(false), $1) }
  | OVAR                  { let (ovnm, rng) = $1 in (SrcContentOf(ovnm), rng) }
  | PVAR                  { let (pvnm, rng) = $1 in (SrcContentOf(pvnm), rng) }
  | LPAREN binop RPAREN   { let (opnm, _) = $2 in (SrcContentOf(opnm), make_range (Token $1) (Token $3)) }
  | LPAREN xplet RPAREN   { let (utastmain, _) = $2 in (utastmain, make_range (Token $1) (Token $3)) }
;

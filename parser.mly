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
    let (oprng, opnm) = op in
    let rng = make_range (Untyped left) (Untyped right) in
      (UTApply((UTApply((UTContentOf(opnm), oprng), left), Range.dummy "lor"), right), rng)

%}

%token EOI
%token<Range.t * string> NUMCONST
%token<Range.t * Types.var_name> OVAR PVAR
%token<Range.t> LET LETREC IN IF THEN ELSE
%token<Range.t> LPAREN RPAREN DEFEQ LAMBDA FIX DOT
%token<Range.t> NEXT PREV BOX UNBOX
%token<Range.t * int> DOWNS
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
        let (_, ovnm) = $2 in
        let rng = make_range (Token $1) (Untyped $6) in
          (UTApply((UTLambda(ovnm, $6), Range.dummy "let1"), $4), rng)
      }
  | LETREC OVAR DEFEQ xplet IN xplet {
        let (_, ovnm) = $2 in
        let rng = make_range (Token $1) (Untyped $6) in
          (UTApply((UTLambda(ovnm, $6), Range.dummy "letrec1"), (UTFixPoint(ovnm, $4), Range.dummy "letrec2")), rng)
      }
  | UNBOX PVAR DEFEQ xplet IN xplet {
        let (_, pvnm) = $2 in
        let rng = make_range (Token $1) (Untyped $6) in
        (UTUnbox(pvnm, 0, $4, $6), rng)
      }
  | UNBOX PVAR DEFEQ DOWNS xplet IN xplet {
        let (_, pvnm) = $2 in
        let (_, downi) = $4 in
        let rng = make_range (Token $1) (Untyped $7) in
          (UTUnbox(pvnm, downi, $5, $7), rng)
      }
  | xpif { $1 }
;
xpif:
  | IF xplet THEN xplet ELSE xplet { (UTIfThenElse($2, $4, $6), make_range (Token $1) (Untyped $6)) }
  | xpfun                          { $1 }
;
xpfun:
  | LAMBDA OVAR DOT xplet { let (_, ovnm) = $2 in (UTLambda(ovnm, $4), make_range (Token $1) (Untyped $4)) }
  | FIX OVAR DOT xplet    { let (_, ovnm) = $2 in (UTFixPoint(ovnm, $4), make_range (Token $1) (Untyped $4)) }
  | xplor                 { $1 }
;
lorop:
  | LOR { ($1, "||") }
;
xplor:
  | xpland lorop xplor { binary_operator $1 $2 $3 }
  | xpland             { $1 }
;
landop:
  | LAND { ($1, "&&") }
;
xpland:
  | xprel landop xpland { binary_operator $1 $2 $3 }
  | xprel               { $1 }
;
relop:
  | EQUAL { ($1, "==") }  | GT { ($1, ">") }  | LT { ($1, "<") }  | GEQ { ($1, ">=") }  | LEQ { ($1, "<=") }
;
xprel:
  | xptimes relop xprel { binary_operator $1 $2 $3 }
  | xptimes             { $1 }
;
timesop:
  | TIMES { ($1, "*") }  | DIVIDES { ($1, "/") }
;
xptimes:
  | xpplus timesop xptimes { binary_operator $1 $2 $3 }
  | xpplus                 { $1 }
;
plusop:
  | PLUS { ($1, "+") }  | MINUS { ($1, "-") }
xpplus:
  | xpapp plusop xpplus { binary_operator $1 $2 $3 }
  | xpapp               { $1 }
;
xpapp:
  | xpapp xpbot { (UTApply($1, $2), make_range (Untyped $1) (Untyped $2)) }
  | NEXT xpbot  { (UTNext($2), make_range (Token $1) (Untyped $2)) }
  | PREV xpbot  { (UTPrev($2), make_range (Token $1) (Untyped $2)) }
  | BOX xpbot   { (UTBox($2), make_range (Token $1) (Untyped $2)) }
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
  | NUMCONST              { let (rng, numstr) = $1 in (UTNumConst(int_of_string numstr), rng) }
  | TRUE                  { (UTBoolConst(true), $1) }
  | FALSE                 { (UTBoolConst(false), $1) }
  | OVAR                  { let (rng, ovnm) = $1 in (UTContentOf(ovnm), rng) }
  | PVAR                  { let (rng, pvnm) = $1 in (UTContentOf(pvnm), rng) }
  | LPAREN binop RPAREN   { let (_, opnm) = $2 in (UTContentOf(opnm), make_range (Token $1) (Token $3)) }
  | LPAREN xplet RPAREN   { let (utastmain, _) = $2 in (utastmain, make_range (Token $1) (Token $3)) }
;

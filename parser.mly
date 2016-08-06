%{
  open Types
%}

%token<string> NUMCONST
%token<Types.var_name> VAR
%token LET LETREC IN IF THEN ELSE
%token LPAREN RPAREN DEFEQ LAMBDA FIX DOT EOI
%token NEXT PREV BOX
%token PLUS MINUS TIMES DIVIDES
%token EQUAL GT LT GEQ LEQ LAND LOR TRUE FALSE

%start main
%type<Types.untyped_tree> main

%%

main:
  | xplet EOI { $1 }
;
xplet:
  | LET VAR DEFEQ xplet IN xplet    { UTApply(UTLambda($2, $6), $4) }
  | LETREC VAR DEFEQ xplet IN xplet { UTApply(UTLambda($2, $6), UTFixPoint($2, $4)) }
  | xpif                            { $1 }
;
xpif:
  | IF xplet THEN xplet ELSE xplet { UTIfThenElse($2, $4, $6) }
  | xpfun                          { $1 }
;
xpfun:
  | LAMBDA VAR DOT xplet { UTLambda($2, $4) }
  | xplor                { $1 }
;
lorop:
  | LOR { "||" }
;
xplor:
  | xpland lorop xplor { UTApply(UTApply(UTContentOf($2), $1), $3) }
  | xpland             { $1 }
;
landop:
  | LAND { "&&" }
;
xpland:
  | xprel landop xpland { UTApply(UTApply(UTContentOf($2), $1), $3) }
  | xprel             { $1 }
;
relop:
  | EQUAL { "==" }  | GT { ">" }  | LT { "<" }  | GEQ { ">=" }  | LEQ { "<=" }
;
xprel:
  | xptimes relop xprel { UTApply(UTApply(UTContentOf($2), $1), $3) }
  | xptimes             { $1 }
;
timesop:
  | TIMES { "*" }  | DIVIDES { "/" }
;
xptimes:
  | xpplus timesop xptimes { UTApply(UTApply(UTContentOf($2), $1), $3) }
  | xpplus                 { $1 }
;
plusop:
  | PLUS { "+" }  | MINUS { "-" }
xpplus:
  | xpapp plusop xpplus { UTApply(UTApply(UTContentOf($2), $1), $3) }
  | xpapp              { $1 }
;
xpapp:
  | xpapp xpbot { UTApply($1, $2) }
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
  | NUMCONST              { UTNumConst(int_of_string $1) }
  | TRUE                  { UTBoolConst(true) }
  | FALSE                 { UTBoolConst(false) }
  | VAR                   { UTContentOf($1) }
  | LPAREN xplet RPAREN   { $2 }
  | LPAREN binop RPAREN   { UTContentOf($2) }
;

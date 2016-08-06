{
  open Parser

  exception Error of string
}

let space = [' ' '\t']
let break = ['\n']
let digit = ['0'-'9']
let capital = ['A'-'Z']
let small = ['a'-'z']
let latin = ( small | capital )
let identifier = (small (digit | latin | "-")*)

rule expr = parse
  | space { expr lexbuf }
  | break { expr lexbuf }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | "/"  { DIVIDES }
  | "."  { DOT }
  | "\\" { LAMBDA }
  | "="  { DEFEQ }
  | "==" { EQUAL }
  | ">"  { GT }
  | "<"  { LT }
  | ">=" { GEQ }
  | "<=" { LEQ }
  | "&&" { LAND }
  | "||" { LOR }
  | (digit+) { NUMCONST(Lexing.lexeme lexbuf) }
  | identifier {
        let tok = Lexing.lexeme lexbuf in
          match tok with
          | "let"    -> LET
          | "in"     -> IN
          | "letrec" -> LETREC
          | "fix"    -> FIX
          | "if"     -> IF
          | "then"   -> THEN
          | "else"   -> ELSE
          | "true"   -> TRUE
          | "false"  -> FALSE
          | "next"   -> NEXT
          | "prev"   -> PREV
          | "box"    -> BOX
          | other    -> VAR(other)
      }
  | eof { EOI }
  | _ as c { raise (Error("illegal token '" ^ (String.make 1 c) ^ "'")) }

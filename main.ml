open Types


let _ =
  if Array.length Sys.argv = 1 then
    print_endline "! no input."
  else
    try
      let filename_in = Sys.argv.(1) in
      let fin = open_in filename_in in
      begin
        Range.initialize_for_lexer () ;
        let sast = Parser.main Lexer.expr (Lexing.from_channel fin) in
        let ast = Typecheck.main sast in
        begin
          print_endline "  [SOURCE]" ;
          print_endline ("    " ^ (string_of_source_tree sast)) ;
          print_endline "  [AST]" ;
          print_endline ("    " ^ (string_of_abstract_tree ast)) ;
        end
      end
    with
    | Lexer.Error(s)      -> print_endline ("! [ERROR in LEXER] " ^ s)
    | Parsing.Parse_error -> print_endline ("! [ERROR in PARSER]")
    | Typecheck.Error(s)  -> print_endline ("! [ERROR in TYPECHECKER] " ^ s)

open Types


let _ =
  if Array.length Sys.argv = 1 then
    print_endline "! no input."
  else
    try
      let filename_in = Sys.argv.(1) in
      let fin = open_in filename_in in
      let utast = Parser.main Lexer.main (Lexing.from_channel fin) in
        print_endline (string_of_untyped_tree utast)
    with
    | Lexer.Error(s)      -> print_endline ("! [ERROR in LEXER] " ^ s)
    | Parsing.Parse_error -> print_endline ("! [ERROR in PARSER]")

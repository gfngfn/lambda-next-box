open Types

exception Error of string


let _ =
  try
    let need_eval_only =
      match Array.length Sys.argv with
      | 2 -> true
      | 3 -> false
      | _ -> raise (Error("wrong number of arguments"))
    in
      let filename_in = Sys.argv.(1) in
      let fin = open_in filename_in in
      begin
        Range.initialize_for_lexer () ;
        Evaluator.initialize () ;
        let sast = Parser.main Lexer.expr (Lexing.from_channel fin) in
        let ast = Typecheck.main sast in
        if need_eval_only then
          let res = Evaluator.main ast in
          begin
            print_endline "  [SOURCE]" ;
            print_endline ("    " ^ (string_of_source_tree sast)) ;
            print_endline "  [AST]" ;
            print_endline ("    " ^ (string_of_abstract_tree ast)) ;
            print_endline "  [EVAL]" ;
            print_endline ("    " ^ (string_of_abstract_tree res)) ;
          end
        else
          let tracelst = Evaluator.trace ast in
          let filename_out = Sys.argv.(2) in
          let fout = open_out filename_out in
          begin
            print_endline "  [SOURCE]" ;
            print_endline ("    " ^ (string_of_source_tree sast)) ;
            print_endline "  [AST]" ;
            print_endline ("    " ^ (string_of_abstract_tree ast)) ;
            Tohtml.main (fun s -> output_string fout s) tracelst ;
            close_out fout ;
            print_endline ("  output written on '" ^ filename_out ^ "'.")
          end
      end
    with
    | Error(s)            -> print_endline ("! [ERROR] " ^ s)
    | Lexer.Error(s)      -> print_endline ("! [ERROR in LEXER] " ^ s)
    | Parsing.Parse_error -> print_endline ("! [ERROR in PARSER] (cannot describe details, sorry.)")
    | Typecheck.Error(s)  -> print_endline ("! [ERROR in TYPECHECKER] " ^ s)

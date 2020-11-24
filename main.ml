let () =
  let _ =
    if Array.length Sys.argv <> 2 then
      (Printf.printf "Usage: imp <file>\n";
       exit 0) in
  let filename = Sys.argv.(1) in
  let lexbuf = 
    try 
      let opened = open_in filename in
      Lexing.from_channel opened
    with Lexer.Eof -> print_string "fail"; exit 0
  in
  let c =
    try Parser.p Lexer.token lexbuf
    with Parser.Error ->
      Printf.printf "Syntax error at line %d character %d\n"
        !Lexer.lineno
        (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1);
      exit 1 in
  Eval.print_val (fst (Eval.eval c (Eval.make_configuration c))); print_newline ()

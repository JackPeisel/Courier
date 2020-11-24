
module MenhirBasics = struct

  exception Error

  type token = 
    | VAR of (
# 8 "parser.mly"
       (string)
# 11 "parser.ml"
  )
    | TRUE
    | TIMES
    | THEN
    | STORE
    | STATE
    | SKIP
    | SET
    | SEMIC
    | RPAREN
    | REG of (
# 9 "parser.mly"
       (string)
# 25 "parser.ml"
  )
    | RBRACE
    | PRINT
    | PLUS
    | ON
    | NOTEQUALS
    | MINUS
    | LPAREN
    | LOAD
    | LET
    | LBRACE
    | INT of (
# 7 "parser.mly"
       (int)
# 40 "parser.ml"
  )
    | IF
    | FALSE
    | EQUALS
    | EOF
    | EMPTY
    | ELSE
    | DO
    | CONS
    | COMMA
    | CALL
    | ASSIGN

end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState50
  | MenhirState41
  | MenhirState37
  | MenhirState31
  | MenhirState27
  | MenhirState25
  | MenhirState19
  | MenhirState17
  | MenhirState11
  | MenhirState9
  | MenhirState8
  | MenhirState7
  | MenhirState5
  | MenhirState2
  | MenhirState0

# 1 "parser.mly"
  
open Ast

let make_reg x = (int_of_string(String.sub x 1 1))

# 91 "parser.ml"

let rec _menhir_goto_eList : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
  match _menhir_s with
  | MenhirState17 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e1 : (Ast.exp list)) = _v in
    let (((_menhir_stack, _menhir_s), _, (r1 : (Ast.var list))), (v1 : (
# 8 "parser.mly"
       (string)
# 103 "parser.ml"
        ))) = _menhir_stack in
    let _6 = () in
    let _4 = () in
    let _3 = () in
    let _1 = () in
    let _v : (Ast.exp) = 
# 64 "parser.mly"
                                                              (CallOn(r1, v1, e1))
# 112 "parser.ml"
         in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
  | MenhirState41 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e2 : (Ast.exp list)) = _v in
    let (_menhir_stack, _menhir_s, (e1 : (
# 25 "parser.mly"
      (Ast.exp)
# 122 "parser.ml"
        ))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.exp list) = 
# 45 "parser.mly"
                                  (e1::e2)
# 128 "parser.ml"
         in
    _menhir_goto_eList _menhir_env _menhir_stack _menhir_s _v
  | _ ->
    _menhir_fail ()

and _menhir_goto_binop : _menhir_env -> 'ttv_tail -> (
# 27 "parser.mly"
      (Ast.binop)
# 137 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
  let _menhir_stack = (_menhir_stack, _v) in
  let _menhir_stack = Obj.magic _menhir_stack in
  assert (not _menhir_env._menhir_error);
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | CALL ->
    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState37
  | DO ->
    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState37
  | FALSE ->
    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState37
  | IF ->
    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37
  | INT _v ->
    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
  | LET ->
    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
  | LPAREN ->
    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
  | REG _v ->
    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
  | SET ->
    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37
  | SKIP ->
    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37
  | TRUE ->
    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState37
  | VAR _v ->
    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
  | _ ->
    assert (not _menhir_env._menhir_error);
    _menhir_env._menhir_error <- true;
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_goto_returns : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.var list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  match _menhir_s with
  | MenhirState11 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s, (v1 : (
# 8 "parser.mly"
       (string)
# 184 "parser.ml"
        ))), _, (r1 : (Ast.var list))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.var list) = 
# 42 "parser.mly"
                                        (v1::r1)
# 190 "parser.ml"
         in
    _menhir_goto_returns _menhir_env _menhir_stack _menhir_s _v
  | MenhirState9 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | ASSIGN ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let _menhir_env = _menhir_discard _menhir_env in
       let _tok = _menhir_env._menhir_token in
       (match _tok with
        | CALL ->
          let _menhir_stack = Obj.magic _menhir_stack in
          let _menhir_env = _menhir_discard _menhir_env in
          let _tok = _menhir_env._menhir_token in
          (match _tok with
           | VAR _v ->
             let _menhir_stack = Obj.magic _menhir_stack in
             let _menhir_stack = (_menhir_stack, _v) in
             let _menhir_env = _menhir_discard _menhir_env in
             let _tok = _menhir_env._menhir_token in
             (match _tok with
              | ON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                 | CALL ->
                   _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                 | DO ->
                   _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                 | FALSE ->
                   _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                 | IF ->
                   _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                 | INT _v ->
                   _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
                 | LET ->
                   _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                 | LPAREN ->
                   _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                 | REG _v ->
                   _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
                 | SET ->
                   _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                 | SKIP ->
                   _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                 | TRUE ->
                   _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                 | VAR _v ->
                   _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
                 | _ ->
                   assert (not _menhir_env._menhir_error);
                   _menhir_env._menhir_error <- true;
                   _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
              | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
           | _ ->
             assert (not _menhir_env._menhir_error);
             _menhir_env._menhir_error <- true;
             let _menhir_stack = Obj.magic _menhir_stack in
             let (_menhir_stack, _menhir_s, _) = _menhir_stack in
             _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          let _menhir_stack = Obj.magic _menhir_stack in
          let (_menhir_stack, _menhir_s, _) = _menhir_stack in
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, _) = _menhir_stack in
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
  | _ ->
    _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
  Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
  assert false

and _menhir_goto_e : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 25 "parser.mly"
      (Ast.exp)
# 282 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  match _menhir_s with
  | MenhirState19 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | THEN ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let _menhir_env = _menhir_discard _menhir_env in
       let _tok = _menhir_env._menhir_token in
       (match _tok with
        | CALL ->
          _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | DO ->
          _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | FALSE ->
          _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | IF ->
          _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | INT _v ->
          _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | LET ->
          _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | LPAREN ->
          _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | REG _v ->
          _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | SET ->
          _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | SKIP ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | TRUE ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | VAR _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, _) = _menhir_stack in
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
  | MenhirState25 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | ELSE ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let _menhir_env = _menhir_discard _menhir_env in
       let _tok = _menhir_env._menhir_token in
       (match _tok with
        | CALL ->
          _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | DO ->
          _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | FALSE ->
          _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | IF ->
          _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | INT _v ->
          _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | LET ->
          _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | LPAREN ->
          _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | REG _v ->
          _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | SET ->
          _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | SKIP ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | TRUE ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | VAR _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
        | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, _) = _menhir_stack in
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
  | MenhirState27 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((((_menhir_stack, _menhir_s), _, (b1 : (
# 25 "parser.mly"
      (Ast.exp)
# 381 "parser.ml"
        ))), _, (e1 : (
# 25 "parser.mly"
      (Ast.exp)
# 385 "parser.ml"
        ))), _, (e2 : (
# 25 "parser.mly"
      (Ast.exp)
# 389 "parser.ml"
        ))) = _menhir_stack in
    let _5 = () in
    let _3 = () in
    let _1 = () in
    let _v : (Ast.exp) = 
# 61 "parser.mly"
                                        (If(b1, e1, e2))
# 397 "parser.ml"
         in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
  | MenhirState31 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _, (e2 : (
# 25 "parser.mly"
      (Ast.exp)
# 406 "parser.ml"
        ))) = _menhir_stack in
    let _2 = () in
    let _v : (
# 25 "parser.mly"
      (Ast.exp)
# 412 "parser.ml"
        ) = 
# 51 "parser.mly"
                          (Seq(e1, e2))
# 416 "parser.ml"
         in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
  | MenhirState41 | MenhirState17 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | COMMA ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let _menhir_env = _menhir_discard _menhir_env in
       let _tok = _menhir_env._menhir_token in
       (match _tok with
        | CALL ->
          _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | DO ->
          _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | FALSE ->
          _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | IF ->
          _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | INT _v ->
          _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | LET ->
          _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | LPAREN ->
          _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | REG _v ->
          _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | SET ->
          _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | SKIP ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | TRUE ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | VAR _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
     | ELSE | EOF | EQUALS | MINUS | NOTEQUALS | PLUS | RPAREN | SEMIC | THEN | TIMES ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, (e1 : (
# 25 "parser.mly"
      (Ast.exp)
# 462 "parser.ml"
            ))) = _menhir_stack in
       let _v : (Ast.exp list) = 
# 46 "parser.mly"
               ([e1])
# 467 "parser.ml"
             in
       _menhir_goto_eList _menhir_env _menhir_stack _menhir_s _v
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, _) = _menhir_stack in
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
  | MenhirState8 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | RPAREN ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let _menhir_env = _menhir_discard _menhir_env in
       let _menhir_stack = Obj.magic _menhir_stack in
       let ((_menhir_stack, _menhir_s), _, (e1 : (
# 25 "parser.mly"
      (Ast.exp)
# 488 "parser.ml"
            ))) = _menhir_stack in
       let _3 = () in
       let _1 = () in
       let _v : (Ast.exp) = 
# 68 "parser.mly"
                           (e1)
# 495 "parser.ml"
             in
       _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, _) = _menhir_stack in
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
  | MenhirState5 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s), _, (e1 : (
# 25 "parser.mly"
      (Ast.exp)
# 510 "parser.ml"
        ))) = _menhir_stack in
    let _1 = () in
    let _v : (Ast.exp) = 
# 59 "parser.mly"
               (Set(e1))
# 516 "parser.ml"
         in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
  | MenhirState0 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | EOF ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, (e1 : (
# 25 "parser.mly"
      (Ast.exp)
# 530 "parser.ml"
            ))) = _menhir_stack in
       let _2 = () in
       let _v : (
# 26 "parser.mly"
      (Ast.exp)
# 536 "parser.ml"
            ) = 
# 74 "parser.mly"
               (e1)
# 540 "parser.ml"
             in
       let _menhir_stack = Obj.magic _menhir_stack in
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_1 : (
# 26 "parser.mly"
      (Ast.exp)
# 547 "parser.ml"
            )) = _v in
       Obj.magic _1
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, _) = _menhir_stack in
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
  | _ ->
    _menhir_fail ()

and _menhir_run30 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
  let _menhir_env = _menhir_discard _menhir_env in
  let _menhir_stack = Obj.magic _menhir_stack in
  let _1 = () in
  let _v : (
# 27 "parser.mly"
      (Ast.binop)
# 567 "parser.ml"
    ) = 
# 39 "parser.mly"
              (Times)
# 571 "parser.ml"
     in
  _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
  let _menhir_env = _menhir_discard _menhir_env in
  let _menhir_stack = Obj.magic _menhir_stack in
  let _1 = () in
  let _v : (
# 27 "parser.mly"
      (Ast.binop)
# 583 "parser.ml"
    ) = 
# 35 "parser.mly"
              (Plus)
# 587 "parser.ml"
     in
  _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
  let _menhir_env = _menhir_discard _menhir_env in
  let _menhir_stack = Obj.magic _menhir_stack in
  let _1 = () in
  let _v : (
# 27 "parser.mly"
      (Ast.binop)
# 599 "parser.ml"
    ) = 
# 38 "parser.mly"
                  (NotEquals)
# 603 "parser.ml"
     in
  _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
  let _menhir_env = _menhir_discard _menhir_env in
  let _menhir_stack = Obj.magic _menhir_stack in
  let _1 = () in
  let _v : (
# 27 "parser.mly"
      (Ast.binop)
# 615 "parser.ml"
    ) = 
# 36 "parser.mly"
              (Minus)
# 619 "parser.ml"
     in
  _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
  let _menhir_env = _menhir_discard _menhir_env in
  let _menhir_stack = Obj.magic _menhir_stack in
  let _1 = () in
  let _v : (
# 27 "parser.mly"
      (Ast.binop)
# 631 "parser.ml"
    ) = 
# 37 "parser.mly"
               (Equals)
# 635 "parser.ml"
     in
  _menhir_goto_binop _menhir_env _menhir_stack _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (string)
# 642 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | COMMA ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | VAR _v ->
       _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
  | ASSIGN ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (v1 : (
# 8 "parser.mly"
       (string)
# 665 "parser.ml"
        ))) = _menhir_stack in
    let _v : (Ast.var list) = 
# 43 "parser.mly"
                   ([v1])
# 670 "parser.ml"
         in
    _menhir_goto_returns _menhir_env _menhir_stack _menhir_s _v
  | _ ->
    assert (not _menhir_env._menhir_error);
    _menhir_env._menhir_error <- true;
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_a : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  match _menhir_s with
  | MenhirState0 | MenhirState5 | MenhirState8 | MenhirState17 | MenhirState41 | MenhirState19 | MenhirState25 | MenhirState31 | MenhirState27 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | EQUALS ->
       _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
     | MINUS ->
       _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
     | NOTEQUALS ->
       _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
     | PLUS ->
       _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
     | SEMIC ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let _menhir_env = _menhir_discard _menhir_env in
       let _tok = _menhir_env._menhir_token in
       (match _tok with
        | CALL ->
          _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | DO ->
          _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | FALSE ->
          _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | IF ->
          _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | INT _v ->
          _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | LET ->
          _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | LPAREN ->
          _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | REG _v ->
          _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | SET ->
          _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | SKIP ->
          _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | TRUE ->
          _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | VAR _v ->
          _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | _ ->
          assert (not _menhir_env._menhir_error);
          _menhir_env._menhir_error <- true;
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
     | TIMES ->
       _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
     | COMMA | ELSE | EOF | RPAREN | THEN ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, (e1 : (Ast.exp))) = _menhir_stack in
       let _v : (
# 25 "parser.mly"
      (Ast.exp)
# 738 "parser.ml"
            ) = 
# 50 "parser.mly"
            (e1)
# 742 "parser.ml"
             in
       _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, _) = _menhir_stack in
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
  | MenhirState37 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | EQUALS ->
       _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
     | MINUS ->
       _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
     | NOTEQUALS ->
       _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
     | PLUS ->
       _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
     | TIMES ->
       _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
     | COMMA | ELSE | EOF | RPAREN | SEMIC | THEN ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let (((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), (b : (
# 27 "parser.mly"
      (Ast.binop)
# 771 "parser.ml"
            ))), _, (e2 : (Ast.exp))) = _menhir_stack in
       let _v : (Ast.exp) = 
# 69 "parser.mly"
                              (BinOp(b, e1, e2))
# 776 "parser.ml"
             in
       _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, _) = _menhir_stack in
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
  | MenhirState7 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | EQUALS ->
       _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
     | MINUS ->
       _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
     | NOTEQUALS ->
       _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
     | PLUS ->
       _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
     | TIMES ->
       _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
     | COMMA | ELSE | EOF | RPAREN | SEMIC | THEN ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let ((_menhir_stack, _menhir_s, (r1 : (
# 9 "parser.mly"
       (string)
# 805 "parser.ml"
            ))), _, (v1 : (Ast.exp))) = _menhir_stack in
       let _2 = () in
       let _v : (Ast.exp) = 
# 67 "parser.mly"
                            (Store(make_reg r1, v1))
# 811 "parser.ml"
             in
       _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, _) = _menhir_stack in
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
  | MenhirState2 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | EQUALS ->
       _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
     | MINUS ->
       _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
     | NOTEQUALS ->
       _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
     | PLUS ->
       _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
     | TIMES ->
       _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
     | COMMA | ELSE | EOF | RPAREN | SEMIC | THEN ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let ((_menhir_stack, _menhir_s, (v1 : (
# 8 "parser.mly"
       (string)
# 840 "parser.ml"
            ))), _, (c1 : (Ast.exp))) = _menhir_stack in
       let _2 = () in
       let _v : (Ast.exp) = 
# 63 "parser.mly"
                            (State(v1, c1))
# 846 "parser.ml"
             in
       _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, _) = _menhir_stack in
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
  | MenhirState50 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | EQUALS ->
       _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
     | MINUS ->
       _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
     | NOTEQUALS ->
       _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
     | PLUS ->
       _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
     | TIMES ->
       _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
     | COMMA | ELSE | EOF | RPAREN | SEMIC | THEN ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let ((_menhir_stack, _menhir_s, (v1 : (
# 8 "parser.mly"
       (string)
# 875 "parser.ml"
            ))), _, (e1 : (Ast.exp))) = _menhir_stack in
       let _2 = () in
       let _v : (Ast.exp) = 
# 62 "parser.mly"
                             (Assign(v1, e1))
# 881 "parser.ml"
             in
       _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, _) = _menhir_stack in
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
  | _ ->
    _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
  match _menhir_s with
  | MenhirState50 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState41 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState37 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState31 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState27 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState25 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState19 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState17 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState11 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState9 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState8 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState7 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState5 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState2 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
  | MenhirState0 ->
    let _menhir_stack = Obj.magic _menhir_stack in
    raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (string)
# 959 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | ASSIGN ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | CALL ->
       _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState50
     | DO ->
       _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState50
     | FALSE ->
       _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50
     | IF ->
       _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50
     | INT _v ->
       _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
     | LET ->
       _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50
     | LPAREN ->
       _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
     | REG _v ->
       _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
     | SET ->
       _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50
     | SKIP ->
       _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50
     | TRUE ->
       _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50
     | VAR _v ->
       _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
  | STATE ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | CALL ->
       _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState2
     | DO ->
       _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState2
     | FALSE ->
       _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState2
     | IF ->
       _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState2
     | INT _v ->
       _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
     | LET ->
       _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState2
     | LPAREN ->
       _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2
     | REG _v ->
       _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
     | SET ->
       _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
     | SKIP ->
       _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
     | TRUE ->
       _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
     | VAR _v ->
       _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
  | COMMA | ELSE | EOF | EQUALS | MINUS | NOTEQUALS | PLUS | RPAREN | SEMIC | THEN | TIMES ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (v1 : (
# 8 "parser.mly"
       (string)
# 1037 "parser.ml"
        ))) = _menhir_stack in
    let _v : (Ast.exp) = 
# 54 "parser.mly"
             (Var(v1))
# 1042 "parser.ml"
         in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
  | _ ->
    assert (not _menhir_env._menhir_error);
    _menhir_env._menhir_error <- true;
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_env = _menhir_discard _menhir_env in
  let _menhir_stack = Obj.magic _menhir_stack in
  let _1 = () in
  let _v : (Ast.exp) = 
# 56 "parser.mly"
         (True)
# 1060 "parser.ml"
     in
  _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_env = _menhir_discard _menhir_env in
  let _menhir_stack = Obj.magic _menhir_stack in
  let _1 = () in
  let _v : (Ast.exp) = 
# 60 "parser.mly"
         (Skip)
# 1072 "parser.ml"
     in
  _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_stack = (_menhir_stack, _menhir_s) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | CALL ->
    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState5
  | DO ->
    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState5
  | FALSE ->
    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState5
  | IF ->
    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState5
  | INT _v ->
    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
  | LET ->
    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5
  | LPAREN ->
    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
  | REG _v ->
    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
  | SET ->
    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
  | SKIP ->
    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
  | TRUE ->
    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
  | VAR _v ->
    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
  | _ ->
    assert (not _menhir_env._menhir_error);
    _menhir_env._menhir_error <- true;
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (string)
# 1114 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | LOAD ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | VAR _v ->
       let _menhir_stack = Obj.magic _menhir_stack in
       let _menhir_env = _menhir_discard _menhir_env in
       let _menhir_stack = Obj.magic _menhir_stack in
       let (v1 : (
# 8 "parser.mly"
       (string)
# 1133 "parser.ml"
            )) = _v in
       let (_menhir_stack, _menhir_s, (r1 : (
# 9 "parser.mly"
       (string)
# 1138 "parser.ml"
            ))) = _menhir_stack in
       let _2 = () in
       let _v : (Ast.exp) = 
# 66 "parser.mly"
                             (Load(v1, make_reg r1))
# 1144 "parser.ml"
             in
       _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       let _menhir_stack = Obj.magic _menhir_stack in
       let (_menhir_stack, _menhir_s, _) = _menhir_stack in
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
  | STORE ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    (match _tok with
     | CALL ->
       _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState7
     | DO ->
       _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState7
     | FALSE ->
       _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState7
     | IF ->
       _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState7
     | INT _v ->
       _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
     | LET ->
       _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
     | LPAREN ->
       _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
     | REG _v ->
       _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
     | SET ->
       _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
     | SKIP ->
       _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
     | TRUE ->
       _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
     | VAR _v ->
       _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
     | _ ->
       assert (not _menhir_env._menhir_error);
       _menhir_env._menhir_error <- true;
       _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
  | COMMA | ELSE | EOF | EQUALS | MINUS | NOTEQUALS | PLUS | RPAREN | SEMIC | THEN | TIMES ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (v1 : (
# 9 "parser.mly"
       (string)
# 1191 "parser.ml"
        ))) = _menhir_stack in
    let _v : (Ast.exp) = 
# 55 "parser.mly"
             (Reg(make_reg v1))
# 1196 "parser.ml"
         in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
  | _ ->
    assert (not _menhir_env._menhir_error);
    _menhir_env._menhir_error <- true;
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_stack = (_menhir_stack, _menhir_s) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | CALL ->
    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState8
  | DO ->
    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState8
  | FALSE ->
    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState8
  | IF ->
    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState8
  | INT _v ->
    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
  | LET ->
    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
  | LPAREN ->
    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
  | REG _v ->
    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
  | SET ->
    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8
  | SKIP ->
    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
  | TRUE ->
    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8
  | VAR _v ->
    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
  | _ ->
    assert (not _menhir_env._menhir_error);
    _menhir_env._menhir_error <- true;
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_stack = (_menhir_stack, _menhir_s) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | VAR _v ->
    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
  | _ ->
    assert (not _menhir_env._menhir_error);
    _menhir_env._menhir_error <- true;
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (int)
# 1257 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
  let _menhir_env = _menhir_discard _menhir_env in
  let _menhir_stack = Obj.magic _menhir_stack in
  let (i : (
# 7 "parser.mly"
       (int)
# 1265 "parser.ml"
    )) = _v in
  let _v : (Ast.exp) = 
# 53 "parser.mly"
            (Int(i))
# 1270 "parser.ml"
     in
  _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_stack = (_menhir_stack, _menhir_s) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | CALL ->
    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState19
  | DO ->
    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19
  | FALSE ->
    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
  | IF ->
    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
  | INT _v ->
    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
  | LET ->
    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19
  | LPAREN ->
    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19
  | REG _v ->
    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
  | SET ->
    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19
  | SKIP ->
    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
  | TRUE ->
    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19
  | VAR _v ->
    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
  | _ ->
    assert (not _menhir_env._menhir_error);
    _menhir_env._menhir_error <- true;
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_env = _menhir_discard _menhir_env in
  let _menhir_stack = Obj.magic _menhir_stack in
  let _1 = () in
  let _v : (Ast.exp) = 
# 57 "parser.mly"
          (False)
# 1317 "parser.ml"
     in
  _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_env = _menhir_discard _menhir_env in
  let _menhir_stack = Obj.magic _menhir_stack in
  let _1 = () in
  let _v : (Ast.exp) = 
# 58 "parser.mly"
       (Do)
# 1329 "parser.ml"
     in
  _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
  let _menhir_stack = (_menhir_stack, _menhir_s) in
  let _menhir_env = _menhir_discard _menhir_env in
  let _tok = _menhir_env._menhir_token in
  match _tok with
  | VAR _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (v1 : (
# 8 "parser.mly"
       (string)
# 1346 "parser.ml"
        )) = _v in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _1 = () in
    let _v : (Ast.exp) = 
# 65 "parser.mly"
                   (Call(v1))
# 1353 "parser.ml"
         in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
  | _ ->
    assert (not _menhir_env._menhir_error);
    _menhir_env._menhir_error <- true;
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
  let lexer = _menhir_env._menhir_lexer in
  let lexbuf = _menhir_env._menhir_lexbuf in
  let _tok = lexer lexbuf in
  {
    _menhir_lexer = lexer;
    _menhir_lexbuf = lexbuf;
    _menhir_token = _tok;
    _menhir_error = false;
  }

and p : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 26 "parser.mly"
      (Ast.exp)
# 1378 "parser.ml"
) =
  fun lexer lexbuf ->
  let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
  Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
             let _menhir_env = _menhir_discard _menhir_env in
             let _tok = _menhir_env._menhir_token in
             match _tok with
             | CALL ->
               _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState0
             | DO ->
               _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState0
             | FALSE ->
               _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState0
             | IF ->
               _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
             | INT _v ->
               _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
             | LET ->
               _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
             | LPAREN ->
               _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
             | REG _v ->
               _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
             | SET ->
               _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
             | SKIP ->
               _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
             | TRUE ->
               _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
             | VAR _v ->
               _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
             | _ ->
               assert (not _menhir_env._menhir_error);
               _menhir_env._menhir_error <- true;
               _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "/home/vagrant/.opam/4.08.1/lib/menhir/standard.mly"
  

# 1424 "parser.ml"

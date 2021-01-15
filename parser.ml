
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | VAR of (
# 50 "parser.mly"
       (string)
# 12 "parser.ml"
  )
    | TYPE of (
# 52 "parser.mly"
       (string)
# 17 "parser.ml"
  )
    | TRUE
    | TO
    | TL
    | TIMES
    | THEN
    | STRING of (
# 49 "parser.mly"
       (string)
# 27 "parser.ml"
  )
    | STORE
    | STATE
    | SKIP
    | SETT
    | SET
    | SEMIC
    | RPAREN
    | REMT
    | REG of (
# 51 "parser.mly"
       (string)
# 40 "parser.ml"
  )
    | RBRACK
    | RBRACE
    | PLUS
    | PERIOD
    | OR
    | ON
    | OF
    | OBJECT
    | NOTEQUALS
    | NOT
    | NEWTYPE
    | MINUS
    | MATCH
    | LT
    | LPAREN
    | LOAD
    | LIST
    | LINES
    | LET
    | LBRACK
    | LBRACE
    | INT of (
# 48 "parser.mly"
       (int)
# 66 "parser.ml"
  )
    | INSPECT
    | IMPORT
    | IF
    | HD
    | GT
    | FUN
    | FOR
    | FALSE
    | EQUALS
    | EOF
    | EMPTY
    | ELSE
    | DO
    | DEFAULT
    | CONS
    | COMMA
    | COLON
    | CARRY
    | CALL
    | ASSIGN
    | ARB
    | APPEND
    | AND
  
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
  | MenhirState202
  | MenhirState201
  | MenhirState200
  | MenhirState196
  | MenhirState193
  | MenhirState190
  | MenhirState186
  | MenhirState184
  | MenhirState181
  | MenhirState180
  | MenhirState179
  | MenhirState177
  | MenhirState176
  | MenhirState175
  | MenhirState173
  | MenhirState172
  | MenhirState164
  | MenhirState161
  | MenhirState160
  | MenhirState159
  | MenhirState156
  | MenhirState154
  | MenhirState153
  | MenhirState150
  | MenhirState149
  | MenhirState148
  | MenhirState147
  | MenhirState146
  | MenhirState145
  | MenhirState143
  | MenhirState142
  | MenhirState140
  | MenhirState138
  | MenhirState136
  | MenhirState135
  | MenhirState134
  | MenhirState133
  | MenhirState132
  | MenhirState131
  | MenhirState130
  | MenhirState128
  | MenhirState127
  | MenhirState126
  | MenhirState125
  | MenhirState124
  | MenhirState123
  | MenhirState122
  | MenhirState120
  | MenhirState119
  | MenhirState117
  | MenhirState116
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState111
  | MenhirState110
  | MenhirState108
  | MenhirState107
  | MenhirState106
  | MenhirState104
  | MenhirState103
  | MenhirState101
  | MenhirState100
  | MenhirState98
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState92
  | MenhirState89
  | MenhirState87
  | MenhirState83
  | MenhirState81
  | MenhirState80
  | MenhirState75
  | MenhirState73
  | MenhirState63
  | MenhirState62
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState53
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState45
  | MenhirState42
  | MenhirState40
  | MenhirState36
  | MenhirState34
  | MenhirState31
  | MenhirState28
  | MenhirState25
  | MenhirState23
  | MenhirState22
  | MenhirState21
  | MenhirState19
  | MenhirState15
  | MenhirState9
  | MenhirState8
  | MenhirState5
  | MenhirState2
  | MenhirState1
  | MenhirState0

# 1 "parser.mly"
  
open Ast

let make_reg x = (int_of_string(String.sub x 1 1))

let check_Str x =
  let y = String.length x in
  if y>5 then
    let z = String.sub x (y-5) 5 in 
    z = " List"
  else false

let check_Str2 x =
  let y = String.length x in
  if y>6 then
    let z = String.sub x (y-6) 6 in 
    z = " List."
  else false

let ret_Lst x =
  let y = String.length x in
  let z = String.sub x 0 (y - 5) in
  z


let rec make_type x = 
  match x with
  |"Int" -> TInt
  |"Bool" -> TBool
  |"Fun" -> TFun
  |"Var" -> TVar
  |"Null" -> NULLTYPE
  |"A'" -> A'
  | _ as a -> TCust a

let tuple_type ts =
  match ts with
  | [t] -> t
  | _ -> TTuple ts

let make_m m =
  List.map (fun (x, y) -> (make_type x, y)) m

let make_str x = String.sub x 1 (String.length x - 2 )


# 258 "parser.ml"

let rec _menhir_reduce35 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (t1 : (Ast.typ))) = _menhir_stack in
    let _v : (Ast.exp) = 
# 171 "parser.mly"
           (Type(t1))
# 266 "parser.ml"
     in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_obTList : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.var * Ast.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (v1 : (
# 50 "parser.mly"
       (string)
# 280 "parser.ml"
        ))), _, (t1 : (Ast.typ))), _, (e2 : ((Ast.var * Ast.typ) list))) = _menhir_stack in
        let _5 = () in
        let _4 = () in
        let _2 = () in
        let _v : ((Ast.var * Ast.typ) list) = 
# 120 "parser.mly"
                                                             ((v1, t1)::e2)
# 288 "parser.ml"
         in
        _menhir_goto_obTList _menhir_env _menhir_stack _menhir_s _v
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (t1 : (
# 52 "parser.mly"
       (string)
# 303 "parser.ml"
            ))), _, (o1 : ((Ast.var * Ast.typ) list))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 175 "parser.mly"
                                                             (TypeDef(t1,o1))
# 312 "parser.ml"
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

and _menhir_run12 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.typ))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (Ast.typ) = 
# 89 "parser.mly"
                                           ( t1 )
# 334 "parser.ml"
     in
    _menhir_goto_baseT _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (t1 : (Ast.typ))) = _menhir_stack in
    let _2 = () in
    let _v : (Ast.typ) = 
# 88 "parser.mly"
                                            ( TList(t1) )
# 347 "parser.ml"
     in
    _menhir_goto_baseT _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce83 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.typ list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (t1 : (Ast.typ list))) = _menhir_stack in
    let _v : (Ast.typ) = 
# 81 "parser.mly"
                                          ( tuple_type t1 )
# 357 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
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
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
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
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | AND | APPEND | ARB | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LT | MINUS | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TIMES | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (t1 : (Ast.typ))), _, (t2 : (Ast.typ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 150 "parser.mly"
                              (RemT(t1, t2))
# 534 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | AND | APPEND | ARB | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LT | MINUS | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (v1 : (
# 50 "parser.mly"
       (string)
# 577 "parser.ml"
            ))), _, (t1 : (Ast.typ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 178 "parser.mly"
                               (ObjectOf(v1, t1, []))
# 584 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | SEMIC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMIC ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR _v ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (v1 : (
# 50 "parser.mly"
       (string)
# 627 "parser.ml"
            ))), _, (t1 : (Ast.typ))) = _menhir_stack in
            let _2 = () in
            let _v : ((Ast.var * Ast.typ) list) = 
# 121 "parser.mly"
                                ([(v1,t1)])
# 633 "parser.ml"
             in
            _menhir_goto_obTList _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | AND | APPEND | ARB | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LT | MINUS | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TIMES | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (v1 : (
# 50 "parser.mly"
       (string)
# 732 "parser.ml"
            ))), _), _, (t1 : (Ast.typ))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.exp) = 
# 153 "parser.mly"
                            (Carry(v1,t1))
# 738 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | AND | APPEND | ARB | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LT | MINUS | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TIMES | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.typ))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.exp) = 
# 184 "parser.mly"
                    (Default t1)
# 761 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState2 | MenhirState5 | MenhirState21 | MenhirState22 | MenhirState28 | MenhirState36 | MenhirState179 | MenhirState49 | MenhirState173 | MenhirState164 | MenhirState161 | MenhirState159 | MenhirState56 | MenhirState154 | MenhirState57 | MenhirState149 | MenhirState62 | MenhirState146 | MenhirState75 | MenhirState143 | MenhirState140 | MenhirState138 | MenhirState80 | MenhirState135 | MenhirState132 | MenhirState130 | MenhirState125 | MenhirState127 | MenhirState81 | MenhirState122 | MenhirState119 | MenhirState116 | MenhirState114 | MenhirState83 | MenhirState111 | MenhirState108 | MenhirState106 | MenhirState103 | MenhirState100 | MenhirState97 | MenhirState95 | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | AND | APPEND | ARB | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LT | MINUS | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TIMES | WITH ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
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
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | AND | APPEND | ARB | COMMA | CONS | EQUALS | GT | LBRACK | LT | MINUS | NOTEQUALS | OR | PERIOD | PLUS | SEMIC | TIMES ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState181 | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState179
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179)
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LIST ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState193)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | AND | APPEND | ARB | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LT | MINUS | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t1 : (Ast.typ))) = _menhir_stack in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 179 "parser.mly"
                        (ObjectOf("",t1,[]))
# 917 "parser.ml"
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

and _menhir_goto_e : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 70 "parser.mly"
      (Ast.exp)
# 932 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState80 ->
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
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState138 ->
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
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState140
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (b1 : (
# 70 "parser.mly"
      (Ast.exp)
# 1095 "parser.ml"
        ))), _, (e1 : (
# 70 "parser.mly"
      (Ast.exp)
# 1099 "parser.ml"
        ))), _, (e2 : (
# 70 "parser.mly"
      (Ast.exp)
# 1103 "parser.ml"
        ))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.exp) = 
# 152 "parser.mly"
                                        (If(b1, e1, e2))
# 1111 "parser.ml"
         in
        _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _), _, (e2 : (
# 70 "parser.mly"
      (Ast.exp)
# 1120 "parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 70 "parser.mly"
      (Ast.exp)
# 1126 "parser.ml"
        ) = 
# 134 "parser.mly"
                          (Seq(e1, e2))
# 1130 "parser.ml"
         in
        _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, (e1 : (
# 70 "parser.mly"
      (Ast.exp)
# 1145 "parser.ml"
            ))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 143 "parser.mly"
                                    (Fun(e1))
# 1154 "parser.ml"
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
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (
# 70 "parser.mly"
      (Ast.exp)
# 1175 "parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 168 "parser.mly"
                           (e1)
# 1182 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e1 : (
# 70 "parser.mly"
      (Ast.exp)
# 1197 "parser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.exp) = 
# 148 "parser.mly"
               (Set(e1))
# 1203 "parser.ml"
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
# 70 "parser.mly"
      (Ast.exp)
# 1217 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 71 "parser.mly"
      (Ast.exp)
# 1223 "parser.ml"
            ) = 
# 191 "parser.mly"
               (e1)
# 1227 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 71 "parser.mly"
      (Ast.exp)
# 1234 "parser.ml"
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

and _menhir_goto_tupleT : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.typ list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState2 | MenhirState5 | MenhirState21 | MenhirState22 | MenhirState28 | MenhirState190 | MenhirState36 | MenhirState181 | MenhirState179 | MenhirState177 | MenhirState49 | MenhirState173 | MenhirState50 | MenhirState164 | MenhirState161 | MenhirState159 | MenhirState56 | MenhirState154 | MenhirState57 | MenhirState149 | MenhirState62 | MenhirState146 | MenhirState75 | MenhirState143 | MenhirState140 | MenhirState138 | MenhirState80 | MenhirState135 | MenhirState132 | MenhirState130 | MenhirState125 | MenhirState127 | MenhirState81 | MenhirState122 | MenhirState119 | MenhirState116 | MenhirState114 | MenhirState83 | MenhirState111 | MenhirState108 | MenhirState106 | MenhirState103 | MenhirState100 | MenhirState97 | MenhirState95 | MenhirState89 | MenhirState87 | MenhirState73 | MenhirState51 | MenhirState42 | MenhirState31 | MenhirState25 | MenhirState23 | MenhirState19 | MenhirState8 | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | APPEND | ARB | COLON | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LT | MINUS | NOT | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TIMES | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Ast.typ))), _, (t2 : (Ast.typ list))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.typ list) = 
# 83 "parser.mly"
                                                 (t1::t2)
# 1265 "parser.ml"
             in
            _menhir_goto_tupleT _menhir_env _menhir_stack _menhir_s _v
        | LIST ->
            _menhir_reduce83 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_returns : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.var list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (v1 : (
# 50 "parser.mly"
       (string)
# 1289 "parser.ml"
        ))), _, (r1 : (Ast.var list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.var list) = 
# 105 "parser.mly"
                                        (v1::r1)
# 1295 "parser.ml"
         in
        _menhir_goto_returns _menhir_env _menhir_stack _menhir_s _v
    | MenhirState51 ->
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
                | CALL ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | DEFAULT ->
                    _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | DO ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | EMPTY ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | FALSE ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | FOR ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | HD ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | IF ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | IMPORT ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | INSPECT ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | INT _v ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | LBRACE ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | LBRACK ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | LET ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | LPAREN ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | MATCH ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | NEWTYPE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | OBJECT ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | REG _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | REMT ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | SET ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | SETT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | SKIP ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | STRING _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | TL ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState159
                | TYPE _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | VAR _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159)
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

and _menhir_goto_binop : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 74 "parser.mly"
      (Ast.binop)
# 1391 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState202 | MenhirState201 | MenhirState200 | MenhirState196 | MenhirState184 | MenhirState176 | MenhirState180 | MenhirState172 | MenhirState175 | MenhirState160 | MenhirState156 | MenhirState153 | MenhirState150 | MenhirState147 | MenhirState145 | MenhirState142 | MenhirState136 | MenhirState131 | MenhirState133 | MenhirState128 | MenhirState124 | MenhirState113 | MenhirState115 | MenhirState123 | MenhirState117 | MenhirState120 | MenhirState92 | MenhirState110 | MenhirState96 | MenhirState98 | MenhirState101 | MenhirState107 | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | DEFAULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | DO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | EMPTY ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | FALSE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | FOR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | HD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | IF ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | IMPORT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | INSPECT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | INT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | LBRACE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LBRACK ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | LPAREN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MATCH ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | NEWTYPE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | OBJECT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | REG _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | REMT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | SET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | SETT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | TL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | DEFAULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | DO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | EMPTY ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | FALSE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | FOR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | HD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | IF ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | IMPORT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | INSPECT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | INT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | LBRACE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | LBRACK ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | LET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | LPAREN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | MATCH ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | NEWTYPE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | OBJECT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | REG _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | REMT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | SET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | SETT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | TL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState127
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | DEFAULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | DO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | EMPTY ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | FALSE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | FOR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | HD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | IF ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | IMPORT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | INSPECT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | INT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | LBRACE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LBRACK ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | LPAREN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | MATCH ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | NEWTYPE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | OBJECT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | REG _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | REMT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SETT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | TL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | DEFAULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | DO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | EMPTY ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | FALSE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | FOR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | HD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | IF ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | IMPORT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | INSPECT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | INT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | LBRACE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | LBRACK ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | LET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | LPAREN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | MATCH ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | NEWTYPE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | OBJECT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | REG _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | REMT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | SET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | SETT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | TL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState146
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146)
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | DEFAULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | DO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | EMPTY ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | FALSE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | FOR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | HD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | IF ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | IMPORT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | INSPECT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | INT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | LBRACE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LBRACK ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | LPAREN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | MATCH ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | NEWTYPE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | OBJECT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | REG _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | REMT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SETT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | TL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_obList : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.var * Ast.exp) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (o1 : ((Ast.var * Ast.exp) list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 174 "parser.mly"
                                (Object(o1))
# 1748 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, (v1 : (
# 50 "parser.mly"
       (string)
# 1763 "parser.ml"
        ))), _, (e1 : (Ast.exp))), _), _, (e2 : ((Ast.var * Ast.exp) list))) = _menhir_stack in
        let _5 = () in
        let _4 = () in
        let _2 = () in
        let _v : ((Ast.var * Ast.exp) list) = 
# 123 "parser.mly"
                                                           ((v1, e1)::e2)
# 1771 "parser.ml"
         in
        _menhir_goto_obList _menhir_env _menhir_stack _menhir_s _v
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (v1 : (
# 50 "parser.mly"
       (string)
# 1786 "parser.ml"
            ))), _, (t1 : (Ast.typ))), _, (o1 : ((Ast.var * Ast.exp) list))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 176 "parser.mly"
                                                                  (ObjectOf(v1, t1, o1))
# 1796 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState193 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (t1 : (Ast.typ))), _, (o1 : ((Ast.var * Ast.exp) list))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 177 "parser.mly"
                                                          (ObjectOf("",t1,o1))
# 1823 "parser.ml"
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

and _menhir_goto_mList : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.typ * Ast.exp) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e2 : ((Ast.typ * Ast.exp) list)) = _v in
        let (((_menhir_stack, _menhir_s, (t1 : (Ast.typ))), _, (e1 : (Ast.exp))), _) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : ((Ast.typ * Ast.exp) list) = 
# 117 "parser.mly"
                                                 ((t1, e1)::e2)
# 1848 "parser.ml"
         in
        _menhir_goto_mList _menhir_env _menhir_stack _menhir_s _v
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (m : ((Ast.typ * Ast.exp) list)) = _v in
        let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.exp))), _) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.exp) = 
# 170 "parser.mly"
                                   (Match(e1, m))
# 1861 "parser.ml"
         in
        _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_tupList : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.exp list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 164 "parser.mly"
                                 (Tup (e1))
# 1886 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _), _, (e2 : (Ast.exp list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.exp list) = 
# 114 "parser.mly"
                                      (e1::e2)
# 1903 "parser.ml"
         in
        _menhir_goto_tupList _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run173 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState173
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173

and _menhir_goto_listList : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.exp list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 165 "parser.mly"
                                  (List (e1))
# 1995 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _), _, (e2 : (Ast.exp list))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.exp list) = 
# 111 "parser.mly"
                                        (e1::e2)
# 2012 "parser.ml"
         in
        _menhir_goto_listList _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce67 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (e1 : (Ast.exp))) = _menhir_stack in
    let _v : (
# 70 "parser.mly"
      (Ast.exp)
# 2024 "parser.ml"
    ) = 
# 135 "parser.mly"
            (e1)
# 2028 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run143 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143

and _menhir_goto_ae : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 73 "parser.mly"
      (Ast.arbExp)
# 2102 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e1 : (
# 73 "parser.mly"
      (Ast.arbExp)
# 2110 "parser.ml"
    )) = _v in
    let _v : (Ast.exp) = 
# 186 "parser.mly"
            (ArbE(e1))
# 2115 "parser.ml"
     in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_pr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 72 "parser.mly"
      (Ast.prop)
# 2122 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | THEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | DEFAULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | DO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | EMPTY ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | FALSE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | FOR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | HD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | IF ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | IMPORT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | INSPECT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | INT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
        | LBRACE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | LBRACK ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | LET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | LPAREN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | MATCH ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | NEWTYPE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | OBJECT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | REG _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
        | REMT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | SET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | SETT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
        | TL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState130
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_eList : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e1 : (Ast.exp list)) = _v in
        let (((_menhir_stack, _menhir_s), _, (v1 : (Ast.exp))), _) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.exp) = 
# 158 "parser.mly"
                                 (CallOn([], v1, e1))
# 2215 "parser.ml"
         in
        _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e2 : (Ast.exp list)) = _v in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.exp list) = 
# 108 "parser.mly"
                                  (e1::e2)
# 2227 "parser.ml"
         in
        _menhir_goto_eList _menhir_env _menhir_stack _menhir_s _v
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e1 : (Ast.exp list)) = _v in
        let ((((_menhir_stack, _menhir_s), _, (r1 : (Ast.var list))), _, (v1 : (Ast.exp))), _) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.exp) = 
# 156 "parser.mly"
                                                            (CallOn(r1, v1, e1))
# 2242 "parser.ml"
         in
        _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
        | AND | APPEND | ARB | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LT | MINUS | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TIMES | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _), (v1 : (
# 50 "parser.mly"
       (string)
# 2330 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.exp) = 
# 172 "parser.mly"
                           (GetField(e1, v1))
# 2336 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run97 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run102 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (e1 : (Ast.exp))) = _menhir_stack in
    let _2 = () in
    let _v : (
# 73 "parser.mly"
      (Ast.arbExp)
# 2428 "parser.ml"
    ) = 
# 131 "parser.mly"
               (ArbV(e1))
# 2432 "parser.ml"
     in
    _menhir_goto_ae _menhir_env _menhir_stack _menhir_s _v

and _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_goto_baseT : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
    | AND | APPEND | ARB | COLON | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LIST | LT | MINUS | NOT | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TO | WITH ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (t1 : (Ast.typ))) = _menhir_stack in
        let _v : (Ast.typ list) = 
# 84 "parser.mly"
                                                ( [t1] )
# 2529 "parser.ml"
         in
        _menhir_goto_tupleT _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 50 "parser.mly"
       (string)
# 2542 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 50 "parser.mly"
       (string)
# 2572 "parser.ml"
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
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (v1 : (
# 50 "parser.mly"
       (string)
# 2595 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.var list) = 
# 106 "parser.mly"
                   ([v1])
# 2600 "parser.ml"
         in
        _menhir_goto_returns _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 50 "parser.mly"
       (string)
# 2613 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | DEFAULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | DO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | EMPTY ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | FALSE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | FOR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | HD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | IF ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | IMPORT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | INSPECT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | INT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | LBRACE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LBRACK ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LPAREN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | MATCH ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NEWTYPE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | OBJECT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | REG _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | REMT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SETT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | TL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce2 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 50 "parser.mly"
       (string)
# 2695 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (v1 : (
# 50 "parser.mly"
       (string)
# 2701 "parser.ml"
    ))) = _menhir_stack in
    let _v : (Ast.exp) = 
# 138 "parser.mly"
             (Var(v1))
# 2706 "parser.ml"
     in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 74 "parser.mly"
      (Ast.binop)
# 2718 "parser.ml"
    ) = 
# 97 "parser.mly"
              (Times)
# 2722 "parser.ml"
     in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 50 "parser.mly"
       (string)
# 2729 "parser.ml"
) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 74 "parser.mly"
      (Ast.binop)
# 2805 "parser.ml"
    ) = 
# 93 "parser.mly"
              (Plus)
# 2809 "parser.ml"
     in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 74 "parser.mly"
      (Ast.binop)
# 2821 "parser.ml"
    ) = 
# 102 "parser.mly"
           (Or)
# 2825 "parser.ml"
     in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 74 "parser.mly"
      (Ast.binop)
# 2837 "parser.ml"
    ) = 
# 96 "parser.mly"
                  (NotEquals)
# 2841 "parser.ml"
     in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 74 "parser.mly"
      (Ast.binop)
# 2853 "parser.ml"
    ) = 
# 94 "parser.mly"
              (Minus)
# 2857 "parser.ml"
     in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 74 "parser.mly"
      (Ast.binop)
# 2869 "parser.ml"
    ) = 
# 98 "parser.mly"
           (LT)
# 2873 "parser.ml"
     in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 74 "parser.mly"
      (Ast.binop)
# 2885 "parser.ml"
    ) = 
# 99 "parser.mly"
           (GT)
# 2889 "parser.ml"
     in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 74 "parser.mly"
      (Ast.binop)
# 2901 "parser.ml"
    ) = 
# 95 "parser.mly"
               (Equals)
# 2905 "parser.ml"
     in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 74 "parser.mly"
      (Ast.binop)
# 2917 "parser.ml"
    ) = 
# 100 "parser.mly"
              (Cons)
# 2921 "parser.ml"
     in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 50 "parser.mly"
       (string)
# 2928 "parser.ml"
) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 50 "parser.mly"
       (string)
# 2947 "parser.ml"
) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run105 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 74 "parser.mly"
      (Ast.binop)
# 3023 "parser.ml"
    ) = 
# 101 "parser.mly"
            (And)
# 3027 "parser.ml"
     in
    _menhir_goto_binop _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_a : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | ON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState92 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | COMMA | ELSE | EOF | LINES | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (v1 : (Ast.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.exp) = 
# 159 "parser.mly"
                 (Call(v1))
# 3142 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _), (v1 : (
# 50 "parser.mly"
       (string)
# 3187 "parser.ml"
            ))), _, (e2 : (Ast.exp))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.exp) = 
# 173 "parser.mly"
                                           (AssignField(e1, v1, e2))
# 3194 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | RBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState98 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | STORE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CALL ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | DEFAULT ->
                    _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | DO ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | EMPTY ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | FALSE ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | FOR ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | HD ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | IF ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | IMPORT ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | INSPECT ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | INT _v ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | LBRACE ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | LBRACK ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | LET ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | LPAREN ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | MATCH ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | NEWTYPE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | OBJECT ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | REG _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | REMT ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | SET ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | SETT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | SKIP ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | STRING _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | TL ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | TYPE _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | VAR _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
            | AND | APPEND | ARB | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LT | MINUS | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TIMES | WITH ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _), _, (e2 : (Ast.exp))), _) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _v : (Ast.exp) = 
# 167 "parser.mly"
                                   (Index (e1, e2))
# 3312 "parser.ml"
                 in
                _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _), _, (e2 : (Ast.exp))), _), _, (e3 : (Ast.exp))) = _menhir_stack in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.exp) = 
# 166 "parser.mly"
                                                  (ChangeIndex(e1, e2, e3))
# 3369 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _), _, (e2 : (Ast.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.exp) = 
# 185 "parser.mly"
                           (Append(e1,e2))
# 3416 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _, (b : (
# 74 "parser.mly"
      (Ast.binop)
# 3461 "parser.ml"
            ))), _, (e2 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 169 "parser.mly"
                              (BinOp(b, e1, e2))
# 3466 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | MenhirState161 | MenhirState111 | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState110 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e1 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp list) = 
# 109 "parser.mly"
               ([e1])
# 3579 "parser.ml"
             in
            _menhir_goto_eList _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SEMIC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState113 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState115 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CALL ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | DEFAULT ->
                    _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | DO ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | EMPTY ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | FALSE ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | FOR ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | HD ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | IF ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | IMPORT ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | INSPECT ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | INT _v ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | LBRACE ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | LBRACK ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | LET ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | LPAREN ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | MATCH ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | NEWTYPE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | OBJECT ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | REG _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | REMT ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | SET ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | SETT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | SKIP ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | STRING _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | TL ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                | TYPE _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | VAR _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMIC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState115 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState117 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CALL ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | DEFAULT ->
                    _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | DO ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | EMPTY ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | FALSE ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | FOR ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | HD ->
                    _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | IF ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | IMPORT ->
                    _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | INSPECT ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | INT _v ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | LBRACE ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | LBRACK ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | LET ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | LPAREN ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | MATCH ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | NEWTYPE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | OBJECT ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | REG _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | REMT ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | SET ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | SETT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | SKIP ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | STRING _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | TL ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState119
                | TYPE _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | VAR _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.exp))), _), _, (e2 : (Ast.exp))), _), _, (e4 : (Ast.exp))), _), _, (e3 : (Ast.exp))) = _menhir_stack in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 181 "parser.mly"
                                                                          (For(e1,e2,e3,e4))
# 4032 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Ast.exp))), _), _, (e2 : (Ast.exp))), _), _, (e3 : (Ast.exp))) = _menhir_stack in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 180 "parser.mly"
                                                           (For(e1,e2,e3,Skip))
# 4083 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.exp) = 
# 162 "parser.mly"
               (HD(e1))
# 4130 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (v1 : (
# 50 "parser.mly"
       (string)
# 4175 "parser.ml"
            ))), _, (b1 : (
# 74 "parser.mly"
      (Ast.binop)
# 4179 "parser.ml"
            ))), _, (a1 : (Ast.exp))) = _menhir_stack in
            let _v : (
# 72 "parser.mly"
      (Ast.prop)
# 4184 "parser.ml"
            ) = 
# 128 "parser.mly"
                                  (VB(v1,b1,a1))
# 4188 "parser.ml"
             in
            _menhir_goto_pr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState131 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState131
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _), _, (p1 : (
# 72 "parser.mly"
      (Ast.prop)
# 4337 "parser.ml"
            ))), _, (e2 : (Ast.exp))), _), _, (e3 : (Ast.exp))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 73 "parser.mly"
      (Ast.arbExp)
# 4346 "parser.ml"
            ) = 
# 132 "parser.mly"
                                                (ArbIf(p1,e2,e3))
# 4350 "parser.ml"
             in
            _menhir_goto_ae _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState134
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _, (b1 : (
# 74 "parser.mly"
      (Ast.binop)
# 4432 "parser.ml"
            ))), _, (a2 : (Ast.exp))) = _menhir_stack in
            let _v : (
# 72 "parser.mly"
      (Ast.prop)
# 4437 "parser.ml"
            ) = 
# 129 "parser.mly"
                                (EB(e1,b1,a2))
# 4441 "parser.ml"
             in
            _menhir_goto_pr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | MenhirState0 | MenhirState22 | MenhirState164 | MenhirState80 | MenhirState138 | MenhirState143 | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | SEMIC ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | THEN | WITH ->
            _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (v1 : (
# 50 "parser.mly"
       (string)
# 4527 "parser.ml"
            ))), _), _, (e1 : (Ast.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.exp) = 
# 154 "parser.mly"
                             (Assign(v1, e1))
# 4533 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (v1 : (
# 50 "parser.mly"
       (string)
# 4578 "parser.ml"
            ))), _, (b1 : (
# 74 "parser.mly"
      (Ast.binop)
# 4582 "parser.ml"
            ))), _, (e1 : (Ast.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.exp) = 
# 182 "parser.mly"
                                          (Inspect(VB(v1,b1,e1)))
# 4588 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState148
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.exp))), _, (b1 : (
# 74 "parser.mly"
      (Ast.binop)
# 4670 "parser.ml"
            ))), _, (e2 : (Ast.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.exp) = 
# 183 "parser.mly"
                                        (Inspect(EB(e1,b1,e2)))
# 4676 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
    | MenhirState154 | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState153 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState153
        | RBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e1 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp list) = 
# 112 "parser.mly"
                  ([e1])
# 4789 "parser.ml"
             in
            _menhir_goto_listList _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState156
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (t1 : (Ast.typ))), _, (e1 : (Ast.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 157 "parser.mly"
                            (SetInit(t1, e1))
# 4837 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156)
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | ON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState160 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState161
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | COMMA ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | SEMIC ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | RPAREN ->
            _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172)
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | COMMA ->
            _menhir_run173 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState175
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Ast.exp))), _), _, (e2 : (Ast.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.exp list) = 
# 115 "parser.mly"
                                ([e1; e2])
# 5033 "parser.ml"
             in
            _menhir_goto_tupList _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState176 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState177
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176)
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | LINES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState180 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState181
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181)
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState180
        | COMMA | ELSE | EOF | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Ast.typ))), _, (e1 : (Ast.exp))) = _menhir_stack in
            let _2 = () in
            let _v : ((Ast.typ * Ast.exp) list) = 
# 118 "parser.mly"
                              ([(t1,e1)])
# 5147 "parser.ml"
             in
            _menhir_goto_mList _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | SEMIC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState184 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMIC ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState184
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (v1 : (
# 50 "parser.mly"
       (string)
# 5216 "parser.ml"
            ))), _, (e1 : (Ast.exp))) = _menhir_stack in
            let _2 = () in
            let _v : ((Ast.var * Ast.exp) list) = 
# 124 "parser.mly"
                                ([(v1,e1)])
# 5222 "parser.ml"
             in
            _menhir_goto_obList _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState196
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (r1 : (
# 51 "parser.mly"
       (string)
# 5267 "parser.ml"
            ))), _, (v1 : (Ast.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.exp) = 
# 161 "parser.mly"
                            (Store(make_reg r1, v1))
# 5273 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState200
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (t1 : (Ast.typ))), _, (t2 : (Ast.typ))), _, (e1 : (Ast.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.exp) = 
# 149 "parser.mly"
                                            (SetT(t1, t2, e1))
# 5322 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState200)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState201
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.exp) = 
# 163 "parser.mly"
               (TL(e1))
# 5369 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState201)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | APPEND ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | ARB ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | LBRACK ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | PERIOD ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState202
        | COMMA | ELSE | EOF | LINES | ON | RBRACE | RBRACK | RPAREN | SEMIC | THEN | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (v1 : (
# 50 "parser.mly"
       (string)
# 5414 "parser.ml"
            ))), _), _, (c1 : (Ast.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.exp) = 
# 155 "parser.mly"
                            (State(v1, c1))
# 5420 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState202)
    | _ ->
        _menhir_fail ()

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState201 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState200 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState193 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
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
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 50 "parser.mly"
       (string)
# 5863 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | CARRY ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | STATE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | AND | APPEND | ARB | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LT | MINUS | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TIMES | WITH ->
        _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 52 "parser.mly"
       (string)
# 5886 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t1 : (
# 52 "parser.mly"
       (string)
# 5894 "parser.ml"
    )) = _v in
    let _v : (Ast.typ) = 
# 87 "parser.mly"
                                            ( make_type t1 )
# 5899 "parser.ml"
     in
    _menhir_goto_baseT _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.exp) = 
# 144 "parser.mly"
         (True)
# 5911 "parser.ml"
     in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 49 "parser.mly"
       (string)
# 5985 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s1 : (
# 49 "parser.mly"
       (string)
# 5993 "parser.ml"
    )) = _v in
    let _v : (Ast.exp) = 
# 139 "parser.mly"
                (String(make_str s1))
# 5998 "parser.ml"
     in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.exp) = 
# 151 "parser.mly"
         (Skip)
# 6010 "parser.ml"
     in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 51 "parser.mly"
       (string)
# 6114 "parser.ml"
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
# 50 "parser.mly"
       (string)
# 6133 "parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (r1 : (
# 51 "parser.mly"
       (string)
# 6138 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.exp) = 
# 160 "parser.mly"
                             (Load(v1, make_reg r1))
# 6144 "parser.ml"
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
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DEFAULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | EMPTY ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | FALSE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | FOR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | HD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | IF ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | IMPORT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | INSPECT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | INT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | LBRACE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LBRACK ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | LPAREN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | MATCH ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | NEWTYPE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | OBJECT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | REG _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | REMT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | SET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | SETT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | TL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | AND | APPEND | ARB | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LT | MINUS | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TIMES | WITH ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (v1 : (
# 51 "parser.mly"
       (string)
# 6223 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.exp) = 
# 140 "parser.mly"
             (Reg(make_reg v1))
# 6228 "parser.ml"
         in
        _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState190
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190)
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | OF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TYPE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR _v ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FUN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState50 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CALL ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | DEFAULT ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | DO ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | EMPTY ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | FALSE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | FOR ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | HD ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | IF ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | IMPORT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | INSPECT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | INT _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | LBRACE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | LBRACK ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | LET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | LPAREN ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | MATCH ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | NEWTYPE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | OBJECT ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | REG _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | REMT ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | SET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | SETT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | SKIP ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | STRING _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | TL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | TYPE _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | VAR _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 48 "parser.mly"
       (int)
# 6648 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 48 "parser.mly"
       (int)
# 6656 "parser.ml"
    )) = _v in
    let _v : (Ast.exp) = 
# 137 "parser.mly"
            (Int(i))
# 6661 "parser.ml"
     in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState62 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | ASSIGN ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | CARRY ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | CONS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | EQUALS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | GT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | LT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | NOTEQUALS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | OR ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | PLUS ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | STATE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | TIMES ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | APPEND | ARB | LBRACK | PERIOD ->
            _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STRING _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VAR _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (v1 : (
# 50 "parser.mly"
       (string)
# 6793 "parser.ml"
                )) = _v in
                let ((_menhir_stack, _menhir_s), (s1 : (
# 49 "parser.mly"
       (string)
# 6798 "parser.ml"
                ))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (Ast.exp) = 
# 142 "parser.mly"
                                      (Import(make_str s1, v1))
# 6805 "parser.ml"
                 in
                _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | AND | APPEND | ARB | COMMA | CONS | ELSE | EOF | EQUALS | GT | LBRACK | LINES | LT | MINUS | NOTEQUALS | ON | OR | PERIOD | PLUS | RBRACE | RBRACK | RPAREN | SEMIC | THEN | TIMES | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (s1 : (
# 49 "parser.mly"
       (string)
# 6819 "parser.ml"
            ))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.exp) = 
# 141 "parser.mly"
                        (let s2 = make_str s1 in Import(s2, s2))
# 6825 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARB ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState80 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | DEFAULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | DO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | EMPTY ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | FALSE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | FOR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | HD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | IF ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | IMPORT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | INSPECT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | INT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | LBRACE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LBRACK ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LPAREN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MATCH ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | NEWTYPE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | OBJECT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | REG _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | REMT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | SET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | SETT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | TL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | VAR _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState125 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AND ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | ASSIGN ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | CARRY ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | CONS ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | EQUALS ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | GT ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | MINUS ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | NOTEQUALS ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | OR ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | PLUS ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | STATE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | TIMES ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | APPEND | ARB | LBRACK | PERIOD ->
                _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CALL ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DEFAULT ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | EMPTY ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | FALSE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | FOR ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | HD ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | IF ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | IMPORT ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | INSPECT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | INT _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | LBRACE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LBRACK ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LPAREN ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MATCH ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | NEWTYPE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | OBJECT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | REG _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | REMT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SETT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SKIP ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | STRING _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | TL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TYPE _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.exp) = 
# 145 "parser.mly"
          (False)
# 7165 "parser.ml"
     in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.exp) = 
# 146 "parser.mly"
          (Empty)
# 7177 "parser.ml"
     in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.exp) = 
# 147 "parser.mly"
       (Do)
# 7189 "parser.ml"
     in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_run87 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run89 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CALL ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

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
# 71 "parser.mly"
      (Ast.exp)
# 7290 "parser.ml"
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
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEFAULT ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DO ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EMPTY ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FALSE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FOR ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | HD ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IMPORT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INSPECT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LBRACE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LBRACK ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MATCH ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEWTYPE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | OBJECT ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REG _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | REMT ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SETT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SKIP ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | TL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TYPE _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "/home/vagrant/.opam/4.08.1/lib/menhir/standard.mly"
  

# 7368 "parser.ml"

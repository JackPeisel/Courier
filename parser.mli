
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | TRUE
  | TIMES
  | THEN
  | STORE
  | STATE
  | SKIP
  | SET
  | SEMIC
  | RPAREN
  | REG of (string)
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
  | INT of (int)
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

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val p: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.exp)

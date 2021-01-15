
(* The type of tokens. *)

type token = 
  | WITH
  | VAR of (string)
  | TYPE of (string)
  | TRUE
  | TO
  | TL
  | TIMES
  | THEN
  | STRING of (string)
  | STORE
  | STATE
  | SKIP
  | SETT
  | SET
  | SEMIC
  | RPAREN
  | REMT
  | REG of (string)
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
  | INT of (int)
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

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val p: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.exp)

{
open Parser
open Printf
exception Eof
exception LexingError of string

let lineno = ref 1
let linestart = ref (-1)

let newline lexbuf : unit =
  linestart := Lexing.lexeme_start lexbuf;
  incr lineno

let info lexbuf =
  let c1 = Lexing.lexeme_start lexbuf in
  let c2 = Lexing.lexeme_end lexbuf in
  let l = !lineno in
  let c = !linestart + 1 in
    ((l, c1 - c),(l, c2 - c - 1))

let error lexbuf msg =
  let i = info lexbuf in
  let t = Lexing.lexeme lexbuf in
  let ((l1,c1),(l2,c2)) = i in
  let s =
    if l2=l1
    then Printf.sprintf "line %d, characters %d-%d" l1 c1 c2
    else Printf.sprintf "line %d, character %d, to line %d, character %d" l1 c1 l2 c2 in
  let err = Printf.sprintf "%s: lexing error %s at %s."
    s
    msg
    t in
  raise (LexingError err)
}


let int = ['-']?['0'-'9']
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
let reg = ['R'] ['0'-'9']
let white = [' ' '\t']

rule token = parse
  | white   {token lexbuf}
  | '\n'    { newline lexbuf; token lexbuf }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "+"     { PLUS }
  | "-"     {MINUS}
  | "*"     {TIMES}
  | "("     {LPAREN}
  | ")"     {RPAREN}
  | "{"     {LBRACE}
  | "}"     {RBRACE}
  | ":="    {ASSIGN}
  | "=="    {EQUALS}
  | "!="    {NOTEQUALS}
  | "::"    {CONS}
  | "hd"    {HD}
  | "tl"    {TL}
  | "[]"    {EMPTY}
  | ";"     {SEMIC}
  | "if"    {IF}
  | "then"  {THEN}
  | "else"  {ELSE}
  | "skip"  {SKIP}
  | id      { VAR (Lexing.lexeme lexbuf) }
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | reg     { REG (Lexing.lexeme lexbuf) }
  | "Call"  {CALL}
  | "States" {STATE}
  | "<<"    {STORE}
  | ">>"    {LOAD}
  | "Set"   {SET}
  | "Do"    {DO}
  | ","     {COMMA}
  | "Let"   {LET}
  | "ON"    {ON}
  | eof     {EOF}
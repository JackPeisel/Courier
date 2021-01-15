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
let typ = ['A'-'Z'] ['a'-'z' '0'-'9']*
let typList = typ (" List" | " List.")
let multiList = typList* (" List" | " List.")
let typeStar = (typ '*') | (typList '*') | (multiList '*')
let typestarmore= typeStar | (typeStar '*')
let multiStar = typestarmore+ (typ | typList | multiList |typestarmore)
let reg = ['R'] ['0'-'9']
let white = [' ' '\t']
let md = ['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | ['_'] | ['-'] | ['?'] | ['!'] | ['+'] | ['-'] | ['/']
let st = '"' md* '"'

rule token = parse
  | white   {token lexbuf}
  | '\n'    { newline lexbuf; token lexbuf }
  | '''     {ARB}
  | st {STRING(Lexing.lexeme lexbuf)}
  | "true"  { TRUE }
  | "false" { FALSE }
  | "import" {IMPORT}
  | "Empty" {EMPTY}
  | "+"     { PLUS }
  | "-"     {MINUS}
  | "*"     {TIMES}
  | "&&"   {AND}
  | "||"    {OR}
  | "|"     {LINES}
  | "("     {LPAREN}
  | ")"     {RPAREN}
  | "{"     {LBRACE}
  | "}"     {RBRACE}
  | "["     {LBRACK}
  | "]"     {RBRACK}
  | ":="    {ASSIGN}
  | "=="    {EQUALS}
  | "!="    {NOTEQUALS}
  | "!"     {NOT}
  | "To"    {TO}
  | "Default"  {DEFAULT}
  | "@"     {APPEND}
  | "::"    {CONS}
  | "hd"    {HD}
  | "tl"    {TL}
  | "[]"    {EMPTY}
  | ";"     {SEMIC}
  | "if"    {IF}
  | "then"  {THEN}
  | "else"  {ELSE}
  | "skip"  {SKIP}
  | "match" {MATCH}
  | "with"  {WITH}
  | "of"    {OF}
  | "fun"   {FUN}
  | "for"   {FOR}
  | id      { VAR (Lexing.lexeme lexbuf) }
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | reg     { REG (Lexing.lexeme lexbuf) }
  | "Call"  {CALL}
  | "States" {STATE}
  | "Carries" {CARRY}
  | "Inspect" {INSPECT}
  | "<<"    {STORE}
  | ">>"    {LOAD}
  | ">"     {GT}
  | "<"     {LT}
  | "SetT"  {SETT}
  | "RemT"  {REMT}
  | "->"    {TO}
  | "Object" {OBJECT}
  | "Type"  {NEWTYPE}
  | "Set"   {SET}
  | "Do"    {DO}
  | ","     {COMMA}
  | "."     {PERIOD}
  | "Let"   {LET}
  | "ON"    {ON}
  | ":"     {COLON}
  | "List"  {LIST}
  | "A'"    {TYPE (Lexing.lexeme lexbuf) }
  | typ     {TYPE (Lexing.lexeme lexbuf) }
  | eof     {EOF}
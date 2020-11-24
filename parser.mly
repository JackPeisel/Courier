%{
open Ast

let make_reg x = (int_of_string(String.sub x 1 1))
%}

%token <int> INT
%token <string> VAR
%token <string> REG
%token SEMIC
%token PLUS MINUS TIMES
  LPAREN RPAREN TRUE FALSE
  EQUALS NOTEQUALS
  SKIP ASSIGN IF THEN ELSE STATE CALL STORE LOAD DO SET LET ON
  CONS HD TL EMPTY
  LBRACE RBRACE
  PRINT
%token COMMA
%token EOF

%nonassoc SEMIC

%right ASSIGN

%type <Ast.exp> e
%type <Ast.exp> p
%type <Ast.binop> binop

%start p

%%



binop : PLUS  {Plus}
      | MINUS {Minus}
      | EQUALS {Equals}
      | NOTEQUALS {NotEquals}
      | TIMES {Times}


returns : v1 = VAR; COMMA; r1 = returns {v1::r1}
        | v1 = VAR {[v1]}

eList : e1 = e; COMMA; e2 = eList {e1::e2}
      | e1 = e {[e1]}

/* Commands */

e : e1 = a  {e1}
  | e1 = a; SEMIC; e2 = e {Seq(e1, e2)}

a : i = INT {Int(i)}
  | v1 = VAR {Var(v1)}
  | v1 = REG {Reg(make_reg v1)}
  | TRUE {True}
  | FALSE {False}
  | DO {Do}
  | SET; e1= e {Set(e1)}
  | SKIP {Skip}
  | IF b1 = e; THEN e1 = e; ELSE e2 = e {If(b1, e1, e2)}
  | v1 = VAR; ASSIGN; e1 = a {Assign(v1, e1)}
  | v1 = VAR; STATE; c1 = a {State(v1, c1)}
  | LET; r1 = returns; ASSIGN; CALL; v1 = VAR; ON; e1 = eList {CallOn(r1, v1, e1)}
  | CALL; v1 = VAR {Call(v1)}
  | r1 = REG; LOAD; v1 = VAR {Load(v1, make_reg r1)}
  | r1 = REG; STORE; v1 = a {Store(make_reg r1, v1)}
  | HD; e1 = a {HD(e1)}
  | TL; e1 = a {TL(e1)}
  | LPAREN; e1 = e; RPAREN {e1}
  | e1 = a; b = binop; e2 = a {BinOp(b, e1, e2)}


/* Programs */

p : e1= e; EOF {e1}

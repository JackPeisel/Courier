%{
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

%}

%token <int> INT
%token <string> STRING
%token <string> VAR
%token <string> REG
%token <string> TYPE
%token PLUS MINUS TIMES
  LPAREN RPAREN TRUE FALSE
  EQUALS NOTEQUALS NOT LT GT AND OR
  SEMIC SKIP ASSIGN IF THEN ELSE STATE CALL STORE LOAD DO SET LET ON SETT REMT TO CARRY INSPECT DEFAULT ARB
  CONS HD TL EMPTY MATCH WITH LINES COLON FUN FOR APPEND
  OBJECT NEWTYPE OF PERIOD
  LBRACE RBRACE
  LBRACK RBRACK
  IMPORT
  LIST
%token COMMA
%token EOF

%nonassoc SEMIC

%right ASSIGN

%type <Ast.exp> e
%type <Ast.exp> p
%type <Ast.prop> pr
%type <Ast.arbExp> ae
%type <Ast.binop> binop

%start p

%%


t: t1 = tupleT                            { tuple_type t1 }

tupleT: t1 = baseT; TIMES; t2 = tupleT           {t1::t2}
      | t1 = baseT                              { [t1] }

baseT:
    | t1 = TYPE                             { make_type t1 }
    | t1 = t; LIST                          { TList(t1) } 
    | LPAREN; t1=t; RPAREN                 { t1 }               



binop : PLUS  {Plus}
      | MINUS {Minus}
      | EQUALS {Equals}
      | NOTEQUALS {NotEquals}
      | TIMES {Times}
      | LT {LT}
      | GT {GT}
      | CONS  {Cons}
      | AND {And}
      | OR {Or}


returns : v1 = VAR; COMMA; r1 = returns {v1::r1}
        | v1 = VAR {[v1]}

eList : e1 = a; COMMA; e2 = eList {e1::e2}
      | e1 = a {[e1]}

listList : e1 = a; COMMA; e2 = listList {e1::e2}
         | e1 = a {[e1]}

tupList : e1 = a; COMMA; e2 = tupList {e1::e2}
        | e1 = a; COMMA; e2 = a {[e1; e2]}

mList : t1 = t; COLON; e1 = a; LINES; e2 = mList {(t1, e1)::e2}
      | t1 = t; COLON; e1 = a {[(t1,e1)]}

obTList : v1 = VAR; COLON; t1 = t; SEMIC;SEMIC; e2 = obTList {(v1, t1)::e2}
      | v1 = VAR; COLON; t1 = t {[(v1,t1)]}

obList : v1 = VAR; COLON; e1 = a; SEMIC;SEMIC; e2 = obList {(v1, e1)::e2}
      | v1 = VAR; COLON; e1 = a {[(v1,e1)]}

/* Commands */

pr : v1 = VAR; b1 = binop; a1 = a {VB(v1,b1,a1)}
   | e1 = a; b1 = binop; a2 = a {EB(e1,b1,a2)}

ae : e1=a; ARB {ArbV(e1)}
   | IF; ARB; p1 = pr; THEN; e2 = a; ELSE; e3=a {ArbIf(p1,e2,e3)}

e : e1 = a; SEMIC; e2 = e {Seq(e1, e2)}
  | e1 = a  {e1}

a : i = INT {Int(i)}
  | v1 = VAR {Var(v1)}
  | s1 = STRING {String(make_str s1)}
  | v1 = REG {Reg(make_reg v1)}
  | IMPORT; s1 = STRING {let s2 = make_str s1 in Import(s2, s2)}
  | IMPORT; s1 = STRING; TO; v1 = VAR {Import(make_str s1, v1)}
  | LPAREN; FUN; TO; e1 = e; RPAREN {Fun(e1)} 
  | TRUE {True}
  | FALSE {False}
  | EMPTY {Empty}
  | DO {Do}
  | SET; e1= e {Set(e1)}
  | SETT; t1 = t; NOT; t2 = t; TO; e1 = a   {SetT(t1, t2, e1)}
  | REMT; t1 = t; NOT; t2 = t {RemT(t1, t2)}
  | SKIP {Skip}
  | IF b1 = e; THEN e1 = e; ELSE e2 = e {If(b1, e1, e2)}
  | v1 = VAR; CARRY; t1 = t {Carry(v1,t1)}
  | v1 = VAR; ASSIGN; e1 = a {Assign(v1, e1)}
  | v1 = VAR; STATE; c1 = a {State(v1, c1)}
  | LET; r1 = returns; ASSIGN; CALL; v1 = a; ON; e1 = eList {CallOn(r1, v1, e1)}
  | LET; t1 = t; TO; e1 = a {SetInit(t1, e1)}
  | CALL; v1 = a; ON; e1 = eList {CallOn([], v1, e1)}
  | CALL; v1 = a {Call(v1)}
  | r1 = REG; LOAD; v1 = VAR {Load(v1, make_reg r1)}
  | r1 = REG; STORE; v1 = a {Store(make_reg r1, v1)}
  | HD; e1 = a {HD(e1)}
  | TL; e1 = a {TL(e1)}
  | LPAREN; e1 = tupList; RPAREN {Tup (e1)}
  | LBRACK; e1 = listList; RBRACK {List (e1)}
  | e1 = a; LBRACK; e2 = a; RBRACK; STORE; e3 = a {ChangeIndex(e1, e2, e3)}
  | e1 = a; LBRACK; e2 = a; RBRACK {Index (e1, e2)} 
  | LPAREN; e1 = e; RPAREN {e1}
  | e1 = a; b = binop; e2 = a {BinOp(b, e1, e2)}
  | MATCH; e1 = a; WITH; m = mList {Match(e1, m)}
  | t1 = t {Type(t1)}
  | e1 = a; PERIOD; v1=VAR {GetField(e1, v1)}
  | e1 = a; PERIOD; v1=VAR; ASSIGN; e2 = a {AssignField(e1, v1, e2)}
  | LBRACE; o1 = obList; RBRACE {Object(o1)}
  | NEWTYPE; t1 = TYPE; ASSIGN; LBRACE; o1 = obTList; RBRACE {TypeDef(t1,o1)} 
  | OBJECT; v1=VAR; OF; t1 = t; WITH; LBRACE; o1 = obList; RBRACE {ObjectOf(v1, t1, o1)}
  | OBJECT; OF; t1 = t; WITH; LBRACE; o1 = obList; RBRACE {ObjectOf("",t1,o1)}
  | OBJECT; v1=VAR; OF; t1 = t {ObjectOf(v1, t1, [])}
  | OBJECT; OF; t1 = t  {ObjectOf("",t1,[])}
  | FOR; LPAREN; e1 = a; SEMIC; e2 = a; RPAREN; TO; e3 = a {For(e1,e2,e3,Skip)}
  | FOR; LPAREN; e1 = a; SEMIC; e2 = a; SEMIC; e4 = a; RPAREN; TO; e3 = a {For(e1,e2,e3,e4)}
  | INSPECT; v1 = VAR; b1 = binop; e1 = a {Inspect(VB(v1,b1,e1))}
  | INSPECT; e1 = a; b1 = binop; e2 = a {Inspect(EB(e1,b1,e2))}
  | DEFAULT; t1 = t {Default t1}
  | e1 = a; APPEND; e2 = a {Append(e1,e2)}
  | e1 = ae {ArbE(e1)}


/* Programs */

p : e1= e; EOF {e1}

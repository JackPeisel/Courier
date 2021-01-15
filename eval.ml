open Ast

exception NoCase of string
exception ArbError of string

type store = (var * value) list

type register = (int * value)

type reg_file = register list

type call_env = (var * exp) list

type type_case = ((typ * typ) * exp) list

type types = (var * type_spec) list


type state = {
  s: store;
  reg_file: register list;
  domain: call_env;
  func: exp;
  cases: type_case;
  types: types

}

let string_binop (b:binop) =
  match b with
  |Plus -> "Plus"
  |Minus -> "Minus"
  |Times -> "Times"
  |Equals -> "Equals"
  |NotEquals -> "NotEquals"
  |Cons -> "Cons"
  |LT -> "Less than"
  |GT -> "Greater than"
  |And -> "And"
  |Or -> "Or"

let rec string_typ (t:typ) =
  match t with
  |TList a -> (string_typ a) ^ " List"
  |TInt -> "Int"
  |TBool -> "Bool"
  |TCust x -> x
  |TFun -> "Function"
  |NULLTYPE-> "Null"
  |TString -> "String"
  |A' -> "Any"
  |TTuple tList->"(Tuple of types: "^(string_typ (List.hd tList))^(List.fold_left (fun s t -> s ^", "^ (string_typ t)) "" (List.tl tList) )^")"
  |_ -> "Haven't bothered yet"

let rec string_exp (e : exp)= 
  match e with
  |Int i -> ("Int:"^(string_of_int i))
  |Reg i -> ("Reg:"^(string_of_int i))
  |True -> "True"
  |False ->"False"
  |Var x -> ("Var: "^x)
  |Skip ->  "Skip"
  |Empty ->  "Empty"
  |Assign (x, e1) -> "Assign "^x^ "to" ^"("^(string_exp e1)^")"
  |Store (r1, e2) -> "Store "^(string_of_int r1)^ " with ("^(string_exp e2)^")"
  |Load (x, r1) -> "Load "^(string_of_int r1) ^ " into "^x
  |Seq (e1, e2) -> "Sequence of "^(string_exp e1)^" and ("^(string_exp e2)^")"
  |If (bEx, e1, e2) -> "If "^(string_exp bEx)^"then execute "^(string_exp e1)^"else execute "^(string_exp e2)
  |State (x, e) -> "State "^x^" to be ("^(string_exp e)^")"
  |Call e -> "Call the function "^ (string_exp e)
  |Set e -> "Set the current function to "^(string_exp e)
  |Do -> "Execute the current function"
  |BinOp(b, e1, e2) -> (string_binop b)^" of ("^(string_exp e1)^" and ("^(string_exp e2)^")"
  |CallOn(vars,f,args) -> begin "Call the function "^(string_exp f)^ " on "^
                                (string_exp (List.hd args))^(List.fold_left (fun s e->(s^" and "^(string_exp e))) "" (List.tl args))^
                                (" and store into variables: ")^(List.hd vars)^(List.fold_left (fun a s -> a^" and "^s) "" (List.tl vars))
    end
  |HD e1 -> "Head of "^(string_exp e1)
  |TL e1 -> "Tail of "^(string_exp e1)
  |Match (e1, elist) -> "Match "^(string_exp e1)^ " with: "^
                        ((List.fold_left (fun s (t,e)->(s^"Type: "^(string_typ t)^", exp: "^(string_exp e))) "" elist))
  |Tup eList -> "Tuple of: "^(string_exp (List.hd eList))^
                (List.fold_left (fun s e->(s^" and "^(string_exp e))) "" (List.tl eList))
  |Index (e, index) -> "Index "^(string_exp index)^" of "^(string_exp e)
  |List eList -> "List of head:"^(string_exp (List.hd eList))^
                 ((List.fold_left (fun s e->(s^" and "^(string_exp e))) "" (List.tl eList)))
  |SetT (typ1, typ2, e) -> "Set type handler for "^(string_typ typ1)^" and "^(string_typ typ2) ^" to "^(string_exp e)
  |RemT (typ1, typ2) -> "Remove type handler for "^(string_typ typ1)^" and "^(string_typ typ2)
  |Type t -> string_typ t
  |Fun e -> "Function: "^string_exp e
  |Object lst -> "Object with k/v pairs: "^(List.fold_left (fun s (x,e)->(s^"Var:"^x^", exp: "^(string_exp e))) "" lst)
  |TypeDef (var, lst) -> "Define type of name:"^var^" and fields: "^
                         (List.fold_left (fun s (x,e)->(s^"Var:"^x^", of type: "^(string_typ e))) "" lst)
  |ObjectOf (var,typ, lst) -> "Object "^var^" of type "^(string_typ typ)^
                              " and fields "^(List.fold_left (fun s (x,e)->(s^"Field:"^x^", Value: "^(string_exp e))) "" lst)
  |GetField (e,x) -> "Get field "^x^" of "^(string_exp e)
  |AssignField (e1, x, e2) -> "Assign field "^x^" of "^(string_exp e1)^" to "^(string_exp e2)
  |String x -> x
  |Import (file,x) -> "Import the module: " ^ file ^ " to var "^x
  |ChangeIndex(e1, e2, e3) -> "Change index "^(string_exp e2)^" of "^
                              (string_exp e1)^" to " ^ (string_exp e3)
  |For(i, eB, e, Skip) -> "For Loop over "^ (string_exp i) ^" with guard "^(string_exp eB)^" and expression "^(string_exp e)
  |For(i, eB, e, after)-> "For Loop over "^ (string_exp i) ^" with guard "^(string_exp eB)^
                          " and expression "^(string_exp e)^". Each loop execute: "^(string_exp after)
  |Carry(x,t) -> "Carry of "^x^" of type "^(string_typ t)
  |Inspect(p) -> "Inspecting a prop"
  |SetInit(t,e)->"Setting the init of type"^(string_typ t)^" to the expression: "^string_exp e
  |Default t -> "Default value of type "^ string_typ t
  |ArbE e -> failwith "Arbitrary Expression"
  |Append (e,lst) -> "Append "^(string_exp e) ^" to list "^(string_exp lst)

let rec print_val = function
  |RInt i -> print_int i;
  |Null -> print_string "Null"
  |REmpty -> print_string "Empty"
  |RList(h, t) -> print_val h ; print_string " :: "; print_val t
  |RTuple(vList) -> print_string "Tuple of "; List.map (print_val) vList; ()
  |RExp e -> print_string ("Function with "^string_exp e)
  |RObject (name, lst) -> print_string ("Object of type "^name^" and "^(string_of_int (List.length lst))^" Values" )
  |RType t -> print_string(string_typ t)
  |RBool b -> print_string (if b then "true" else "false")
  |RString x -> print_string ("String: "^x)
  |RArb(x,TInt,I ls)-> print_string ("Arbitrary Int "^x^" with c of "^(string_of_int ls.v.c)^" and factor of "^(string_of_int ls.v.n)^" times "^x) 
  |RArb(x,t,inf) -> print_string ("Arbitrary "^(string_typ t)^": "^x)
  |_->failwith ""

let print_val2 = function
  |a,RInt i -> print_endline ((string_of_int a) ^ "," ^(string_of_int i));
  |_,Null -> print_endline "Null"
  |_->failwith ""

let get_var (s : state) (x : var): value= 
  try 
    List.assoc x s.s 
  with Not_found -> print_endline x;failwith "Temp"

let set_var (state : state) (x : var) (v : value): state =
  let takeout = List.remove_assoc x state.s in
  let add = (x, v) :: takeout in
  {state with s = add;}

let get_reg (s : state) (n : int) : value =
  try
    List.assoc n s.reg_file 
  with Not_found -> failwith ((string_of_int n)^ " is not a stored register")

let set_reg (state : state) (n : int) (v : value) : state =
  let takeout = List.remove_assoc n state.reg_file in
  let addin = (n, v) :: takeout in
  {state with reg_file = addin}

let get_fun (state : state) (f : var) : exp =
  try
    List.assoc f state.domain
  with Not_found -> failwith "Not a function"

let set_case (state : state) (types : typ * typ) (case: exp) : state =
  let takeout = List.remove_assoc types state.cases in
  let addin = (types, case) :: takeout in
  {state with cases = addin}

let get_case (state : state) (types : typ * typ) : exp =
  match List.assoc_opt types state.cases with
  |Some e -> e
  |None -> raise (NoCase "")


let rem_case (state:state) (types: typ*typ) : state =
  let takeout = List.remove_assoc types state.cases in {state with cases=takeout}

let make_configuration x : state = {
  s=[];
  reg_file=[];
  domain=[];
  func=Skip;
  cases=[];
  types=[]
}

let store_args (vals: value list) (state:state) =
  let rec change i s = function
    |[]-> s
    |h::t -> let s' = set_reg s i h in change (i+1) s' t
  in
  change 0 state vals

let load_args (vars: var list) (state:state) =
  let rec change i s = function
    |[] -> s
    |h::t -> let s' = set_var s h (get_reg s i) in change (i+1) s' t
  in
  change 0 state vars

let rec val_to_exp (v : value) : exp =
  match v with
  |RBool b-> if b then True else False
  |RInt i -> Int i
  |RId x -> Var x
  |RString x -> String x
  |RExp e -> Fun e
  |REmpty -> Empty
  |RList(v1,v2) -> BinOp(Cons, (val_to_exp v1), (val_to_exp v2))
  |Null -> Type(NULLTYPE)
  |RType t -> Type(t)
  |RTuple vList -> Tup (List.map val_to_exp vList)
  |RObject (name,vList) -> ObjectOf("", TCust name, List.map (fun (x,y) -> x,val_to_exp y) vList)
  |RArb (x,z,y) -> Var(x)

let rec typeof (s : state) (v : value): typ =
  match v with
  |RBool b-> TBool
  |RInt i -> TInt
  |RId x -> typeof s (get_var s x) 
  |RString x -> TString
  |RExp e -> TFun
  |REmpty -> TList NULLTYPE
  |RList(v1,v2) -> begin
      if v2 == REmpty then TList (typeof s v1) else
        let t1 = typeof s v1 in
        let t2 = typeof s v2 in
        if TList(t1)=t2 then t2 else failwith "List of multiple types"
    end
  |Null -> NULLTYPE
  |RType t -> t
  |RTuple vList -> TTuple (List.map (typeof s) vList)
  |RObject (name, vList) -> TCust name
  |RArb(x, t, l) -> t

let rec default_value (t : typ) : value =
  match t with
  | TInt -> RInt 0
  | TBool -> RBool false
  | TFun -> RExp Skip
  | TVar -> RId ""
  | TCust _ -> raise (NoCase "Catch this default value")
  | TList _ -> REmpty
  | NULLTYPE -> Null
  | TString -> RString ""
  | A' -> Null
  | TTuple tList -> RTuple (List.map default_value tList)

let rec eval (e : exp) (s : state) : value * state=
  (*print_endline(string_exp e);*)
  match e with
  |Fun e -> RExp e, s
  |Int i -> RInt i, s
  |String x -> RString x, s
  |Reg i -> get_reg s i, s
  |True -> RBool true, s
  |False ->RBool false, s
  |Var x -> get_var s x, s
  |List eList -> evalList eList s
  |Type typ -> RType typ, s
  |Skip -> Null, s
  |Empty -> REmpty, s
  |Tup eList -> evalTup eList s
  |Assign (x, e1) -> evalAssign x e1 s
  |Store (r1, e2) -> evalStore r1 e2 s
  |Load (x, r1) -> evalLoad x r1 s
  |Seq (e1, e2) -> let (_, s1) = eval e1 s in eval e2 s1
  |If (bEx, e1, e2) -> evalIf bEx e1 e2 s
  |State (x, e) -> evalState x e s
  |Call e -> evalCall e s
  |Set e1 -> evalSet e1 s
  |Do -> evalDo s.func s
  |BinOp(b, e1, e2) -> evalBinop b e1 e2 s
  |CallOn(vars,f,args) -> evalCallOn vars f args s
  |HD e1 -> evalHd e1 s
  |TL e1 -> evalTl e1 s
  |Match (e1, eList) -> evalMatch e1 eList s
  |Index (e, index) -> evalIndex e index s
  |SetT (typ1, typ2, e) -> evalSetT (typ1, typ2) e s
  |RemT (typ1, typ2) -> evalRemT (typ1, typ2) s
  |Object vList-> evalObject  vList s
  |ObjectOf (x,typ, vList) -> evalObjectOf x typ vList s
  |TypeDef (name, typList) -> evalTypeDef name typList s
  |GetField (e, field) -> evalField e field s
  |AssignField (e1, field, e2) -> evalAssignField e1 field e2 s
  |Import (file,x) -> evalImport file x s
  |ChangeIndex (e1, index, e2) -> evalChangeIndex e1 index e2 s
  |For(i, eB, e, after) -> evalFor i eB e after s
  |Carry(x, t) -> evalCarry x t s
  |Inspect p -> evalInspect p s
  |SetInit(t,e)->evalSetInit t e s
  |Default t -> evalDefault t s
  |ArbE e -> evalA e s
  |Append(e,lst) -> evalAppend e lst s

and evalA (e:arbExp) (s:state) =
  match e with
  |ArbV e -> eval e s
  |ArbIf(p,e1,e2) -> evalArbIf p e1 e2 s

(** typ2 is what you have, typ1 is what you wanted*)
and case (typ1) (v:value) (s:state) (error: string) : value  =
  try
    let typ2 = typeof s v in
    if typ2 == typ1 then v else
      let e = get_case s (typ1, typ2) in
      let s' =store_args [v] s in
      fst(eval e s')
  with NoCase _-> raise (NoCase error)

and evalToType (t : typ) (e : exp) (s : state) =
  let v1, s1 = eval e s in
  let t2 = typeof s v1 in
  if t2 == t then v1, s1 else case t v1 s1 ("Not of type "^string_typ t), s1

and evalAppend (e:exp) (lst:exp) (s:state) =
  let v1,s1 = eval e s in
  let v2, s2 = eval lst s1 in
  let rec add_to_end = function
    |RList(v,REmpty) -> RList(v,RList(v1,REmpty))
    |REmpty -> RList(v1,REmpty)
    |RList(v,z) -> RList(v,add_to_end z)
    |_ -> failwith "Malformed append list"
  in
  match v1,v2 with
  |_,RList (h,lst) when TList(typeof s1 v1) = typeof s2 v2 -> add_to_end v2 , s2
  |_,REmpty -> RList(v1, REmpty), s2
  |_,RList (h,lst) -> begin
      let newt = typeof s2 h in
      let newV = case newt v1 s2 "Cons-ing on a value of wrong type" in
      evalAppend (val_to_exp newV) (val_to_exp v2) s2
    end
  |_ ->failwith "Second value is not a list"

and evalArbIf (p:prop) (e1:exp) (e2:exp) (s:state) =
  let xN,b,o= begin 
    match p with 
    | VB(x,b,o) -> x, b, o
    | EB(e,b,o)-> begin 
        (match eval e s with 
         |RArb(x,TInt,ls),s'->x,b,o
         |_->failwith "Don't play with Arbitrary If's if the guard is not arbitrary")
      end
    |_->failwith "Unimplemented" 
  end in
  let x,_ = eval (Var xN) s in
  let o,s' = eval o s in
  let a,z = begin
    match b with
    |LT -> fst (evalBinop Minus (val_to_exp o) (Int 1) s'), x
    |GT -> fst (evalBinop Plus (val_to_exp o) (Int 1) s'), x
    |Equals -> o,x
    |NotEquals -> fst (evalBinop Plus (val_to_exp x) (Int 1) s'),o
    |_->failwith "Binary operator used is not boolean"
  end in
  let a,s1 = eval e1 (set_var s' xN a) in
  let b,s2 = eval e2 (set_var s' xN z) in
  match a,b with
  |RInt i, RInt j -> begin 
      if i==j then RInt i, s 
      else 
        let n = (i+j)/2 in
        let v = {n=0;other=None;m=0;c=n} in
        let v = I{isZero= false; amountAdded= Some n; times=0; v=v} in
        RArb("",TInt,v),s
    end
  |RInt i, RArb(x,TInt,ls)
  |RArb(x,TInt,ls),RInt i -> begin
      let ls = match ls with |I a -> a in
      let t = ls.times/2 in
      let c = (ls.v.c + i)/2 in
      let v = {n=t; other=ls.v.other; m=ls.v.m/2;c=c} in
      let v = I{isZero=c==0&&t==0&&v.m==0;amountAdded = if c==0 then None else Some c; times=t;v=v} in
      RArb(x,TInt,v),s
    end
  |RArb(x,TInt,ls),RArb(y,TInt,ls')-> begin
      let v = evalArbPM ls ls' true x y in
      let v = match v with |I a -> a in
      if v.isZero then RArb(x, TInt, I v), s else
        let t = v.times/2 in
        let a = match v.amountAdded with |Some x -> x |None -> 0 in
        let v = {n=v.v.n/2;other=v.v.other;m=v.v.m/2;c=a/2} in
        let v = I{isZero=a==0&&t==0&&v.m==0;amountAdded = if a==0 then None else Some (a/2); times=t; v=v} in
        RArb(x,TInt,v),s
    end
  |_-> failwith "Brachnes of an arbitrary If should return 2 integers, arbitrary or concrete"


and evalDefault (t:typ) (s:state) =
  try default_value t, s
  with NoCase _ -> evalObjectOf "" t [] s

and evalSetInit (t:typ) (e:exp) (s:state) =
  let t = match t with |TCust x -> x |_->failwith "Cannot set the init function of a base type" in
  let typspec = List.assoc_opt t s.types in
  let typList = match typspec with |Some (_,a)->a |None ->failwith "Cannot set the init function of a nonexistant type" in
  let temp = List.remove_assoc t s.types in
  let addin = (t,(e,typList))::temp in
  Null,{s with types = addin}

and evalInspect (p:prop) (s:state) =
  let b =
    match p with
    |VB (x,b,e) -> begin 
        let t, lst = match get_var s x with |RArb (x, t, lst)-> t,lst | _ -> failwith "Not an arbitrary value"  in
        match evalBinop b (Var x) e s with
        |RBool b, _ -> b
        |_ -> failwith "Binop was not a boolean operator"
      end
    |EB (e1,b,e2) -> begin
        match evalBinop b e1 e2 s with
        |RBool b,_-> b
        |_->failwith "Binop was not a boolean operator"
      end
    |_ -> failwith "Unimplemeneted"
  in
  RBool b, s

and evalCarry (x:var) (t:typ) (s:state) =
  let inf = 
    match t with
    | TInt -> begin 
        let v = {n=1;other=None;m=0;c=0} in
        I{isZero= false; amountAdded= None; times=1; v=v}
      end
    | _-> failwith "Unimplemented Carry"
  in let a = RArb (x, t, inf) in a,set_var s x a


and evalFor (i:exp) (eB:exp) (e:exp) (after:exp) (s:state) =
  let rec looper s' = begin
    let _,s'' = eval e s' in
    let _, s'' = eval after s'' in
    let v', s''' = evalToType TBool eB s'' in
    let b = match v' with |RBool b -> b |_ -> failwith "Impossible" in
    if not b then Null, s''' else looper s'''
  end
  in
  let _, si = eval i s in
  let v2, s2 = evalToType TBool eB si in
  let b = match v2 with |RBool b -> b |_ -> failwith "Impossible" in
  if not b then Null, s2 else looper s2

and evalChangeIndex (e1:exp) (index:exp) (e2:exp) (s :state) =
  let i, s' = match eval index s with
    |(RInt i, s') -> i,s'
    |t, s' -> begin 
        let newV = case TInt t s' "Indexing with a non-integer" in
        match newV with RInt i -> i, s' | _ -> failwith "Case statement failed to arrive at int"
      end
  in
  let data, sData = match eval e1 s with
    |RTuple _, s'' as a -> a
    |RList _, s'' as a -> a
    |t, s''-> begin
        try
          let newV = case (TList A') t s'' "Indexing a non-integer" in
          match newV with |RTuple _ as a -> a, s'' | RList _ as a -> a, s'' |_ -> raise(NoCase "Case did not arrive at an indexable value")
        with NoCase _ ->
          let newV = case (TTuple [A']) t s' "Indexing a non-integer" in
          match newV with |RTuple _ as a -> a, s'' | RList _ as a -> a, s'' |_ -> raise(NoCase "Case did not arrive at an indexable value")
      end
  in 
  let v1, s1 = eval e2 sData in
  match data with
  |RList (hd, tl) -> begin
      let rec replacer n = function
        |RList(v,l) when n=0 -> RList(v1,l)
        |RList(v, l) -> RList(v,replacer (n-1) l)
        |REmpty -> raise Not_found
        |_ -> failwith "Malformed List"
      in
      if (typeof s1 v1) != typeof s1 hd then failwith "Attemping to place a value of the incorrect type into this list" else
        replacer i data, s1
    end
  |RTuple (eList) -> begin
      let rec replace n = function
        |[] when n=0 -> []
        |h::t when n=0 -> v1::t
        |h::t -> h :: (replace (n-1) t)
        |[] -> failwith "Indexing an invalid index"
      in
      RTuple(replace i eList), s1
    end
  |_ -> raise(NoCase "How did you even get here?") 



and evalImport (file:string) (x:var) (s:state) =
  let lexbuf = 
    try 
      let opened = open_in (file^".cr") in
      Lexing.from_channel opened
    with Lexer.Eof -> print_string "fail"; exit 0
  in
  let c =
    try Parser.p Lexer.token lexbuf
    with Parser.Error ->
      print_endline ("Error in: "^file);
      Printf.printf "Syntax error at line %d character %d\n"
        !Lexer.lineno
        (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1);
      exit 1 in
  let ret,sOther = (eval c (make_configuration c)) in
  let newtypes = List.rev_append sOther.types s.types in
  let s = {s with types = newtypes} in
  let o = RObject("Module", sOther.s) in
  ret, set_var s x o 


and evalAssignField (e1 : exp) (field:var) (e2: exp) (s:state) =
  let v1, s1 = eval e1 s in
  let vals, name = 
    match v1 with
    |RObject (name, vals) -> vals, name
    |_ -> failwith "Cannot retrieve fields"
  in
  let vals = List.remove_assoc field vals in
  let v2, s2 = eval e2 s1 in
  let vals = (field,v2)::vals in
  let newO = RObject(name,vals) in
  match e1 with
  |Var x -> newO, set_var s2 x newO
  |Reg i -> newO, set_reg s2 i newO
  | _ -> newO,s2


and evalField (e:exp) (field:var) (s :state ) = 
  let v1,s1 = eval e s in
  let vals =
    match v1 with
    |RObject (_, vals) -> vals
    |_ -> failwith "Cannot retrieve fields"
  in
  match List.assoc_opt field vals with
  |Some v -> v,s1
  |None -> Null, s1

and evalObjectOf (x:var) (typ: typ) (eList : (var*exp) list) (s:state) =
  let e_to_v field_name field_type =
    let v = List.assoc_opt field_name eList in
    match v with
    |None -> default_value field_type
    |Some e -> begin 
        let value = fst(eval e s) in 
        if (typeof s value) == field_type then 
          value 
        else 
          failwith ("Assigning a value of the wrong type to the field: "^field_name) 
      end 
  in
  let typ = match typ with |TCust x -> x |_ -> failwith "Cannot create an object of this type" in
  let typ_spec = List.assoc_opt typ s.types in
  let typ_spec = match typ_spec with |Some x -> x |None -> failwith "This type doesn't exist" in
  match typ_spec with
  |Skip, val_pairs -> begin
      let object_vals = List.map (fun (var,typ)->var, e_to_v var typ) val_pairs in
      let ob = RObject(typ, object_vals) in
      match x with
      |"" -> ob, s
      |_ -> ob, set_var s x ob
    end
  |e,val_pairs-> begin 
      let ob,s' = eval e s in
      let ob_lst = match ob with |RObject(_,lst)->lst |_ -> failwith "Not an object" in
      let check_init field_name field_type = 
        let v = List.assoc_opt field_name ob_lst in
        match v with
        |None -> failwith ("Initialization function failed to initialize the field "^field_name)
        |Some v -> begin 
            if field_type <> typeof s v then failwith ("Field "^field_name^" was given a value of the wrong type.") else v 
          end
      in
      let ob_vals = List.map (fun (var,typ)->var,check_init var typ) val_pairs in
      let ob = RObject(typ, ob_vals) in
      match x with
      |"" -> ob, s
      |_ -> ob, set_var s x ob
    end


and evalTypeDef (name:var) (typList:(var*typ) list) (s:state) =
  let temp = List.remove_assoc name s.types in
  let addin = (name,(Skip,typList))::temp in
  Null,{s with types = addin}

and evalObject (vList: (var*exp) list) (s:state) =
  let rec to_vals = function
    |[]->[]
    |(n,e)::t -> (n,(fst(eval e s))) :: to_vals t
  in
  let lst = to_vals vList in 
  RObject("", lst), s

and evalRemT (types: typ*typ) (s:state): value*state =
  Null, rem_case s types

and evalSetT (types:typ*typ) (e : exp) (s:state): value * state =
  Null, set_case s types e

and evalList (eList: exp list) (s:state) =
  let rec to_vals = function
    |[]->[]
    |h::t -> (fst(eval h s)) :: to_vals t
  in
  let rec to_list = function
    |[]->REmpty
    |h::t -> RList(h,to_list t)
  in
  let vals = to_vals eList in

  RList(List.hd vals, to_list (List.tl vals)), s

and evalIndex (e:exp) (index: exp) (s:state) =
  match eval index s with
  |(RInt i, s') -> begin
      match eval e s with
      |RTuple eList, s'' -> begin 
          try List.nth eList i, s 
          with Not_found -> Null,s 
        end
      |RList (hd, tl), s'' -> begin
          let rec indexer n = function
            |RList(v,_) when n=0 -> v, s
            |RList(v, l) -> indexer (n-1) l
            |REmpty -> Null,s
            |_ -> failwith "Malformed List"
          in
          if i == 0 then hd, s else indexer (i-1) tl
        end
      |t, s''-> begin
          try
            let newV = case (TList A') t s'' "Indexing a non-integer" in
            evalIndex (val_to_exp newV) (Int i) s''
          with NoCase _ ->
            let newV = case (TTuple [A']) t s' "Indexing a non-integer" in
            evalIndex e (val_to_exp newV) s'
        end

    end
  |t, s' -> begin 
      let newV = case TInt t s' "Indexing with a non-integer" in
      evalIndex e (val_to_exp newV) s'
    end

and evalTup (eList: exp list) (s:state) : value * state =
  let rec to_vals = function
    |[]->[]
    |h::t -> (fst(eval h s)) :: to_vals t
  in 
  let vals = to_vals eList in
  RTuple vals, s

and evalMatch (e:exp) (eList:(typ * exp) list) (s:state) : value * state =
  let rec test = function
    |[] -> print_endline "Done"
    |(t3,e4) :: t -> print_endline (string_typ t3); test t
  in
  let v1, s' = eval e s in
  let t = typeof s' v1 in
  try
    let e' = List.assoc t eList in
    eval e' s'
  with Not_found -> begin
      if (match t with |TList _ -> true | _ -> false) 
      then
        try 
          let e' = List.assoc (TList A') eList in
          eval e' s'
        with Not_found -> failwith (string_typ t)
      else
        failwith (string_typ t)
    end

and evalHd (e:exp) (s:state) =
  match eval e s with
  |RList(h,_), s' -> h, s'
  |REmpty, s' -> Null, s'
  |a -> a

and evalTl (e:exp) (s:state) =
  match eval e s with
  |RList(_,t), s' -> t, s'
  |REmpty, s' -> REmpty, s'
  |_, s' -> Null, s'

and evalCallOn (vars:var list) (f:exp) (args:exp list) (s:state) =
  let rec to_vals = function
    |[]->[]
    |h::t -> (fst(eval h s)) :: to_vals t
  in 
  let func = match f with 
    |Var x -> get_fun s x | _-> 
      begin
        match eval f s with
        |RId x, z -> get_fun s x 
        |RExp e, z -> e
        |a, b -> begin
            let newV = case TFun a b "Not a function" in
            if (typeof b newV) == TFun then (val_to_exp newV) else raise (NoCase "No function found")
          end
      end
  in
  let vals = to_vals args in
  let s' = store_args vals s in
  let v,s' = eval func s' in
  v, (load_args vars s')

and evalDo (e:exp) (s:state) =
  let v1,s1 = eval e s in v1, {s with reg_file=s1.reg_file}

and evalSet (e:exp) (s:state) =
  match e with
  |Var x -> begin
      try
        Null,{s with func=List.assoc x s.domain}
      with Not_found ->failwith "asdf"
    end
  |_ -> Null, {s with func = e}

and evalArbPlus (ls : info) (i:int) =
  let ls = match ls with |I a -> a in
  let x = match ls.amountAdded with
    |None -> i
    |Some x -> x + i
  in
  let b =if (i<0 && ls.times = 0 && x<1) then true else false in
  I{ls with amountAdded = Some x; v = {ls.v with c=x}; isZero=b}

and evalArbTimes (ls :info) (i:int) =
  let ls = match ls with |I a -> a in
  let times = ls.times * i in
  let add = match ls.amountAdded with
    |None -> None
    |Some x -> Some (i*x)
  in
  let v = {ls.v with n=times; c = ls.v.c * i} in
  I{amountAdded = add; times = times; isZero = (i==0||(ls.times==0&&ls.amountAdded=None)); v=v}


and evalArbPM (ls : info) (ls': info) (pm : bool) (n1 : var) (n2 : var)=
  let ls = match ls with |I a ->a in
  let ls' = match ls' with |I a-> a in
  let x = match ls.amountAdded, ls'.amountAdded with
    |Some x, Some y -> Some(if pm then x + y else x - y)
    |Some x, None -> Some x
    |None, Some x -> Some (if pm then x else -x)
    |None, None -> None
  in
  let n,z = begin
    match ls.v.other with
    |None -> n2,ls'.times
    |Some x when x=n2 -> x,ls.v.m + ls'.times
    |_ -> raise (ArbError"Cannot support adding more than 2 arbitrary ints")

  end in
  let z' = begin
    match ls'.v.other with
    |None -> 0
    |Some x when x=n1 -> ls'.v.m
    |_ -> raise (ArbError"Cannot support adding more than 2 arbitrary ints")
  end
  in
  let v = {c = (match x with |None -> 0 |Some x -> x); other = Some n; m = z; n=ls.v.n+z'} in
  I{ls with amountAdded = x; v=v}

and evalArbT (ls : info) (ls':info) (n2:var)=
  let help i = match i with |None -> 0 |Some i -> i 
  in
  let ls = match ls with |I a ->a in
  let ls' = match ls' with |I a-> a in
  let a = if ls.times = 0 then help ls.amountAdded else ls.times in
  let b = if ls'.times = 0 then help ls'.amountAdded else ls'.times in
  let ad, t, isZ =
    if not (ls.isZero || ls'.isZero|| a==0 || b ==0) then
      let add = match ls.amountAdded, ls'.amountAdded with
        |Some x, Some y -> Some(b * x + a * y)
        |Some x, None -> Some(ls'.times * a)
        |None, Some x -> Some(ls.times * a)
        |None, None -> None  in
      add, a*b, false
    else 
      None, 0, true
  in
  let name = Some begin
      match ls.v.other with
      |None -> n2
      |Some x when x=n2 -> n2
      |_ -> raise(ArbError "Cannot support multiplying more than 2 arbitrary ints")
    end in
  let n = if ls.times == 0 then 0 else ls.v.n * b in
  let m = if ls'.times == 0 then 0 else ls'.times * a in
  let v = {c = (match ad with |None -> 0 |Some x -> x); other = name; n = n; m = m} in
  I{amountAdded = ad; times = t; isZero = isZ; v = v}

and evalArbEQ (n1:string) (n2:string) (ls : info) (ls':info) =
  (** Written oddly, but will fix when the int_info is expanded and I have a 
      better idea of what the values really look like. *)
  let ls = match ls with |I a ->a in
  let ls' = match ls' with |I a-> a in
  if n1 != n2 then ls.times == 0 && ls'.times==0 && ls.v.c==ls'.v.c && (if ls'.v.other=ls.v.other then ls.v.m=ls'.v.m else ls.v.m==0&&ls'.v.m==0)

  else if ls.times != ls'.times then false 
  else if ls.amountAdded <> ls'.amountAdded then false
  else if ls.v.other <> ls.v.other && not (ls.v.m==0&&ls'.v.m==0) then false
  else ls.v.m==ls'.v.m

and evalArbLG (ls:info) (i:int) (gt:bool)=
  let ls = match ls with |I a -> a in
  let add = match ls.amountAdded with | None -> 0 |Some x -> x in
  if gt then 
    if add > i then true else
    if ls.isZero then false else ls.times > i
  else
  if add > i then false else
  if ls.isZero then i!=0 else ls.times < i


and evalTwoLT (ls:info) (ls' : info) (n1) (n2)=
  let ls = match ls with |I a ->a in
  let ls' = match ls' with |I a-> a in
  let c1, c2 = ls.v.c, ls'.v.c in
  if ls.isZero && not ls'.isZero then true else
  if ls.v.n == 0 && (ls.v.other = None || ls.v.m==0) && ls'.v.n == 0 && (ls'.v.other==None || ls'.v.m=0) then c1<c2 else
  if n1 == n2 then 
    ((ls'.times>=ls.times && (c1<c2))||(ls'.times-ls.times>c1-c2)||(ls'.times>ls.times&&(c1<=c2))) && 
    (if ls.v.other=ls'.v.other 
     then ls.v.m<=ls'.v.m 
     else false) 
  else
    match ls.v.other, ls'.v.other with
    |None, Some n when n1 == n -> c1<c2 && (ls'.v.m>=ls.times)
    |Some n, None when n2 == n -> c1<c2 && (ls.v.m<=ls'.times)
    |Some n11, Some n12 when n1 == n11 && n2==n12 -> (c1<c2)&&(ls.v.m<=ls'.v.n)&&(ls.v.n<=ls'.v.m)
    |Some n11, Some n12 when n1 == n11 -> c1<c2 && (ls.v.m<=ls'.times)
    |Some n11, Some n12 when n2==n12 -> c1<c2 && (ls.v.m<=ls'.times)
    |None, Some _ 
    |Some _, None 
    |None, None -> if n1 != n2 then ls.isZero&&ls'.isZero&&(c1<c2) else false
    | _-> failwith "Debug this"

and evalTwoGT (ls:info) (ls' : info) (n1) (n2) =
  let ls = match ls with |I a ->a in
  let ls' = match ls' with |I a-> a in
  let c1, c2 = ls.v.c, ls'.v.c in
  if not ls.isZero && ls'.isZero then true else
  if ls.v.n == 0 && (ls.v.other = None || ls.v.m==0) && ls'.v.n == 0 && (ls'.v.other==None || ls'.v.m=0) then c1>c2 else
  if n1 == n2 then 
    ls'.times<=ls.times && (c1>c2) && 
    (if ls.v.other=ls'.v.other 
     then ls.v.m>=ls'.v.m 
     else false) 
  else
    match ls.v.other, ls'.v.other with
    |None, Some n when n1 == n -> c1>c2 && (ls'.v.m<=ls.times)
    |Some n, None when n2 == n -> c1>c2 && (ls.v.m>=ls'.times)
    |Some n11, Some n12 when n1 == n11 && n2==n12 -> (c1>c2)&&(ls.v.m>=ls'.v.n)&&(ls.v.n>=ls'.v.m)
    |Some n11, Some n12 when n1 == n11 -> c1>c2 && (ls.v.m>=ls'.times)
    |Some n11, Some n12 when n2==n12 -> c1>c2 && (ls.v.m>=ls'.times)
    |None, Some _ 
    |Some _, None 
    |None, None -> if n1 != n2 then ls.isZero&&ls'.isZero&&(c1>c2) else false
    | _-> failwith "Debug this"

and evalBinop (b:binop) (e1:exp) (e2:exp) (s:state) =
  let rec listsEq l1 l2 =
    match l1, l2 with
    |RList (v1,l), RList (v2, l') when v1=v2 -> false
    |RList _, REmpty
    |REmpty, RList _ -> false
    |REmpty, REmpty -> true
    |RList (v1,l), RList (v2, l') -> listsEq l l' 
    |_ -> failwith "Malformed List equality"
  in
  let v1,s1 = eval e1 s in 
  let v2, s2 = eval e2 s1 in 
  match b with
  |Plus-> begin
      match v1,v2 with
      |RArb(x, TInt, ls), RArb(y,TInt, ls1) -> RArb("",TInt,evalArbPM ls ls1 true x y), s2
      |RArb(x, TInt, ls), RInt i
      |RInt i, RArb(x, TInt, ls)-> RArb(x, TInt, evalArbPlus ls i), s2
      |RInt i, RInt l -> RInt(i+l), s2
      |RString x, RString y -> RString(x^y), s2
      |RInt i, x
      |x, RInt i -> begin
          let newV = case TInt x s "Addition of non-integers" in
          evalBinop Plus (Int i) (val_to_exp newV) s2
        end
      |RString x, z -> begin
          let newV = case TString z s "String concatenation of a non string" in
          evalBinop Plus (String x) (val_to_exp newV) s2
        end
      |x,y -> begin
          let newV1 = case TInt x s "Addition of non-integers" in
          let newV2 = case TInt y s "Addition of non-integers" in
          evalBinop Plus (val_to_exp newV1) (val_to_exp newV2) s2
        end
    end
  |Minus-> begin
      match v1,v2 with
      |RArb(x, TInt, ls), RArb(y,TInt, ls1) -> RArb("", TInt, evalArbPM ls ls1 false x y), s2
      |RArb(x, TInt, ls), RInt i
      |RInt i, RArb(x, TInt, ls)-> RArb(x, TInt, evalArbPlus ls (-i)), s2
      |RInt i, RInt l -> RInt(i-l), s2
      |RInt i, x -> begin
          let newV = case TInt x s "Subtraction of non-integers" in
          evalBinop Minus (Int i) (val_to_exp newV) s2

        end
      |x, RInt i -> begin
          let newV = case TInt x s "Subtraction of non-integers" in
          evalBinop Minus (val_to_exp newV) (Int i)  s2
        end
      |x,y -> begin
          let newV1 = case TInt x s "Subtraction of non-integers" in
          let newV2 = case TInt y s "Subtraction of non-integers" in
          evalBinop Minus (val_to_exp newV1) (val_to_exp newV2) s2
        end
    end
  |Times-> begin
      match v1,v2 with
      |RArb(x, TInt, ls), RArb(y,TInt, ls1) -> RArb("",TInt,evalArbT ls ls1 y), s2
      |RArb(x, TInt, ls), RInt i
      |RInt i, RArb(x, TInt, ls)-> RArb(x, TInt, evalArbTimes ls i), s2
      |RInt i, RInt l ->RInt(i*l), s2
      |RInt i, RString x -> begin
          let rec stri x n = if n = 0 then "" else (x^stri x (n-1)) in
          RString(stri x i), s2
        end
      |RInt i, x
      |x, RInt i -> begin
          let newV = case TInt x s "Multiplication of non-integers" in
          evalBinop Times (Int i) (val_to_exp newV) s2
        end
      |x,y -> begin
          let newV1 = case TInt x s "Multiplication of non-integers" in
          let newV2 = case TInt y s "Multiplication of non-integers" in
          evalBinop Minus (val_to_exp newV1) (val_to_exp newV2) s2
        end
    end
  |Equals-> begin
      match v1, v2 with
      |RArb(x, TInt, ls), RArb(y,TInt, ls1) -> RBool(evalArbEQ x y ls ls1), s2
      |RArb(x, TInt, ls), RInt i
      |RInt i, RArb(x, TInt, ls)-> let ls = match ls with |I a ->a in 
        RBool((ls.isZero&&i==0)||(ls.times == 0 &&ls.v.m =0 && ls.amountAdded = Some i)), s2
      |RInt i, RInt l-> RBool (i==l), s2
      |RBool a, RBool c -> RBool (a==c), s2
      |Null, Null -> RBool true, s2
      |RExp e, RExp e' ->RBool (e1=e2), s2 (* CHECK THIS *)
      |RString x, RString y -> RBool (x==y), s2
      |REmpty, REmpty -> RBool true, s2
      |RList _, REmpty 
      |REmpty, RList _ -> RBool false, s2
      |RList (a,b), RList (c,d) -> RBool (listsEq (RList(a,b)) (RList(c,d))), s2
      |RType t1, RType t2 -> RBool(t1=t2), s2
      |RObject(x, a), RObject(y,b) -> RBool(x==y), s2
      |RTuple vList, RTuple vList' -> RBool(vList=vList'),s2
      |a, b -> begin
          let t1 = typeof s2 a in
          let newV = case t1 b s2 "Equality between unequal types" in
          if t1 = typeof s2 newV then 
            evalBinop Equals (val_to_exp a) (val_to_exp newV) s2
          else 
            RBool false, s2
        end
    end
  |NotEquals-> begin
      match v1, v2 with
      |RArb(x, TInt, ls), RArb(y,TInt, ls1) -> RBool(not (evalArbEQ x y ls ls1)), s2
      |RArb(x, TInt, ls), RInt i
      |RInt i, RArb(x, TInt, ls)-> let ls = match ls with |I a ->a in 
        RBool(ls.times != 0 && ls.amountAdded <> Some i), s2
      |RInt i, RInt l -> RBool (i!=l), s2
      |RBool a, RBool c -> RBool (a!=c), s2
      |Null, Null -> RBool false, s2
      |RExp e, RExp e' ->RBool (e1<>e2), s2 (* CHECK THIS *)
      |RString x, RString y -> RBool(x!=y), s2
      |REmpty, REmpty -> RBool false, s2
      |RList _, REmpty
      |REmpty, RList _ -> RBool true, s2
      |RList (a,b), RList (c,d) -> RBool (not (listsEq (RList(a,b)) (RList(c,d)))), s2
      |RType t1, RType t2 -> RBool(t1<>t2), s2
      |RObject(x, a), RObject(y,b) -> RBool(x!=y), s2
      |RTuple vList, RTuple vList' -> RBool(vList<>vList'),s2
      |a, b -> begin
          let t1 = typeof s2 a in
          let newV = case t1 b s2 "Equality between unequal types" in
          if t1 = typeof s2 newV then 
            evalBinop Equals (val_to_exp a) (val_to_exp newV) s2
          else 
            RBool true, s2
        end
    end
  |Cons -> begin
      match v1,v2 with
      |_,RList (h,lst) when TList(typeof s1 v1) = typeof s2 v2 -> RList (v1,RList(h,lst)), s2
      |_,REmpty -> RList(v1, REmpty), s2
      |_,RList (h,lst) -> begin
          let newt = typeof s2 h in
          let newV = case newt v1 s2 "Cons-ing on a value of wrong type" in
          evalBinop Cons (val_to_exp newV) (val_to_exp v2) s2
        end
      |_ ->failwith "Second value is not a list"
    end
  |LT -> begin
      match v1, v2 with
      |RArb(x, TInt, ls), RArb(y,TInt, ls1) -> RBool(evalTwoLT ls ls1 x y), s2
      |RArb(x, TInt, ls), RInt i 
      |RInt i, RArb(x, TInt, ls)-> RBool(evalArbLG ls i false), s2
      |RInt i, RInt j ->RBool(i<j), s2
      |RInt i, x -> begin
          let newV = case TInt x s "Less than of a non-integer" in
          evalBinop LT (Int i) (val_to_exp newV) s2
        end
      |x, RInt i -> begin
          let newV = case TInt x s "Less than of a non-integer" in
          evalBinop LT (val_to_exp newV) (Int i) s2
        end
      |x,y -> begin
          let newV1 = case TInt x s "Less than of a non-integer" in
          let newV2 = case TInt y s "Less than of a non-integer" in
          evalBinop LT (val_to_exp newV1) (val_to_exp newV2) s2
        end

    end
  |GT -> begin 
      match v1, v2 with
      |RArb(x, TInt, ls), RArb(y,TInt, ls1) -> RBool(evalTwoGT ls ls1 x y), s2
      |RArb(x, TInt, ls), RInt i 
      |RInt i, RArb(x, TInt, ls)-> RBool(evalArbLG ls i true), s2
      |RInt i, RInt j ->RBool(i>j), s2
      |RInt i, x -> begin
          let newV = case TInt x s "Greater than of a non-integer" in
          evalBinop GT (Int i) (val_to_exp newV) s2
        end
      |x, RInt i -> begin
          let newV = case TInt x s "Greater than of a non-integer" in
          evalBinop GT (val_to_exp newV) (Int i) s2
        end
      |x,y -> begin
          let newV1 = case TInt x s "Greater than of a non-integer" in
          let newV2 = case TInt y s "Greater than of a non-integer" in
          evalBinop GT (val_to_exp newV1) (val_to_exp newV2) s2
        end
    end
  |And -> begin
      match v1, v2 with
      |RBool x, RBool y -> RBool(x&&y), s2
      |RBool x, a when x = false -> RBool false, s2
      |RBool x, a
      |a, RBool x -> begin
          let newV = case TBool a s "And of a non-boolean" in
          let x = if x then True else False in
          evalBinop And (val_to_exp newV) x s2
        end
      |x, y -> begin
          let newV1 = case TBool x s "And of a non-boolean" in
          let newV2 = case TBool y s "And of a non-boolean" in
          evalBinop And (val_to_exp newV1) (val_to_exp newV2) s2
        end
    end
  |Or -> begin
      match v1, v2 with
      |RBool x, RBool y -> RBool(x||y), s2
      |RBool x, a when x = true -> RBool true, s2
      |RBool x, a
      |a, RBool x -> begin
          let newV = case TBool a s "Or of a non-boolean" in
          let x = if x then True else False in
          evalBinop Or (val_to_exp newV) x s2
        end
      |x, y -> begin
          let newV1 = case TBool x s "Or of a non-boolean" in
          let newV2 = case TBool y s "Or of a non-boolean" in
          evalBinop Or (val_to_exp newV1) (val_to_exp newV2) s2
        end
    end

and evalAssign (x : var) (e : exp) (s : state) : value * state =
  let (v, st) = eval e s in (v, set_var st x v)

and evalStore r e s =
  let (v2, s2) = eval e s in v2, set_reg s2 r v2


and evalLoad x r s =
  let newV = get_reg s r in
  let new_st = set_var s x newV in
  (newV, new_st)


and evalIf (b:exp) (e1:exp) (e2:exp) (s: state) : value * state =
  match eval b s with
  |RBool true, s1 -> eval e1 s1
  |RBool false, s1 -> eval e2 s1
  |v, s1 -> begin
      let newV = case TBool v s1 "Guard of if is not a boolean" in
      evalIf (val_to_exp newV) e1 e2 s1
    end

and evalState (x:var) (e:exp) (s:state) : value * state =
  let takeout = List.remove_assoc x s.domain in
  let addin = (x,e)::takeout in
  RExp e, {s with domain = addin}

and evalCall (x:exp) (s:state) =
  let id x = begin try
      let f = List.assoc x s.domain in
      let v,s'' = eval f s in
      v, {s with reg_file=s''.reg_file}
    with Not_found -> failwith "Not a function"
  end
  in
  match x with
  |Var x -> id x
  |_ -> begin
      match eval x s with
      |RId x, z-> id x
      |RExp e, z -> eval e s
      |t, s' -> begin
          let newV = case TFun t s' "Not a function" in
          evalCall (val_to_exp newV) s'
        end
    end

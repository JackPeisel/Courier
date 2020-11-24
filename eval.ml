open Ast


type store = (var * value) list

type register = (int * value)

type reg_file = register list

type call_env = (var * exp) list

type state = {
  s: store;
  reg_file: register list;
  domain: call_env;
  func: exp;
}

let string_binop (b:binop) =
  match b with
  |Plus -> "Plus"
  |Minus -> "Minus"
  |Times -> "Times"
  |Equals -> "Equals"
  |NotEquals -> "NotEquals"
  |Cons -> "Cons"

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
  |If (bEx, e1, e2) -> ""
  |State (x, e) -> "State "^x^" to be ("^(string_exp e)^")"
  |Call x -> "Call the function "^ x
  |Set e -> ""
  |Do -> ""
  |BinOp(b, e1, e2) -> (string_binop b)^" of ("^(string_exp e1)^" and ("^(string_exp e2)^")"
  |CallOn(vars,f,args) -> "PLACEHOLDER"
  |HD e1 -> "Head of "^(string_exp e1)
  |TL e1 -> "Tail of "^(string_exp e1)

let print_val = function
  |RInt i -> print_int i;
  |Null -> print_endline "Null"
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

let make_configuration x : state = {
  s=[];
  reg_file=[];
  domain=[];
  func=Skip
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

let rec typeof (v : value) (s : state): typ =
  match v with
  |RBool b-> TBool
  |RInt i -> TInt
  |RId x -> typeof (get_var s x) s
  |RExp e -> TFun
  |REmpty -> TList NULLTYPE
  |RList(v1,v2) -> begin
      let t1 = typeof v1 s in
      let t2 = typeof v2 s in
      if t1==t2 then t1 else failwith "List of multiple types"
    end
  |Null -> NULLTYPE
  |RType t -> t

let rec eval (e : exp) (s : state) : value * state=
  (*print_endline(string_exp e);*)
  match e with
  |Int i -> RInt i, s
  |Reg i -> get_reg s i, s
  |True -> RBool true, s
  |False ->RBool false, s
  |Var x -> get_var s x, s
  |Skip -> Null, s
  |Empty -> REmpty, s
  |Assign (x, e1) -> evalAssign x e1 s
  |Store (r1, e2) -> evalStore r1 e2 s
  |Load (x, r1) -> evalLoad x r1 s
  |Seq (e1, e2) -> let (_, s1) = eval e1 s in eval e2 s1
  |If (bEx, e1, e2) -> evalIf bEx e1 e2 s
  |State (x, e) -> evalState x e s
  |Call x -> evalCall x s
  |Set e1 -> evalSet e1 s
  |Do -> evalDo s.func s
  |BinOp(b, e1, e2) -> evalBinop b e1 e2 s
  |CallOn(vars,f,args) -> evalCallOn vars f args s
  |HD e1 -> evalHd e1 s
  |TL e1 -> evalTl e1 s

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

and evalCallOn (vars:var list) (f:var) (args:exp list) (s:state) =
  let rec to_vals = function
    |[]->[]
    |h::t -> (fst(eval h s)) :: to_vals t
  in 
  let vals = to_vals args in
  let func = get_fun s f in
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

and evalBinop (b:binop) (e1:exp) (e2:exp) (s:state) =
  let v1,s1 = eval e1 s in 
  let v2, s2 = eval e2 s1 in 
  match b with
  |Plus-> begin
      match v1,v2 with
      |RInt i, RInt l -> RInt(i+l), s2
      |_ ->failwith "Addition of non-integers"
    end
  |Minus-> begin
      match v1,v2 with
      |RInt i, RInt l -> RInt(i-l), s2
      |_ ->failwith "Subtraction of non-integers"
    end
  |Times-> begin
      match v1,v2 with
      |RInt i, RInt l ->RInt(i*l), s2
      |_->failwith "Multiplication of non-integers"
    end
  |Equals-> begin
      match v1, v2 with
      |RInt i, RInt l when i==l-> RBool true, s2
      |RBool a, RBool c when a==c-> RBool true, s2
      |Null, Null -> RBool true, s2
      |RExp e, RExp e' ->RBool (e1=e2), s2 (* CHECK THIS *)
      |_ -> RBool false, s2
    end
  |NotEquals-> begin
      match v1, v2 with
      |RInt i, RInt l when i==l-> RBool false, s2
      |RBool a, RBool c when a==c-> RBool false, s2
      |Null, Null -> RBool false, s2
      |RExp e, RExp e' ->RBool (e1<>e2), s2 (* CHECK THIS *)
      |_ -> RBool true, s2
    end
  |Cons -> begin
      match v1,v2 with
      |_,RList (h,lst) -> RList (v1,RList(h,lst)), s2
      |_,REmpty -> RList(v1, REmpty), s2
      |_ ->failwith "placeholder"
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
  |_-> failwith "Guard of if is not a boolean"

and evalState (x:var) (e:exp) (s:state) : value * state =
  let takeout = List.remove_assoc x s.domain in
  let addin = (x,e)::takeout in
  Null, {s with domain = addin}

and evalCall (x:var) (s:state) =
  try
    let f = List.assoc x s.domain in
    let v,s' = eval f s in
    v, {s with reg_file=s'.reg_file}
  with Not_found -> failwith "Not a function"

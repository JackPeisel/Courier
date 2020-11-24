
type var = string

type reg = int

type binop =
  | Plus
  | Minus
  | Equals
  | NotEquals
  | Times
  | Cons

and exp = 
  | Int of int
  | Reg of reg
  | True
  | False
  | Empty
  | Var of var
  | Skip
  | Assign of var * exp
  | Store of reg * exp
  | Load of var * reg
  | Seq of exp * exp
  | If of exp * exp * exp
  | State of var * exp
  | Call of var
  | CallOn of var list * var * exp list
  | Set of exp
  | Do
  | BinOp of binop * exp * exp

and value =
  | RInt of int
  | RBool of bool
  | RId of var
  | RExp of exp
  | REmpty
  | RList of value * value
  | Null

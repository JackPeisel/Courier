
type var = string

type reg = int

type binop =
  | Plus
  | Minus
  | Equals
  | NotEquals
  | Times
  | Cons
  | LT
  | GT
  | And
  | Or

and exp = 
  | Int of int
  | Reg of reg
  | String of string
  | True
  | False
  | Empty
  | Fun of exp
  | Tup of exp list
  | Index of exp * exp
  | List of exp list
  | Type of typ
  | Var of var
  | Skip
  | Assign of var * exp
  | Store of reg * exp
  | Load of var * reg
  | Seq of exp * exp
  | If of exp * exp * exp
  | State of var * exp
  | Call of exp
  | CallOn of var list * exp * exp list
  | Set of exp
  | Do
  | BinOp of binop * exp * exp
  | HD of exp
  | TL of exp
  | Match of exp * (typ * exp) list
  | SetT of typ * typ * exp
  | RemT of typ * typ
  | Object of (var * exp) list
  | TypeDef of var * (var * typ) list
  | ObjectOf of var * typ * (var* exp) list
  | GetField of exp * var
  | AssignField of exp * var * exp
  | Import of string * var
  | ChangeIndex of exp * exp * exp
  | For of exp * exp * exp * exp
  | Carry of var * typ
  | Inspect of prop
  | SetInit of typ * exp
  | Default of typ
  | ArbE of arbExp
  | Append of exp * exp

and value =
  | RInt of int
  | RBool of bool
  | RId of var
  | RExp of exp
  | REmpty
  | RList of value * value
  | Null
  | RType of typ
  | RTuple of value list
  | RObject of var * (var*value) list
  | RString of string
  | RArb of var * typ * info

and typ =
  |TInt
  |TBool
  |TFun
  |TCust of string
  |TVar
  |TList of typ
  |NULLTYPE
  |TTuple of typ list
  |TString
  |A'

and arbExp =
  |ArbV of exp
  |ArbIf of prop * exp * exp

and type_spec = exp * ((var*typ) list)

and info = 
  |I of int_info

and int_info = {
  isZero: bool;
  amountAdded: int option;
  times: int;
  v: arbInt
}

and prop = 
  |VF of exp
  |EF of exp * exp
  |VB of var * binop * exp
  |EB of exp * binop * exp

and arbInt = {
  n: int;
  other: var option;
  m: int;
  c:int

}
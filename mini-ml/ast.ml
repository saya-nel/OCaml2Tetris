
type prog = tmodule list
and tmodule = { mod_name : name ;
                decls : decl list }
and name = string

and decl = 
  | Exp of (exp)
  | DefVar of (name * exp)
  | DefFun of    ((name * name list * exp) list)
  | DefFunRec of ((name * name list * exp) list)
and exp = 
  | Constant of (constant)
  | Ident of (name)
  | Let of (name * exp * exp)
  | Fun of (name * exp)
  (*  | LetFun of (name * name list * exp * exp) *)
  | App of (exp * exp list)
  | If of (exp * exp * exp)
  | Match of (exp * match_case list)
  | BinOp of (binop * exp * exp)
  | UnOp of (unop * exp)
  | Pair of (exp * exp)
  | Cons of (exp * exp)
  | Array_create of (exp list)
  | Array_assign of (exp * exp * exp)
  | Array_access of (exp * exp)
  | Ref of (exp)
  | Ref_access of (exp)
  | Ref_assign of (exp * exp)
  | Seq of (exp * exp)
  | While of (exp * exp)
  | For of (name * exp * exp * exp)
  | Assert of (exp * Parseutils.pos)
and constant = 
  | Unit
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | Constr of string
  | List_empty
  | Array_empty
and match_case =
  | Case of (constant * exp)
  | Otherwise of (exp)
and binop =
  | Add
  | Minus
  | Mult
  | Div
  | Lt
  | Le
  | Neq
  | Eq
  | Ge
  | Gt
  | Or
  | And
  | Lor
  | Land

and unop =
  | UMinus
  | Not

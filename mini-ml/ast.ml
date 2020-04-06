
type prog = tmodule list
and tmodule = { mod_name : name ;
                decls : decl list }
and name = string
and var = (name * Types.typ option)

and decl = 
  | Exp of (exp)
  | DefVar of (var * exp)
  | DefFun of    ((name * var list * Types.typ option * exp) list)
  | DefFunRec of ((name * var list * Types.typ option * exp) list)
  | Type of (name * Types.typ)
and exp = 
  | Annotation of (exp * Types.typ)
  | Constant of (constant)
  | Ident of (name)
  | Let of (var * exp * exp)
  | Fun of (var * exp)
  (*  | LetFun of (name * name list * exp * exp) *)
  | App of (exp * exp list)
  | If of (exp * exp * exp)
  | Match of (exp * match_case list)
  | BinOp of (binop * exp * exp)
  | UnOp of (unop * exp)
  | Pair of (exp * exp)
  | Cons of (exp * exp)
  | Array_create of (exp list)
  | Array_alloc of (exp)
  | Array_assign of (exp * exp * exp)
  | Array_access of (exp * exp)
  | Ref of (exp)
  | Ref_access of (exp)
  | Ref_assign of (exp * exp)
  | Seq of (exp * exp)
  | While of (exp * exp)
  | For of (name * exp * exp * exp)
  | Assert of (exp)
  | SetGlobal of (exp * int) (* privé *)
  | ReadGlobal of (int)  (* privé *)
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

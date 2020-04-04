
type prog = tmodule list
and tmodule = { mod_name : name ;
                decls : decl list }
and name = string
and patern = (name * Types.typ option)

and decl = 
  | Exp of (exp)
  | DefVar of (patern * exp)
  | DefFun of    ((name * name list * exp) list)
  | DefFunRec of ((name * name list * exp) list)
  | Type of (name * Types.typ)
and exp = 
  | Annotation of (exp * Types.typ)
  | Constant of (constant)
  | Ident of (name)
  | Let of (patern * exp * exp)
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
and ty =
  | Sum of (name list)
  | Ident_ty of string
  | Star_ty of (ty list)
  | Arrow_ty of (ty * ty)
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

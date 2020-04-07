type prog = {
    modules : tmodule list
  }
          
and tmodule = {
    mod_name : Ast.name ;
    decls: decl list ;
    init : Ast.name list
  }



and decl = 
  | DefFun of (Ast.name * arity * exp)
and arity = int
and exp = 
  | Constant of constant
  | Variable of var
  | GFun of (Ast.name)
  | Fun of (exp * int * int)
  | Let of (int * exp * exp)
  | App of (exp * exp list)
  | If of (exp * exp * exp)
  | BinOp of (Ast.binop * exp * exp)
  | UnOp of (Ast.unop * exp)
  | Seq of (exp * exp)
  | While of (exp * exp)
  | SetGlobal of (exp * int)
  | ReadGlobal of (int)
and constant = 
  | Unit 
  | Bool of bool 
  | Int of int 
  | Array_empty
  | List_empty
  (* | FunValue of int *)
and var = 
  | Global of string
  | Argument of int 
  | Local of int
  | Free of int

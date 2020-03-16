type name = string

type op = Add | Sub | Lt | Le | Eq | Ge | Gt | Or | And | Lor | Land

module Ast = struct  
  type prog = tmodule list 
  and tmodule = {mod_name : name ; decls : decl list }
  and decl = 
  | Exp of (exp)
  | GVar of (name * exp)
  | GFun of (name * name list * exp)
  | Type of (name * ty)
  and ty = name list
  and exp = 
  | Constant of constant
  | Ident of (name)
  | Let of (name * exp * exp)
  | App of (exp * exp list)
  | If of (exp * exp * exp)
  | Match of (exp * (patern * exp) list)
  | Op of (op * exp list)
  | Ref of (exp)
  | Bloc_alloc of (exp)
  | Assign of (exp * exp * exp)
  | Access of (exp * exp)
  | Seq of (exp * exp)
  | While of (exp * exp)
  | For of (name * exp * exp * exp)
  and constant = 
  | Bool of bool
  | Int of int
  | Constr of string
  and patern = constant
end

module Print_ast = struct
  open Ast

  let sptf = Printf.sprintf

  let indent_factor = 2

  let indent_string (level:int) : string =
    String.make (level * indent_factor) ' '

  let mcat c f l = String.concat c (List.map f l)
  
  let rec sprint_prog ms = List.map (sprint_module 0) ms
  and sprint_module lvl {mod_name;decls} =
     sptf "%smodule %s = struct\n%s\n%send" (indent_string lvl) mod_name 
        (mcat ";;" (sprint_decl (lvl+1)) decls) (indent_string lvl) 
  and sprint_decl lvl = function
  | GVar (name,e) -> sptf "%slet %s = %s" (indent_string lvl) name (sprint_exp lvl e)
  | GFun (name,args,e) -> sptf "%slet %s %s = %s" (indent_string lvl) name (String.concat " " args) (sprint_exp lvl e)
  | Exp (e) ->  sptf "let () = %s" (sprint_exp lvl e)
  and sprint_exp lvl = function
  | Constant(Bool b) -> sptf "%b" b
  | Constant(Int n) -> sptf "%d" n
  | Ident name -> name
  | Let(name,e1,e2) -> sptf "(let %s = %s in\n%s%s)" name
                            (sprint_exp 0 e1)
                            (indent_string lvl)
                            (sprint_exp (lvl+1) e1)
  | App(e,args) -> sptf "(%s %s)"
                   (sprint_exp lvl e)
                   (mcat " " (sprint_exp (lvl+1)) args) 
  | If(e1,e2,e3) -> sptf "(if %s\n%sthen %s \n%selse %s)"
                         (sprint_exp 0 e1)
                         (indent_string lvl) (sprint_exp 0 e2)
                         (indent_string lvl) (sprint_exp 0 e3)
  | Ref(e) -> sptf "(ref %s)" (sprint_exp 0 e)
  | Assign(e1,e2,e3) -> sptf "((%s).(%s) <- %s)" 
                         (sprint_exp lvl e1)
                        (sprint_exp (lvl+1) e2)
                        (sprint_exp (lvl+2) e3)
  | Access(e1,e2) -> sptf "(! %s)" (sprint_exp 0 e1)
  | Seq(e1,e2) -> sptf "(%s; %s)" (sprint_exp lvl e1) (sprint_exp (lvl+1) e2)
  | While(e1,e2) -> sptf "while %s do\n%s\n%sdone"
                   (sprint_exp lvl e1) (sprint_exp (lvl+1) e2) (indent_string lvl)
  | For(x,e1,e2,e3) -> sptf "for %s = %s to %s do\n%s\n%sdone" x
                   (sprint_exp 0 e1) (sprint_exp lvl e2) (sprint_exp (lvl+1) e3) 
                   (indent_string lvl)
end
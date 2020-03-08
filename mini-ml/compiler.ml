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
  (* | Match of (exp * (patern * exp) list)*)
  (* | Op of (op * exp list)*)
  | Ref(e) -> sptf "(ref %s)" (sprint_exp 0 e)
  (*| Bloc_alloc of (exp)*)
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





(*






(* ************************************************** *)













type name = string

type op = Add | Sub | Lt | Le | Eq | Ge | Gt | Or | And | Lor | Land

module Ast = struct  

  type prog = tmodule list 
  and tmodule = {mod_name : name ; decls : decl list }
  and decl = 
  | GlobalVoidExpr of (exp)
  | Def of (name * exp)
  | VoidExpr of (exp)
  | Defun of (name * name list * exp)
  | TypeEnum of (name * ty)
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
  and patern = PInt of int | Constr of string
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
        (mcat "\n" (sprint_decl (lvl+1)) decls) (indent_string lvl) 
  and sprint_decl lvl = function
  | Def (name,e) -> sptf "%slet %s = %s" (indent_string lvl) name (sprint_exp lvl e)
  | Defun (name,args,e) -> sptf "%slet %s %s = %s" (indent_string lvl) name (String.concat " " args) (sprint_exp lvl e)
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
  (* | Match of (exp * (patern * exp) list)*)
  (* | Op of (op * exp list)*)
  | Ref(e) -> sptf "(ref %s)" (sprint_exp 0 e)
  (*| Bloc_alloc of (exp)*)
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

module Kast = struct  
  type prog = tmodule list 
  and tmodule = { mod_name : name ; decls: decl list ; init: exp list } (* stocker module name *)
  and decl = 
  | Def of (name)
  | Defun of (name * name list * exp)
  and exp = 
  | Constant of constant
  | Variable of var
  | Let of (int * exp * exp)
  | App of (exp * exp list)
  | If of (exp * exp * exp)
  | Match of (exp * (patern * exp) list)
  | Op of (op * exp list)
  | Bloc_alloc of (exp)
  | Assign of (exp * exp * exp)
  | Access of (exp * exp)
  | Seq of (exp * exp)
  | While of (exp * exp)
  and constant = 
  | Bool of bool
  | Int of int
  and patern = constant
  and var = Argument of int | Local of int | Global of int * string
end

module Ast2Kast = struct

  let gensym = 
    let c = ref 0 in 
    (fun ~prefix -> incr c; Printf.sprintf "__%s%d" prefix !c)

  module Env = struct
    type t = { locals : (name * int) list;
               constrs : (name * int) list;
               globals : (name * int) list; }
    let create () = { locals=[];constrs=[];globals=[] }

    let add_local env x =
      let len = List.length env.locals in
      let env' = {env with locals=(x,len-1) :: env.locals} in
      (len, env')

    let add_constr env x =
       let len = List.length env.constrs in
       {env with constrs=(x,len-1) :: env.constrs}

    let add_global env x =
       let len = List.length env.globals in
       {env with globals=(x,len-1) :: env.globals}

    let fetch env name = 
      List.assoc_opt name env.locals

    let gfetch env name = 
      List.assoc_opt name env.globals
  end

  let rec (* rewrite_module env {decls;mod_name} = match decls with
  | [] -> []
  | d::t -> let env',k = rewrite_decl env d in 
            match k with
            | None -> rewrite_prog env' t
            | Some c -> c :: (rewrite_prog env' t)
  and *) rewrite_decl env = function
  | Ast.TypeEnum (_,names) -> let env' = List.fold_left Env.add_constr env names in (env',None)
  | Ast.VoidExpr (e) -> rewrite_decl env @@
                        let name = gensym ~prefix:"voidExpr" in Ast.Def (name,e)
  | Ast.Def (name,e) -> let ke = rewrite_exp env e in
                        let env' = Env.add_global env name in
                        (env', Some (Kast.Defun(name,[],ke)))
  | Ast.Defun (name,args,e) -> let ke = rewrite_exp env e in 
                               (env,Some (Kast.Defun (name,args,ke)))
  and rewrite_exp env = function
  | Ast.(Constant (Int n)) -> Kast.(Constant(Int n))
  | Ast.(Constant (Bool b)) -> Kast.(Constant(Bool b)) 
  | Ast.Ident (name) -> (match Env.fetch env name with
                         | None -> (match Env.gfetch env name with
                                    | None -> assert false
                                    | Some i -> Kast.Variable(Kast.Global (i,name)))
                         | Some i -> Kast.Variable(Kast.Local i))
  | Ast.Let(name,e1,e2) -> let i,env' = Env.add_local env name in 
                       Kast.Let(i,rewrite_exp env e1, rewrite_exp env' e2)
  | Ast.App(e,args) -> Kast.App(rewrite_exp env e, List.map (rewrite_exp env) args)
  | Ast.If(e1,e2,e3) -> Kast.If(rewrite_exp env e1,
                                rewrite_exp env e2,
                                rewrite_exp env e3)
  | Ast.Op(op,args) -> Kast.Op(op,List.map (rewrite_exp env) args)
  | Ast.Assign(e1,e2,e3) -> Kast.Assign(rewrite_exp env e1,
                                        rewrite_exp env e2,
                                        rewrite_exp env e3)
  | Ast.Access(e1,e2) -> Kast.Access(rewrite_exp env e1,
                                     rewrite_exp env e2)
  | Ast.Bloc_alloc(e) -> Kast.Bloc_alloc(rewrite_exp env e)
  | Ast.Ref(e) -> rewrite_exp env @@
                  let k = gensym ~prefix:"tmp" in
                  Ast.(Let(k,
                           Bloc_alloc(Constant(Int(1))), 
                           e))
  | Ast.Seq(e1,e2) -> Kast.Seq(rewrite_exp env e1,
                               rewrite_exp env e2)
  | Ast.While(e1,e2) -> Kast.While(rewrite_exp env e1,
                                   rewrite_exp env e2)
  | Ast.For(name,e0,e1,e2) -> 
      rewrite_exp env @@
      let name_zz = gensym ~prefix:name in
      let len_zz = gensym ~prefix:"len" in
      let open Ast in
      Let(name_zz, Ref(e0),
              Let(len_zz,
                  e1,
                  While(Op (Le, [ Access(Ident(name_zz),Constant(Int(0))); 
                                   Ident(len_zz) ]),
                        Let(name,
                            Access(Ident(name_zz),Constant(Int(0))),
                            Seq(e2,
                                Assign(Ident(name_zz),Constant(Int(0)),
                                Op(Add,[ Access(Ident(name_zz),Constant(Int(0))); Constant(Int(1)) ])))))))

end




module Bytecode = struct
  type bytecode = instr list
                
  and instr =
    | Push of segment
    | Pop of segment
    | Label of label
    | IfGoto of label
    | Goto of label
    | Return
    | Function of fun_name * int (* nb locales *)
    | Call of fun_name * int (* arité *)
    | Op of vm_operator
            
  and segment =
    | Anywhere
    | Argument of int
    | Constant of int
    | Static of int
    | Local of int
    | That of int
    | Temp of int
    | Pointer of int
               
  and label = string
  and fun_name = string

  and vm_operator = Add | Sub | Eq | Gt | Lt | And | Or | Not
end

(*
module Kast2Bytecode = struct
  open Bytecode

  let mapcat f l = List.concat (List.map f l)

  let gensym = 
    let c = ref 0 in 
    (fun prefix -> incr c; Printf.sprintf "%s%d" prefix !c)

  let rec bytecode_of_exp = function
  | Kast.Constant c -> bytecode_of_constant c
  | Kast.Variable v -> bytecode_of_variable v
  | Kast.If(e1,e2,e3) -> let bc_e1 = bytecode_of_exp e1 
                         and bc_e2 = bytecode_of_exp e2
                         and bc_e3 = bytecode_of_exp e3 in
                         let lbl_if_false = gensym "IfFalse"
                         and lbl_end = gensym "IfEnd" in
                         bc_e1 @ [Op Not; IfTrue lbl_if_false] @
                         bc_e2 @ [Goto lbl_end; Label lbl_if_false] @
                         bc_e3 @ [Label lbl_end]
  | Kast.While(e1,e2) -> let bc_e1 = bytecode_of_exp e1 
                         and bc_e2 = bytecode_of_exp e2 in
                         let lbl_begin = gensym "WhileBegin"
                         and lbl_end = gensym "WhileEnd" in
                         [Label lbl_begin] @ bc_e1 @ [Op Not; IfTrue lbl_end] @
                          bc_e2 [Goto lbl_begin; Label lbl_end]
  | Kast.Let(n,e1) -> let bc_e1 = bytecode_of_exp e1 in
                      bc_e1 @ [Pop(Local n)]
  | Kast.Seq(e1,e2) -> let bc_e1 = bytecode_of_exp e1 
                       and bc_e2 = bytecode_of_exp e2 in
                       bc_e1 @ [Pop Anywhere] @ bc_e2
  | Kast.App(f,args) -> let arity = List.length args in
                        mapcat bytecode_of_exp args @ 
                        (match f with
                         | GlobalFun (name) -> [Call(name,arity)]
                         | _ -> assert false)
  and bytecode_of_constant = function
  | Kast.Int n -> [Push (Constant n)]
  | Kast.Bool b -> if b = 0 then [Push(Constant 0)] else [Push(Constant 0);Op(Not)]


end*)











*)
open Utils


type prog = decl list
and decl = name * exp
and exp =  
| Val of value
| Var of var
| Lam of exp
| App of exp * exp
| If of exp * exp * exp
| While of exp * exp
| Global of name
and var = {index : int; name : name }

module Lenv = struct
  type t = {depth : int; vars : (name * int) list }

  let empty = {depth=0;vars=[]}

  let kvar name {depth=d;vars=l} =
    match List.assoc_opt name l with
    | Some i -> Var {index=i;name=name}
    | None -> Global name

  let extend name {depth=d;vars=l} =
    {depth=d+1;vars=(name,d)::l}
end
            
(** [kast a] convertit l'ast [a] en ast noyau *)
let rec kast ast = List.map kdecl ast
and kdecl (d,e) = (d,kexp Lenv.empty e)
and kexp lenv = function
  | Ast.Val v -> Val v
  | Ast.Var name -> Lenv.kvar name lenv
  | Ast.Lam (x,e) -> let e' = kexp (Lenv.extend x lenv) e
                     in Lam (e')
  | Ast.App (e1,e2) -> let e1' = kexp lenv e1
                       and e2' = kexp lenv e2
                       in App(e1',e2')  
  | Ast.If (e1,e2,e3) -> let e1' = kexp lenv e1
                         and e2' = kexp lenv e2 
                         and e3' = kexp lenv e3
                         in If (e1',e2',e3')
  | Ast.While (e1,e2) -> let e1' = kexp lenv e1
                         and e2' = kexp lenv e2
                         in While (e1',e2')
  | Ast.Let ((name,e1),e2) -> let ast_let = Ast.App(Ast.Lam(name,e2),e1)
                              in kexp lenv ast_let
  | Ast.Rec ((name,e1),e2) -> let ast_rec = Ast.App(Ast.Lam(name,e2),e1)
                              and lenv' = Lenv.extend name lenv
                              in kexp lenv' ast_rec
                                   

                                   


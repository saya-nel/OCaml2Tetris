(* Comme la compilation d'une variable globale *)
(* engendre un code complexe comportant plusieurs fonctions *)
(* (pour ne calculer qu'une fois l'expression d'initialisation), *)
(* on réécrit le noeud Ast.DefVar en une fonction d'arité 0 *) 
(* si l'expression est une constante *)
(* (puisque l'initialisation ne réalise ni calcul ni effet de bord) *)

let rw_decl d = 
 match d with
  | Ast.DefVar(v,e) -> (match e with 
                        | Ast.Constant(c) -> 
                          (match c with 
                           | Ast.String s -> d 
                           | _ -> (Ast.DefFun([Ast.DF (v,[],e)])))
                        | _ -> Ast.DefVar(v,e))
  | Ast.DefFun(l) -> Ast.DefFun(l)
  | Ast.DefFunRec(l) -> Ast.DefFunRec(l)
  | _ -> d 

let rewrite m =
  match m with 
  | Ast.Module(mod_name,decls) ->
    let decls = (List.map rw_decl decls) in
    Ast.Module(mod_name,decls)

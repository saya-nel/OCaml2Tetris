open Ast

type collect = Kast.var list 

let coll = ref []

let rec collect lenv cenv e =
  coll := [];
  collect_exp lenv cenv e;
  !coll
and collect_exp lenv cenv = function
  | Ast.Annotation (e,ty) -> collect_exp lenv cenv e
  | Constant _ -> ()
  | Ident name -> 
    if not (List.mem name cenv) || List.mem name lenv 
    then coll := name :: !coll
  | Let((name,_),e1,e2) -> 
    let cenv' = name :: cenv in 
    collect_exp lenv cenv' e1;
    collect_exp lenv cenv' e2 
  | Fun((name,_),e) ->
    let cenv' = name :: cenv in 
    collect_exp lenv cenv' e
  | BinOp(op,e1,e2) ->
    collect_exp lenv cenv e1;
    collect_exp lenv cenv e2
  | UnOp(op,e1) ->
    collect_exp lenv cenv e1
  | App(e,args) ->
     collect_exp lenv cenv e;
     List.iter (collect_exp lenv cenv) args
  | If(e1,e2,e3) ->
    collect_exp lenv cenv e1;
    collect_exp lenv cenv e2;
    collect_exp lenv cenv e3
  | Ref(e) ->
    collect_exp lenv cenv e
  | Ref_access(e) ->
    collect_exp lenv cenv e
  | Ref_assign(e1,e2) ->
    collect_exp lenv cenv e1;
    collect_exp lenv cenv e2
  | Pair(e1,e2) ->
    collect_exp lenv cenv e1;
    collect_exp lenv cenv e2
  | Cons(e1,e2) ->
    collect_exp lenv cenv e1;
    collect_exp lenv cenv e2
  | Array_create(es) ->
     List.iter (collect_exp lenv cenv) es
  | Array_assign(e1,e2,e3) ->
    collect_exp lenv cenv e1;
    collect_exp lenv cenv e2;
    collect_exp lenv cenv e3
  | Array_access(e1,e2) ->
    collect_exp lenv cenv e1;
    collect_exp lenv cenv e2
  | Seq(e1,e2) ->
    collect_exp lenv cenv e1;
    collect_exp lenv cenv e2
  | While(e1,e2) ->
    collect_exp lenv cenv e1;
    collect_exp lenv cenv e2
  | For(x,e1,e2,e3) ->
    let cenv' = x :: cenv in 
    collect_exp lenv cenv' e1;
    collect_exp lenv cenv' e2;
    collect_exp lenv cenv' e3  
  | Assert(e,_) ->
     collect_exp lenv cenv e
  | Match(e,cases) ->
    collect_exp lenv cenv e;
    (List.iter 
      (function Case (_,e) | Otherwise (e) -> collect_exp lenv cenv e)
     cases)
  | Array_alloc _ | SetGlobal _ | ReadGlobal _ -> 
     failwith "bug : AST interne (noeud temporaire pour la réécriture en Kast), ne devrait pas pouvoir être construit par le parseur, ni affiché ..."
open Ast

type collect = string list 

let create () = ref []

let rec collect env lenv e =
  let coll = create () in 
  collect_exp coll env lenv e;
  !coll
and collect_exp coll env lenv = function
  | Ast.Constant _ -> ()
  | Ast.Ident name -> 
    if not (List.mem name lenv) || List.mem name env 
    then coll := name :: !coll
  | Ast.Let(name,e1,e2) -> 
    let lenv' = name :: lenv in 
    collect_exp coll env lenv' e1;
    collect_exp coll env lenv' e2 
  | Ast.Fun(name,e) ->
    let lenv' = name :: lenv in 
    collect_exp coll env lenv' e
  | Ast.BinOp(op,e1,e2) ->
    collect_exp coll env lenv e1;
    collect_exp coll env lenv e2
  | Ast.UnOp(op,e1) ->
    collect_exp coll env lenv e1
  | Ast.App(e,args) ->
     collect_exp coll env lenv e;
     List.iter (collect_exp coll env lenv) args
  | Ast.If(e1,e2,e3) ->
    collect_exp coll env lenv e1;
    collect_exp coll env lenv e2;
    collect_exp coll env lenv e3
  | Ast.Ref(e) ->
    collect_exp coll env lenv e
  | Ast.Ref_access(e) ->
    collect_exp coll env lenv e
  | Ast.Ref_assign(e1,e2) ->
    collect_exp coll env lenv e1;
    collect_exp coll env lenv e2
  | Ast.Pair(e1,e2) ->
    collect_exp coll env lenv e1;
    collect_exp coll env lenv e2
  | Ast.Cons(e1,e2) ->
    collect_exp coll env lenv e1;
    collect_exp coll env lenv e2
  | Ast.Array_create(es) ->
     List.iter (collect_exp coll env lenv) es
  | Ast.Array_assign(e1,e2,e3) ->
    collect_exp coll env lenv e1;
    collect_exp coll env lenv e2;
    collect_exp coll env lenv e3
  | Ast.Array_access(e1,e2) ->
    collect_exp coll env lenv e1;
    collect_exp coll env lenv e2
  | Ast.Seq(e1,e2) ->
    collect_exp coll env lenv e1;
    collect_exp coll env lenv e2
  | Ast.While(e1,e2) ->
    collect_exp coll env lenv e1;
    collect_exp coll env lenv e2
  | Ast.For(x,e1,e2,e3) ->
    let lenv' = x :: lenv in 
    collect_exp coll env lenv' e1;
    collect_exp coll env lenv' e2;
    collect_exp coll env lenv' e3  
  | Ast.Assert(e,_) ->
     collect_exp coll env lenv e
  | Ast.Match(e,cases) ->
    collect_exp coll env lenv e;
    (List.iter 
      (function Ast.Case (_,e) | Ast.Otherwise (e) -> collect_exp coll env lenv e)
     cases)
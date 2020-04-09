(* extrait dans des variables globales les chaînes du programme, pour éviter qu'elles ne soient allouées plusieurs fois *)

let gensym = 
  let c = ref 0 in 
  (fun () ->
    incr c;
    Printf.sprintf "__static%d" !c)

let collect = ref []

let rec visit_tmodule Ast.{mod_name;decls} = 
  collect := [];
  let decls = List.map visit_decl decls in
  let decls = !collect @ decls in
  Ast.{mod_name;decls}

and visit_decl = function
  | Ast.Exp(e) -> Ast.Exp(visit_exp e)
  | Ast.DefVar(v,e) -> Ast.DefVar(v,visit_exp e)
  | Ast.DefFun(l) -> Ast.DefFun (visit_fundecs l)
  | Ast.DefFunRec(l) -> Ast.DefFunRec (visit_fundecs l)
and visit_fundecs l = 
  List.map (fun (name,args,e) -> (name,args,visit_exp e)) l 
and visit_exp = function
| Ast.Constant c -> visit_constant c
| Ast.Let(v,e1,e2) -> Ast.Let(v,visit_exp e1,visit_exp e2)
| Ast.App(e,args) -> Ast.App(visit_exp e,List.map visit_exp args) 
| Ast.If(e1,e2,e3) -> Ast.If(visit_exp e1,visit_exp e2,visit_exp e3)
| Ast.BinOp(op,e1,e2) -> Ast.BinOp(op,visit_exp e1,visit_exp e2)
| Ast.UnOp(op,e1) -> Ast.UnOp(op,visit_exp e1)
| Ast.Ref_access(e1) -> Ast.Ref_access(visit_exp e1)
| Ast.Ref_assign(e1,e2) -> Ast.Ref_assign(visit_exp e1,visit_exp e2)
| Ast.Ref(e) -> Ast.Ref(visit_exp e)
| Ast.Array_access(e1,e2) -> Ast.Array_access(visit_exp e1,visit_exp e2) 
| Ast.Array_assign(e1,e2,e3)  -> Ast.Array_assign(visit_exp e1,visit_exp e2,visit_exp e3)
| Ast.Pair(e1,e2) -> Ast.Pair(visit_exp e1,visit_exp e2) 
| Ast.Cons(e1,e2) -> Ast.Cons(visit_exp e1,visit_exp e2)
| Ast.Array_create(xs) -> Ast.Array_create(List.map visit_exp xs)
| Ast.Seq(e1,e2) -> Ast.Seq(visit_exp e1,visit_exp e2)
| Ast.While(e1,e2) -> Ast.While(visit_exp e1,visit_exp e2)
| Ast.For(name,e0,e1,e2) -> Ast.For(name,visit_exp e0,visit_exp e1,visit_exp e2)
| Ast.Match(e,ms) -> Ast.Match (visit_exp e,List.map (function Ast.Case(c,e) -> Ast.Case(c,visit_exp e) | Ast.Otherwise e -> Ast.Otherwise(visit_exp e)) ms) 
| Ast.Assert(e,pos) -> Ast.Constant(Ast.Unit)
| e -> e
and visit_constant = function
| (Ast.String _) as s -> 
  let k = gensym () in
  collect := (Ast.DefVar(k,Ast.Constant(s))):: !collect;
  Ast.Ident(k);
| c -> Ast.Constant(c)


(* extrait dans des variables globales les chaînes du programme, 
pour éviter qu'elles ne soient allouées plusieurs fois *)

let gensym = 
  let c = ref 0 in 
  (fun () ->
    incr c;
    Printf.sprintf "__static%d" !c)

let collect = ref []

(* globalise les chaînes de caractères dans le module mdl *)  

let rec rewrite ?(depth_max=5) m =
  match m with Ast.Module(mod_name,decls) ->
  collect := [];
  let decls = !collect @ List.map rw_decl decls in
  Ast.Module(mod_name,decls)


and rw_decl = function
  | Ast.DefVar(v,e) -> Ast.DefVar(v,rw_exp e)
  | Ast.DefFun(l) -> Ast.DefFun (rw_fundecs l)
  | Ast.DefFunRec(l) -> Ast.DefFunRec (rw_fundecs l)
  | d -> d 
and rw_fundecs l = 
  List.map (fun (name,args,e) -> (name,args,rw_exp e)) l 
and rw_exp = function
  | Ast.Constant c -> rw_constant c
  | Ast.Let(v,e1,e2) -> Ast.Let(v,rw_exp e1,rw_exp e2)
  | Ast.App(e,args) -> Ast.App(rw_exp e,List.map rw_exp args) 
  | Ast.If(e1,e2,e3) -> Ast.If(rw_exp e1,rw_exp e2,rw_exp e3)
  | Ast.BinOp(op,e1,e2) -> Ast.BinOp(op,rw_exp e1,rw_exp e2)
  | Ast.UnOp(op,e1) -> Ast.UnOp(op,rw_exp e1)
  | Ast.Ref_access(e1) -> Ast.Ref_access(rw_exp e1)
  | Ast.Ref_assign(e1,e2) -> Ast.Ref_assign(rw_exp e1,rw_exp e2)
  | Ast.Ref(e) -> Ast.Ref(rw_exp e)
  | Ast.Array_access(e1,e2) -> Ast.Array_access(rw_exp e1,rw_exp e2) 
  | Ast.Array_assign(e1,e2,e3)  -> Ast.Array_assign(rw_exp e1,rw_exp e2,rw_exp e3)
  | Ast.Pair(e1,e2) -> Ast.Pair(rw_exp e1,rw_exp e2) 
  | Ast.Cons(e1,e2) -> Ast.Cons(rw_exp e1,rw_exp e2)
  | Ast.Array_create(xs) -> Ast.Array_create(List.map rw_exp xs)
  | Ast.Seq(Ast.Constant(Ast.Unit),e2) -> rw_exp e2
  | Ast.Seq(e1,e2) -> Ast.Seq(rw_exp e1,rw_exp e2)
  | Ast.While(e1,e2) -> Ast.While(rw_exp e1,rw_exp e2)
  | Ast.For(name,e0,e1,e2) -> Ast.For(name,rw_exp e0,rw_exp e1,rw_exp e2)
  | Ast.Match(e,ms) -> Ast.Match (rw_exp e,List.map (function Ast.Case(c,e) -> Ast.Case(c,rw_exp e) | Ast.Otherwise e -> Ast.Otherwise(rw_exp e)) ms) 
  | Ast.Assert(e,pos) -> Ast.Constant(Ast.Unit)
  | e -> e
and rw_constant = function
  | (Ast.String _) as s -> 
     let k = gensym () in
     collect := (Ast.DefVar(k,Ast.Constant(s))):: !collect;
     Ast.Ident(k);
  | c -> Ast.Constant(c)


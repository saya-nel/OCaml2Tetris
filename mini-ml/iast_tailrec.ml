exception Abort

let rec rewrite_tmodule Iast.{mod_name;decls} = 
  let decls = List.map rewrite_decl decls in
  Iast.{mod_name;decls}
 
and rewrite_decl d = match d with
  | Iast.DefFunRec l -> 
    Iast.DefFunRec (List.map (fun ((name,args,e) as f) -> 
                                (try let f' = rewrite_defun f in 
                                 (name,args,f')
                                 with Abort -> f)) l )
  | d -> d
and rewrite_defun (name,args,e) =
  Iast.Ext(Iast.Label(name,rewrite_exp name args e))
  (* List.fold_right
            (fun x e -> Iast.Let (x,Iast.Ident(x),e))
            args (Iast.Seq (Iast.Ext(Iast.Label(name)),(rewrite_exp name args e)))
*)
and rewrite_exp f args exp = 
let rec rewrite_exp exp = match exp with 
  | Iast.Constant(c) -> exp
  | Iast.Ident(name) -> if f = name then raise Abort else exp
    (* if name = f 
    then Iast.Goto(name)
    else*)
  | Iast.Let(name,e1,e2) -> 
    if name = f then Iast.Let(name,rewrite_exp e1,e2)
    else Iast.Let(name,e1,rewrite_exp e2)
  | Iast.Fun(name,e) -> Iast.Fun(name,e)
  | Iast.App(Iast.Ident name,argv) -> 
    if name = f 
    then (Iast.Ext(Iast.Goto(name,argv))) else exp
  | Iast.App(e,args) -> exp
  | Iast.If(e1,e2,e3) ->
     Iast.If(e1,rewrite_exp e2,rewrite_exp e3)
  | Iast.BinOp(op,e1,e2) -> exp
  | Iast.UnOp(op,e1) -> exp
  | Iast.Ref_access(e1) -> exp
  | Iast.Ref_assign(e1,e2) -> exp
  | Iast.Ref(e) -> exp
  | Iast.Array_access(e1,e2) -> exp
  | Iast.Array_assign(e1,e2,e3) -> exp
  | Iast.Ext(ext) -> exp
  | Iast.Pair(e1,e2) -> exp
  | Iast.Cons(e1,e2) -> exp
  | Iast.Array_create(xs) -> exp
  | Iast.Seq(e1,e2) -> Iast.Seq(e1,rewrite_exp e2)
  | Iast.While(e1,e2) -> exp
  | Iast.For(name,e0,e1,e2) -> exp
  | Iast.Match (e,ms) ->
    let ms = List.map (function 
                       | Iast.Case (c,e) -> Iast.Case(c,rewrite_exp e)
                       | Iast.Otherwise e -> Iast.Otherwise (rewrite_exp e)) ms in
    Iast.Match (e,ms)          
  | Iast.Assert(e,pos) -> exp in rewrite_exp exp
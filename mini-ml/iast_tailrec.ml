exception Abort

let rec rewrite Iast.{mod_name;decls} = 
  let decls = List.map rw_decl decls in
  Iast.{mod_name;decls}
  
and rw_decl d = match d with
  | Iast.DefFunRec l -> 
     Iast.DefFunRec (List.map (fun ((name,args,e) as f) -> 
                         (try let f' = rw_defun f in 
                              (name,args,f')
                          with Abort -> f)) l )
  | d -> d
and rw_defun (name,args,e) =
  Iast.Ext(Iast.Label(name,rw_exp name args e))
and rw_exp f args exp = 
  let rec rw_exp exp = match exp with 
    | Iast.Constant(c) -> exp
    | Iast.Ident(name) -> if f = name then raise Abort else exp
    | Iast.Let(name,e1,e2) -> 
       if name = f then Iast.Let(name,rw_exp e1,e2)
       else Iast.Let(name,e1,rw_exp e2)
    | Iast.Fun(name,e) -> Iast.Fun(name,e)
    | Iast.App(Iast.Ident name,argv) -> 
       if name = f 
       then (Iast.Ext(Iast.Goto(name,argv))) else exp
    | Iast.App(e,args) -> exp
    | Iast.If(e1,e2,e3) ->
       Iast.If(e1,rw_exp e2,rw_exp e3)
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
    | Iast.Seq(e1,e2) -> Iast.Seq(e1,rw_exp e2)
    | Iast.While(e1,e2) -> exp
    | Iast.For(name,e0,e1,e2) -> exp
    | Iast.Match (e,ms) ->
       let ms = List.map
                  (function 
                   | Iast.Case (c,e) -> Iast.Case(c,rw_exp e)
                   | Iast.Otherwise e -> Iast.Otherwise (rw_exp e)) ms in
       Iast.Match (e,ms)          
    | Iast.Assert(e,pos) -> exp
  in
  rw_exp exp

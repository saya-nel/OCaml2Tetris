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
    | Iast.Ident(name) -> if f = name then raise Abort else exp
    | Iast.Let(name,e1,e2) -> 
       if name = f then Iast.Let(name,rw_exp e1,e2)
       else Iast.Let(name,e1,rw_exp e2)
    | Iast.App(Iast.Ident name,argv) -> 
       if name = f 
       then (Iast.Ext(Iast.Goto(name,argv))) else exp
   
    | Iast.If(e1,e2,e3) ->
      Iast.If(e1,rw_exp e2,rw_exp e3)
    | Iast.Seq(e1,e2) -> Iast.Seq(e1,rw_exp e2)
    | Iast.Match (e,ms) ->
       let ms = List.map
                  (function 
                   | Iast.Case (c,e) -> Iast.Case(c,rw_exp e)
                   | Iast.Otherwise e -> Iast.Otherwise (rw_exp e)) ms in
       Iast.Match (e,ms)          
    | _ -> exp
  in
  rw_exp exp

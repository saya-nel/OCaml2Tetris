
let gen_closure_id = 
  let c = ref 10000 in 
  (fun () -> incr c; !c)


let rec rewrite Iast.{mod_name;decls} = 
  let decls = List.map rw_decl decls in
  Iast.{mod_name;decls}

and rw_decl = function
  | Iast.DefVar(v,e) -> Iast.DefVar(v,rw_exp [] e)
  | Iast.DefFun(l) -> Iast.DefFun (rw_fundecs l)
  | Iast.DefFunRec(l) -> Iast.DefFunRec (rw_fundecs l)
  | Iast.Type(s,ty) -> Iast.Type(s,ty)
and rw_fundecs l = 
  List.map (fun (name,args,e) -> (name,args,(rw_exp []) e)) l 
and rw_exp env = function
  | Iast.Ident name -> Iast.Ident name
  | Iast.Fun (name,e) -> 
    (match env with 
     | [] -> Iast.Fun (name,rw_exp [name] e)
     | _ -> let env = name :: env in
            let addr = gen_closure_id () in
            let code = rw_exp env e in
            let closure_env = List.map (fun name -> Iast.Ident(name)) env in
            let closure = Iast.Constant(Iast.Int addr)::closure_env in
            Iast.Closure((addr,code),name,Iast.Array_create(closure)))
    (* Iast.Fun (name,rw_exp env e) *)
  | Iast.Constant c -> Iast.Constant (c)
  | Iast.Let(v,e1,e2) -> Iast.Let(v,rw_exp env e1,rw_exp env e2)
  | Iast.App(e,args) -> Iast.App(rw_exp env e,List.map (rw_exp env) args) 
  | Iast.If(e1,e2,e3) -> Iast.If(rw_exp env e1,rw_exp env e2,rw_exp env e3)
  | Iast.BinOp(op,e1,e2) -> Iast.BinOp(op,rw_exp env e1,rw_exp env e2)
  | Iast.UnOp(op,e1) -> Iast.UnOp(op,rw_exp env e1)
  | Iast.Ref_access(e1) -> Iast.Ref_access(rw_exp env e1)
  | Iast.Ref_assign(e1,e2) -> Iast.Ref_assign(rw_exp env e1,rw_exp env e2)
  | Iast.Ref(e) -> Iast.Ref(rw_exp env e)
  | Iast.Array_access(e1,e2) -> Iast.Array_access(rw_exp env e1,rw_exp env e2) 
  | Iast.Array_assign(e1,e2,e3) -> Iast.Array_assign(rw_exp env e1,rw_exp env e2,rw_exp env e3)
  | Iast.Pair(e1,e2) -> Iast.Pair(rw_exp env e1,rw_exp env e2) 
  | Iast.Cons(e1,e2) -> Iast.Cons(rw_exp env e1,rw_exp env e2)
  | Iast.Array_create(xs) -> Iast.Array_create(List.map (rw_exp env) xs)
  | Iast.Seq(e1,e2) -> Iast.Seq(rw_exp env e1,rw_exp env e2)
  | Iast.While(e1,e2) -> Iast.While(rw_exp env e1,rw_exp env e2)
  | Iast.For(name,e0,e1,e2) -> Iast.For(name,rw_exp env e0,rw_exp env e1,rw_exp env e2)
  | Iast.Match(e,ms) -> 
     Iast.Match (rw_exp env e, 
                 List.map (function Iast.Case(c,e) -> Iast.Case(c,rw_exp env e)
                                  | Iast.Otherwise e -> Iast.Otherwise(rw_exp env e)) ms) 
  | Iast.Assert(e,pos) -> Iast.Constant(Iast.Unit)
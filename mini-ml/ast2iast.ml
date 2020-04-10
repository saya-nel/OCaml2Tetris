
let gen_closure_id = 
  let c = ref 10000 in 
  (fun () -> incr c; !c)


let rec rewrite Ast.{mod_name;decls} = 
  let decls = List.map rw_decl decls in
  Iast.{mod_name;decls}

and rw_decl = function
  | Ast.DefVar(v,e) -> Iast.DefVar(v,rw_exp [] e)
  | Ast.DefFun(l) -> Iast.DefFun (rw_fundecs l)
  | Ast.DefFunRec(l) -> Iast.DefFunRec (rw_fundecs l)
  | Ast.Type(s,ty) -> Iast.Type(s,ty)
and rw_fundecs l = 
  List.map (fun (name,args,e) -> (name,args,(rw_exp []) e)) l 
and rw_exp env = function
  | Ast.Ident name -> Iast.Ident name
  | Ast.Fun (name,e) -> 
    (match env with 
     | [] -> Iast.Fun (name,rw_exp [name] e)
     | _ -> let env = env in
            let addr = gen_closure_id () in
            let code = rw_exp env e in
            let closure_env = List.map (fun name -> Iast.Ident(name)) env in
            let closure = Iast.Constant(Iast.Int addr)::closure_env in
            Iast.Closure((addr,code),name,Iast.Array_create(closure)))
    (* Iast.Fun (name,rw_exp env e) *)
  | Ast.Constant c -> Iast.Constant (rw_constant c)
  | Ast.Let(v,e1,e2) -> Iast.Let(v,rw_exp env e1,rw_exp env e2)
  | Ast.App(e,args) -> Iast.App(rw_exp env e,List.map (rw_exp env) args) 
  | Ast.If(e1,e2,e3) -> Iast.If(rw_exp env e1,rw_exp env e2,rw_exp env e3)
  | Ast.BinOp(op,e1,e2) -> Iast.BinOp(op,rw_exp env e1,rw_exp env e2)
  | Ast.UnOp(op,e1) -> Iast.UnOp(op,rw_exp env e1)
  | Ast.Ref_access(e1) -> Iast.Ref_access(rw_exp env e1)
  | Ast.Ref_assign(e1,e2) -> Iast.Ref_assign(rw_exp env e1,rw_exp env e2)
  | Ast.Ref(e) -> Iast.Ref(rw_exp env e)
  | Ast.Array_access(e1,e2) -> Iast.Array_access(rw_exp env e1,rw_exp env e2) 
  | Ast.Array_assign(e1,e2,e3) -> Iast.Array_assign(rw_exp env e1,rw_exp env e2,rw_exp env e3)
  | Ast.Pair(e1,e2) -> Iast.Pair(rw_exp env e1,rw_exp env e2) 
  | Ast.Cons(e1,e2) -> Iast.Cons(rw_exp env e1,rw_exp env e2)
  | Ast.Array_create(xs) -> Iast.Array_create(List.map (rw_exp env) xs)
  | Ast.Seq(e1,e2) -> Iast.Seq(rw_exp env e1,rw_exp env e2)
  | Ast.While(e1,e2) -> Iast.While(rw_exp env e1,rw_exp env e2)
  | Ast.For(name,e0,e1,e2) -> Iast.For(name,rw_exp env e0,rw_exp env e1,rw_exp env e2)
  | Ast.Match(e,ms) -> 
     Iast.Match (rw_exp env e, 
                 List.map (function Ast.Case(c,e) -> Iast.Case(rw_constant c,rw_exp env e)
                                  | Ast.Otherwise e -> Iast.Otherwise(rw_exp env e)) ms) 
  | Ast.Assert(e,pos) -> Iast.Constant(Iast.Unit)
and rw_constant = function
  | Ast.String(s) -> Iast.String(s)
  | Ast.Unit -> Iast.Unit
  | Ast.Int n -> Iast.Int n
  | Ast.Char c -> Iast.Char (c)
  | Ast.Constr name -> Iast.Constr name
  | Ast.Bool b -> Iast.Bool b 
  | Ast.Array_empty -> Iast.Array_empty
  | Ast.List_empty -> Iast.List_empty

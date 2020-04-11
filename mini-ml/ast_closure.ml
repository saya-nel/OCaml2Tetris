
let gen_closure_id = 
  let c = ref 10000 in 
  (fun () -> incr c; !c)


let rec rewrite m =
    match m with Ast.Module(mod_name,decls) ->
  let decls = List.map rw_decl decls in
  Ast.Module(mod_name,decls)

and rw_decl = function
  | Ast.DefVar(v,e) -> Ast.DefVar(v,rw_exp [] e)
  | Ast.DefFun(l) -> Ast.DefFun (rw_fundecs l)
  | Ast.DefFunRec(l) -> Ast.DefFunRec (rw_fundecs l)
  | Ast.Type(s,lvs,ty) -> Ast.Type(s,lvs,ty)
and rw_fundecs l = 
  List.map (fun (name,args,e) -> (name,args,(rw_exp []) e)) l 
and rw_exp env = function
  | Ast.Ident name -> Ast.Ident name
  | Ast.Fun (name,e) -> 
    (match env with 
     | [] -> Ast.Fun (name,rw_exp [name] e)
     | _ -> let env = name :: env in
            let addr = gen_closure_id () in
            let code = rw_exp env e in
            let closure_env = List.map (fun name -> Ast.Ident(name)) env in
            let closure = Ast.Constant(Ast.Int addr)::closure_env in
            Ast.Closure((addr,code),name,Ast.Block(closure)))
    (* Ast.Fun (name,rw_exp env e) *)
  | Ast.Constant c -> Ast.Constant (c)
  | Ast.Let(v,e1,e2) -> Ast.Let(v,rw_exp env e1,rw_exp env e2)
  | Ast.App(e,args) -> Ast.App(rw_exp env e,List.map (rw_exp env) args) 
  | Ast.If(e1,e2,e3) -> Ast.If(rw_exp env e1,rw_exp env e2,rw_exp env e3)
  | Ast.BinOp(op,e1,e2) -> Ast.BinOp(op,rw_exp env e1,rw_exp env e2)
  | Ast.UnOp(op,e1) -> Ast.UnOp(op,rw_exp env e1)
  | Ast.Block(xs) -> Ast.Block(List.map (rw_exp env) xs)
  | Ast.Seq(e1,e2) -> Ast.Seq(rw_exp env e1,rw_exp env e2)
  | Ast.While(e1,e2) -> Ast.While(rw_exp env e1,rw_exp env e2)
  | Ast.Match(e,ms) -> 
     Ast.Match (rw_exp env e, 
                 List.map (function Ast.Case(c,e) -> Ast.Case(c,rw_exp env e)
                                  | Ast.Otherwise e -> Ast.Otherwise(rw_exp env e)) ms) 
  | Ast.Assert(e,pos) -> Ast.Constant(Ast.Unit)
  | e -> e (* supposer ne pas contenir de valeur fonctionnelle à transformer. (à vérifier) *)

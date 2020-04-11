let gen_closure_id = 
  let c = ref 10000 in 
  (fun _ -> incr c; !c)

(* let rec rw_fundecs l = 
  List.map (fun f -> match f with (name,args,e) -> (name,args,(rw_exp []) e)) l  *)
let rec rw_exp env e = 
  match e with 
  | Ast.Ident (name) -> Iast.Ident(name)
  | Ast.Fun (name,e) -> let e0 = rw_exp [] e in
                        Iast.Fun (name,e0)
  | Ast.Constant (c) -> Iast.Constant (rw_constant c)
  | Ast.Let (v,e1,e2) -> let e1 = rw_exp env e1 in
                         let e2 = rw_exp env e2 in
                         Iast.Let(v,e1,e2)
  | Ast.App(e,args) -> let e = rw_exp env e in
                       let args = List.map (rw_exp env) args in
                       Iast.App(e,args)
  | Ast.If(e1,e2,e3) -> let e1 = rw_exp env e1 in
                        let e2 = rw_exp env e2 in
                        let e3 = rw_exp env e3 in
                        Iast.If(e1,e2,e3)
  | Ast.BinOp(op,e1,e2) -> let e1 = rw_exp env e1 in
                           let e2 = rw_exp env e2 in
                           Iast.BinOp(op,e1,e2)
  | Ast.UnOp(op,e1) -> let e1 = rw_exp env e1 in
                       Iast.UnOp(op,e1)
  | Ast.Ref_access(e1) -> let e1 = rw_exp env e1 in
                          Iast.Ref_access(e1)
  | Ast.Ref_assign(e1,e2) -> let e1 = rw_exp env e1 in
                             let e2 = rw_exp env e2 in
                             Iast.Ref_assign(e1,e2)
  | Ast.Ref(e1) -> let e1 = rw_exp env e1 in
                   Iast.Ref(e1)
  | Ast.Array_access(e1,e2) -> let e1 = rw_exp env e1 in
                               let e2 = rw_exp env e2 in
                               Iast.Array_access(e1,e2) 
  | Ast.Array_assign(e1,e2,e3) -> let e1 = rw_exp env e1 in
                                  let e2 = rw_exp env e2 in
                                  let e3 = rw_exp env e3 in
                                  Iast.Array_assign(e1,e2,e3)
  | Ast.Pair(e1,e2) -> let e1 = rw_exp env e1 in
                       let e2 = rw_exp env e2 in
                       Iast.Pair(e1,e2)
  | Ast.Cons(e1,e2) -> let e1 = rw_exp env e1 in
                       let e2 = rw_exp env e2 in
                       Iast.Cons(e1,e2)                   
  | Ast.Array_create(es) -> let es = List.map (rw_exp env) es in
                            Iast.Array_create(es)
  | Ast.Seq(e1,e2) -> let e1 = rw_exp env e1 in
                      let e2 = rw_exp env e2 in
                      Iast.Seq(e1,e2)  
  | Ast.While(e1,e2) -> let e1 = rw_exp env e1 in
                        let e2 = rw_exp env e2 in
                        Iast.While(e1,e2)  
  | Ast.For(name,e1,e2,e3) -> let e1 = rw_exp env e1 in
                              let e2 = rw_exp env e2 in
                              let e3 = rw_exp env e3 in
                              Iast.For(name,e1,e2,e3)
  | Ast.Match(e,ms) -> let e = rw_exp env e in 
                       let ms = List.map (rw_cases env) ms in
                       Iast.Match(e,ms)
  | Ast.Assert(e,pos) -> Iast.Constant(Iast.Unit)

and rw_cases env m = 
  match m with
  | Ast.Case(c,e) -> let c = rw_constant c in      
                     let e = rw_exp env e in
                     Iast.Case(c,e)
  | Ast.Otherwise (e) -> let e = rw_exp env e in
                         Iast.Otherwise(e)
and rw_constant c =
  match c with
  | Ast.String(s) -> Iast.String(s)
  | Ast.Unit -> Iast.Unit
  | Ast.Int (n) -> Iast.Int (n)
  | Ast.Char (c) -> Iast.Char (c)
  | Ast.Constr (name) -> Iast.Constr (name)
  | Ast.Bool (b) -> Iast.Bool (b)
  | Ast.Array_empty -> Iast.Array_empty
  | Ast.List_empty -> Iast.List_empty

(*
let _ = 
  let n = Ast.Int(42) in
  let c = Ast.Constant(n) in
  let l = Ast.Let ("foo",c,Ast.Ident("foo")) in
  rw_exp [] l *)

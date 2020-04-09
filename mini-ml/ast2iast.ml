let rec visit_tmodule Ast.{mod_name;decls} = 
  let decls = List.map visit_decl decls in
  Iast.{mod_name;decls}

and visit_decl = function
  | Ast.Exp(e) -> Iast.Exp(visit_exp e)
  | Ast.DefVar(v,e) -> Iast.DefVar(v,visit_exp e)
  | Ast.DefFun(l) -> Iast.DefFun (visit_fundecs l)
  | Ast.DefFunRec(l) -> Iast.DefFunRec (visit_fundecs l)
and visit_fundecs l = 
  List.map (fun (name,args,e) -> (name,args,visit_exp e)) l 
and visit_exp = function
  | Ast.Ident name -> Iast.Ident name
  | Ast.Fun (name,e) -> Iast.Fun (name,visit_exp e)
  | Ast.Constant c -> Iast.Constant (visit_constant c)
  | Ast.Let(v,e1,e2) -> Iast.Let(v,visit_exp e1,visit_exp e2)
  | Ast.App(e,args) -> Iast.App(visit_exp e,List.map visit_exp args) 
  | Ast.If(e1,e2,e3) -> Iast.If(visit_exp e1,visit_exp e2,visit_exp e3)
  | Ast.BinOp(op,e1,e2) -> Iast.BinOp(op,visit_exp e1,visit_exp e2)
  | Ast.UnOp(op,e1) -> Iast.UnOp(op,visit_exp e1)
  | Ast.Ref_access(e1) -> Iast.Ref_access(visit_exp e1)
  | Ast.Ref_assign(e1,e2) -> Iast.Ref_assign(visit_exp e1,visit_exp e2)
  | Ast.Ref(e) -> Iast.Ref(visit_exp e)
  | Ast.Array_access(e1,e2) -> Iast.Array_access(visit_exp e1,visit_exp e2) 
  | Ast.Array_assign(e1,e2,e3) -> Iast.Array_assign(visit_exp e1,visit_exp e2,visit_exp e3)
  | Ast.Pair(e1,e2) -> Iast.Pair(visit_exp e1,visit_exp e2) 
  | Ast.Cons(e1,e2) -> Iast.Cons(visit_exp e1,visit_exp e2)
  | Ast.Array_create(xs) -> Iast.Array_create(List.map visit_exp xs)
  | Ast.Seq(e1,e2) -> Iast.Seq(visit_exp e1,visit_exp e2)
  | Ast.While(e1,e2) -> Iast.While(visit_exp e1,visit_exp e2)
  | Ast.For(name,e0,e1,e2) -> Iast.For(name,visit_exp e0,visit_exp e1,visit_exp e2)
  | Ast.Match(e,ms) -> 
     Iast.Match (visit_exp e, 
                 List.map (function Ast.Case(c,e) -> Iast.Case(visit_constant c,visit_exp e)
                                  | Ast.Otherwise e -> Iast.Otherwise(visit_exp e)) ms) 
  | Ast.Assert(e,pos) -> Iast.Constant(Iast.Unit)
and visit_constant = function
  | Ast.String(s) -> Iast.String(s)
  | Ast.Unit -> Iast.Unit
  | Ast.Int n -> Iast.Int n
  | Ast.Char c -> Iast.Char (c)
  | Ast.Constr name -> failwith "todo"
  | Ast.Bool b -> Iast.Bool b 
  | Ast.Array_empty -> Iast.Array_empty
  | Ast.List_empty -> Iast.List_empty

let rec visit_tmodule Past.{mod_name;decls} = 
  let decls = let rec aux acc = function (* map filter *)
              | [] -> List.rev acc  
              | d::t -> (match visit_decl d with
                         | Some d -> aux (d::acc) t
                         | None -> aux acc t) in aux [] decls in
  Ast.{mod_name;decls}

and visit_decl Past.{decl_desc;decl_loc} = 
match decl_desc with
  | Past.Exp(e) -> Some (Ast.Exp(visit_exp e))
  | Past.DefVar(v,e) -> Some (Ast.DefVar(v,visit_exp e))
  | Past.DefFun(l) -> Some (Ast.DefFun ((visit_fundecs l)))
  | Past.DefFunRec(l) -> Some (Ast.DefFunRec ((visit_fundecs l)))
  | Past.Type _ -> None
and visit_fundecs l = 
  List.map (fun (name,args,t,e) -> (name,args,t,visit_exp e)) l 
and visit_exp Past.{exp_desc;exp_loc} =
  match exp_desc with
  | Past.Ident name -> Ast.Ident name
  | Past.Annotation (e,ty) -> Ast.Annotation (visit_exp e,ty)
  | Past.Constant c -> Ast.Constant(visit_cst c)
  | Past.Let(v,e1,e2) ->  Ast.Let(v,visit_exp e1,visit_exp e2)
  | Past.App(e,args) -> Ast.App(visit_exp e,List.map visit_exp args)
  | Past.Fun (var,e) -> Ast.Fun (var,visit_exp e) 
  | Past.If(e1,e2,e3) -> Ast.If(visit_exp e1,visit_exp e2,visit_exp e3)
  | Past.BinOp(op,e1,e2) -> Ast.BinOp(visit_binop op,visit_exp e1,visit_exp e2)
  | Past.UnOp(op,e1) -> Ast.UnOp(visit_unop op,visit_exp e1)
  | Past.Ref_access(e1) -> Ast.Ref_access(visit_exp e1)
  | Past.Ref_assign(e1,e2) -> Ast.Ref_assign(visit_exp e1,visit_exp e2)
  | Past.Ref(e) -> Ast.Ref(visit_exp e)
  | Past.Array_access(e1,e2) -> Ast.Array_access(visit_exp e1,visit_exp e2) 
  | Past.Array_assign(e1,e2,e3)  -> Ast.Array_assign(visit_exp e1,visit_exp e2,visit_exp e3)
  | Past.Array_alloc(e) -> Ast.Array_alloc(visit_exp e)
  | Past.Pair(e1,e2) -> Ast.Pair(visit_exp e1,visit_exp e2) 
  | Past.Cons(e1,e2) -> Ast.Cons(visit_exp e1,visit_exp e2)
  | Past.Array_create(xs) -> Ast.Array_create(List.map visit_exp xs)
  | Past.Seq(e1,e2) -> Ast.Seq(visit_exp e1,visit_exp e2)
  | Past.While(e1,e2) -> Ast.While(visit_exp e1,visit_exp e2)
  | Past.For(name,e0,e1,e2) -> Ast.For(name,visit_exp e0,visit_exp e1,visit_exp e2)
  | Past.Match (e,ms) -> Ast.Match (visit_exp e,List.map (function Past.Case(c,e) -> Ast.Case(visit_cst c,visit_exp e) | Past.Otherwise e -> Ast.Otherwise(visit_exp e)) ms) 
  | Past.Assert(e,pos) -> Ast.Assert(visit_exp e,pos) 
  | Past.Magic(e) -> visit_exp e
  | Past.SetGlobal(e,i) -> Ast.SetGlobal(visit_exp e,i)
  | Past.ReadGlobal(i) -> Ast.ReadGlobal(i) 
and visit_cst = function
| Past.Unit -> Ast.Unit
| Past.Bool b -> Ast.Bool b
| Past.Int n -> Ast.Int n 
| Past.Char c -> Ast.Char c 
| Past.String s -> Ast.String s
| Past.Constr name -> Ast.Constr name 
| Past.List_empty -> Ast.List_empty 
| Past.Array_empty -> Ast.Array_empty 
and visit_binop = function
  | Past.Add -> Ast.Add
  | Past.Minus -> Ast.Minus
  | Past.Mult -> Ast.Mult
  | Past.Div -> Ast.Div
  | Past.Eq -> Ast.Eq
  | Past.Neq -> Ast.Neq
  | Past.Gt -> Ast.Gt
  | Past.Ge -> Ast.Ge
  | Past.Lt -> Ast.Lt
  | Past.Le -> Ast.Le
  | Past.Or -> Ast.Or
  | Past.And -> Ast.And
  | Past.Lor -> Ast.Lor
  | Past.Land -> Ast.Land
and visit_unop = function
  | Past.Not -> Ast.Not
  | Past.UMinus -> Ast.UMinus

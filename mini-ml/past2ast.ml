let gensym = 
  let c = ref 0 in 
  (fun ~prefix ->
    incr c;
    Printf.sprintf "__%s%d" prefix !c)



let rec visit_tmodule Past.{mod_name;decls} = 
  let decls = let rec aux acc = function (* map filter *)
              | [] -> List.rev acc  
              | d::t -> let d' = visit_decl d in aux (d'::acc) t 
                        in aux [] decls in
  Ast.{mod_name;decls}

and visit_decl Past.{decl_desc;decl_loc} = 
match decl_desc with
  | Past.DefVar((name,_),e) -> Ast.DefVar(name,visit_exp e)
  | Past.DefFun(l) -> Ast.DefFun ((visit_fundecs l))
  | Past.DefFunRec(l) -> Ast.DefFunRec ((visit_fundecs l))
  | Past.Type (s,ty) -> Ast.Type (s,ty)
and visit_fundecs l = 
  List.map (fun (name,args,_,e) -> (name,List.map fst args,visit_exp e)) l 
and visit_exp Past.{exp_desc;exp_loc} =
  match exp_desc with
  | Past.Ident name -> Ast.Ident name
  | Past.Annotation (e,_) -> visit_exp e
  | Past.Constant c -> Ast.Constant(visit_cst c)
  | Past.Let((name,_),e1,e2) ->  Ast.Let(name,visit_exp e1,visit_exp e2)
  | Past.App(e,args) -> Ast.App(visit_exp e,List.map visit_exp args)
  | Past.Fun ((name,_),e) -> Ast.Fun (name,visit_exp e) 
  | Past.If(e1,e2,e3) -> Ast.If(visit_exp e1,visit_exp e2,visit_exp e3)
  | Past.BinOp(op,e1,e2) -> Ast.BinOp(visit_binop op,visit_exp e1,visit_exp e2)
  | Past.UnOp(op,e1) -> Ast.UnOp(visit_unop op,visit_exp e1)
  | Past.Ref_access(e1) -> Ast.Ref_access(visit_exp e1)
  | Past.Ref_assign(e1,e2) -> Ast.Ref_assign(visit_exp e1,visit_exp e2)
  | Past.Ref(e) -> Ast.Ref(visit_exp e)
  | Past.Array_access(e1,e2) -> Ast.Array_access(visit_exp e1,visit_exp e2) 
  | Past.Array_assign(e1,e2,e3)  -> Ast.Array_assign(visit_exp e1,visit_exp e2,visit_exp e3)
  | Past.Pair(e1,e2) -> Ast.Pair(visit_exp e1,visit_exp e2) 
  | Past.Cons(e1,e2) -> Ast.Cons(visit_exp e1,visit_exp e2)
  | Past.Array_create(xs) -> Ast.Array_create(List.map visit_exp xs)
  | Past.Seq(e1,e2) -> Ast.Seq(visit_exp e1,visit_exp e2)
  | Past.While(e1,e2) -> Ast.While(visit_exp e1,visit_exp e2)
  | Past.For(name,e0,e1,e2) -> Ast.For(name,visit_exp e0,visit_exp e1,visit_exp e2)
  | Past.Match (e,ms) -> visit_match e ms
  | Past.Assert(e,pos) -> Ast.Assert(visit_exp e,pos) 
  | Past.Magic(e) -> visit_exp e
and visit_match ec ms =
  (* possiblité d'éviter un let dans le cas (match x with ...) où x est un ident *)
  let name = gensym ~prefix:"match" in  (* !!!!!!!! *)
  Ast.Let (name,visit_exp ec,
  Ast.Match (Ast.If(Ast.BinOp(Ast.Le,
                              Ast.Ident(name),
                              Ast.Constant(Ast.Int(256))),
                    Ast.Ident(name),
                    Ast.Array_access(Ast.Ident(name),Ast.Constant(Ast.Int(0)))),
  List.map (function 
            | Past.Case(c,[],e) -> Ast.Case(visit_cst c,visit_exp e)
            | Past.Case(c,args,e) -> 
            let e = visit_exp e in
            let e' = List.fold_right2 (fun arg v e -> Ast.Let (arg,v,e)) 
            args (List.mapi (fun i _ -> Ast.Array_access(Ast.Ident(name),Ast.Constant(Ast.Int(i+1)))) args) e in
            (* TODO *)
            Ast.Case(visit_cst c,e') | Past.Otherwise e -> Ast.Otherwise(visit_exp e)) ms))
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

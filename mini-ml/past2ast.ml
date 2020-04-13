let gensym = 
  let c = ref 0 in 
  (fun ~prefix ->
    incr c;
    Printf.sprintf "__%s%d" prefix !c)

let compile_assertions = ref false

let rec visit_tmodule Past.{mod_name;decls} = 
  let rec aux acc env = function (* map filter *)
  | [] -> List.rev acc  
  | Past.{decl_desc}::ds -> 
    (match decl_desc with 
    | Past.DefVar((name,_),e) -> 
      aux (Ast.DefVar(name,rw_exp env e) :: acc) env ds
    | Past.DefFun(l) -> 
      aux (Ast.DefFun (visit_fundecs env l) :: acc) env ds
    | Past.DefFunRec(l) -> 
      aux (Ast.DefFunRec ((visit_fundecs env l)) :: acc) env ds
    | Past.Type (s,lvs,ty) -> 
      aux acc (match ty with 
               | Past.Exp_ty (_) -> env
               | Past.Sum (ls) -> (let rec aux2 n env lcs = 
                                   match lcs with 
                                   | [] -> env
                                   | (name,tys)::cs -> 
                                     let size = List.length tys in
                                     aux2 (n+1) ((name,(n,size)) :: env) cs 
                                   in
                                   aux2 0 env ls)) ds)
  in 
  let ds = aux [] [] decls in
  Ast.Module(mod_name,ds)

and visit_fundecs env l = 
  List.map (fun (name,args,_,e) -> (name,List.map fst args,rw_exp env e)) l 
and rw_exp env Past.{exp_desc;exp_loc} =
  match exp_desc with
  | Past.Ident name -> Ast.Ident name
  | Past.Annotation (e,_) -> rw_exp env e
  | Past.Constant c -> Ast.Constant(rw_cst env c)
  | Past.Let((name,_),e1,e2) ->  Ast.Let(name,rw_exp env e1,rw_exp env e2)
  | Past.App(e,args) -> 
    let args = List.map (rw_exp env) args in 
    (match e with 
    | Past.{exp_desc=Past.Constant(Past.Constr name)} -> 
     (let id,arity = List.assoc name env in
      let n = List.length args in
       let k = arity - n in 
       let rec aux extra = function
       | 0 -> Ast.Block(Ast.Constant(Ast.Int(id)) :: args @ extra)
       | k -> assert (n > 0);
               let name = gensym ~prefix:"__cstparam" in
               Ast.Fun (name,aux (Ast.Ident(name)::extra) (k-1)) 
       in aux [] k)
    | _ -> Ast.App(rw_exp env e,args))
  | Past.Fun ((name,_),e) -> Ast.Fun (name,rw_exp env e) 
  | Past.If(e1,e2,e3) -> Ast.If(rw_exp env e1,rw_exp env e2,rw_exp env e3)
  | Past.BinOp(op,e1,e2) -> Ast.BinOp(visit_binop op,rw_exp env e1,rw_exp env e2)
  | Past.UnOp(op,e1) -> Ast.UnOp(visit_unop op,rw_exp env e1)
  | Past.Ref_access(e1) -> Ast.App(Ast.Ident("Internal.array_get"),[rw_exp env e1;Ast.Constant(Ast.Int(0))])
  | Past.Ref_assign(e1,e2) -> Ast.App(Ast.Ident("Internal.array_set"),[rw_exp env e1;Ast.Constant(Ast.Int(0));rw_exp env e2])
  | Past.Ref(e) -> Ast.App(Ast.Ident("Pervasives.ref"),[rw_exp env e])
  | Past.Array_access(e1,e2) -> Ast.App(Ast.Ident("Internal.array_get"),[rw_exp env e1;rw_exp env e2])
  | Past.Array_assign(e1,e2,e3)  -> Ast.App(Ast.Ident("Internal.array_set"),[rw_exp env e1;rw_exp env e2;rw_exp env e3])
  | Past.Pair(e1,e2) -> Ast.App(Ast.Ident("Internal.pair"),[rw_exp env e1;rw_exp env e2]) 
  | Past.Cons(e1,e2) -> Ast.App(Ast.Ident("Internal.cons"),[rw_exp env e1;rw_exp env e2]) 
  | Past.Array_create(xs) -> Ast.Block(List.map (rw_exp env) xs)
  | Past.Seq(e1,e2) -> Ast.Seq(rw_exp env e1,rw_exp env e2)
  | Past.While(e1,e2) -> Ast.While(rw_exp env e1,rw_exp env e2)
  | Past.For(name,e0,e1,e2) ->
       let name_zz = gensym ~prefix:name in
       let len_zz = gensym ~prefix:"L" in
       Ast.Let(name_zz, 
        Ast.App(Ast.Ident("Pervasives.ref"),[rw_exp env e0]),
           Ast.Let(len_zz,rw_exp env e1,
               Ast.While(Ast.BinOp (Ast.Le,
                Ast.App(Ast.Ident("Pervasives.ref_contents"),[Ast.Ident(name_zz)]),
                Ast.Ident(len_zz)),
                     Ast.Let(name, Ast.App(Ast.Ident("Pervasives.ref_contents"),[Ast.Ident(name_zz)]),
                         Ast.Seq(rw_exp env e2,Ast.App(Ast.Ident("Pervasives.incr"),[Ast.Ident(name_zz)])))))) 
  | Past.Match (e,ms) -> visit_match env e ms
  | Past.Assert(e,pos) -> (* prevoir l'accès au nom du module *)
     if not !compile_assertions 
     then  Ast.Constant(Ast.Unit)
    else
    Ast.If(rw_exp env e,
              Ast.Constant(Ast.Unit),
              Ast.Seq(
                Ast.App(Ast.Ident("Pervasives.print_string"),
                  [Ast.Constant
                    (Ast.String 
                      (Printf.sprintf "assertion fail [%s]" (Past_print.sprint_exp 0 e)))]),
                Ast.Seq(Ast.App(Ast.Ident("Pervasives.print_newline"),[ Ast.Constant(Ast.Unit)]),
                Ast.Seq(Ast.App(Ast.Ident("Pervasives.print_string"),
                  [Ast.Constant
                    (Ast.String 
                      (Printf.sprintf "at %s. exit." (Parseutils.string_of_position pos)))]),
                Ast.App(Ast.Ident("Pervasives.exit"),
                       [Ast.Constant (Ast.Int(0))])))))
  | Past.Magic(e) -> rw_exp env e
and visit_match env ec ms =
  (* possiblité d'éviter un let dans le cas (match x with ...) où x est un ident *)
  let name = gensym ~prefix:"match" in  (* !!!!!!!! *)
  Ast.Let (
    name,
    rw_exp env ec,
    Ast.Match (
      Ast.If(
        Ast.BinOp(
          Ast.Le,
          Ast.Ident(name),
          Ast.Constant(Ast.Int(256))),
        Ast.Ident(name),
        Ast.App(Ast.Ident("Array.get"),[
            Ast.Ident(name);
            Ast.Constant(Ast.Int(0))])),
        List.map 
          (function 
          | Past.Case(c,[],e) -> Ast.Case(rw_cst env c,rw_exp env e)
          | Past.Case(c,args,e) -> 
            let e = rw_exp env e in
            let e' = List.fold_right2 (fun arg v e -> Ast.Let (arg,v,e)) 
            args (List.mapi (fun i _ -> Ast.App(Ast.Ident("Array.get"),[
                                          Ast.Ident(name);Ast.Constant(Ast.Int(i+1))])) args) e in
            (* TODO *) (* ? *)
            Ast.Case(rw_cst env c,e') | Past.Otherwise e -> Ast.Otherwise(rw_exp env e)) ms))

and rw_cst env = function
| Past.Unit -> Ast.Unit
| Past.Bool b -> Ast.Bool b
| Past.Int n -> Ast.Int n 
| Past.Char c -> Ast.Char c 
| Past.String s -> Ast.String s
| Past.Constr name -> let id,_ = List.assoc name env in Ast.Int(id)
| Past.List_empty -> Ast.Int(0)
| Past.List_cons -> Ast.Int(1)
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

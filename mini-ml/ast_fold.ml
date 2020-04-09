(* constant folding *)

let compile_assertions = ref false



let rec occurences v = function
| Ast.Constant c -> 0
| Ast.Let(name,e1,e2) -> if v = name then occurences v e1 else occurences v e1 + occurences v e2
| Ast.App(e,args) -> occurences v e + occurences_list v args
| Ast.If(e1,e2,e3) -> occurences v e1 + occurences v e2 + occurences v e3
| Ast.BinOp(op,e1,e2) -> occurences v e1 + occurences v e2
| Ast.UnOp(op,e1) -> occurences v e1 
| Ast.Ref_access(e1) -> occurences v e1 
| Ast.Ref_assign(e1,e2) -> occurences v e1 + occurences v e2
| Ast.Ref(e) -> occurences v e
| Ast.Array_access(e1,e2) -> occurences v e1 + occurences v e2
| Ast.Array_assign(e1,e2,e3)  -> occurences v e1 + occurences v e2 + occurences v e3
| Ast.Pair(e1,e2) -> occurences v e1 + occurences v e2
| Ast.Cons(e1,e2) -> occurences v e1 + occurences v e2
| Ast.Array_create(xs) -> occurences_list v xs
| Ast.Seq(e1,e2) -> occurences v e1 + occurences v e2
| Ast.While(e1,e2) -> occurences v e1 + occurences v e2
| Ast.For(name,e0,e1,e2) -> if v = name then 0 else occurences v e0 + occurences v e1 + occurences v e2
| Ast.Match(e,ms) -> occurences v e + occurences_list v (List.map (function Ast.Case(_,e) -> e | Ast.Otherwise e -> e) ms)
| Ast.Assert(e,pos) -> if !compile_assertions then occurences v e else 0
| Ast.Ident name -> if name = v then 1 else 0
| e -> 0
and occurences_list v l = 
  List.fold_left (fun acc e -> acc + occurences v e) 0 l




let replace v x e = 
  let rec replace = function
| Ast.Ident name -> if v = name then x else Ast.Ident name
| Ast.Constant c -> Ast.Constant c
| Ast.Let(name,e1,e2) -> Ast.Let(name,replace e1, if v = name then e2 else replace e2)
| Ast.App(e,args) -> Ast.App(replace e,List.map replace args) 
| Ast.If(e1,e2,e3) -> Ast.If(replace e1,replace e2,replace e3)
| Ast.BinOp(op,e1,e2) -> Ast.BinOp(op,replace e1,replace e2)
| Ast.UnOp(op,e1) -> Ast.UnOp(op,replace e1)
| Ast.Ref_access(e1) -> Ast.Ref_access(replace e1)
| Ast.Ref_assign(e1,e2) -> Ast.Ref_assign(replace e1,replace e2)
| Ast.Ref(e) -> Ast.Ref(replace e)
| Ast.Array_access(e1,e2) -> Ast.Array_access(replace e1,replace e2) 
| Ast.Array_assign(e1,e2,e3)  -> Ast.Array_assign(replace e1,replace e2,replace e3)
| Ast.Pair(e1,e2) -> Ast.Pair(replace e1,replace e2) 
| Ast.Cons(e1,e2) -> Ast.Cons(replace e1,replace e2)
| Ast.Array_create(xs) -> Ast.Array_create(List.map replace xs)
| Ast.Seq(e1,e2) -> Ast.Seq(replace e1,replace e2)
| Ast.While(e1,e2) -> Ast.While(replace e1,replace e2)
| Ast.For(name,e0,e1,e2) -> Ast.For(name,replace e0,replace e1,replace e2)
| Ast.Match(e,ms) -> Ast.Match (replace e,List.map (function Ast.Case(c,e) -> Ast.Case(c,replace e) | Ast.Otherwise e -> Ast.Otherwise(replace e)) ms) 
| Ast.Assert(e,pos) -> Ast.Constant(Ast.Unit)
| e -> e in replace e




let rec visit_tmodule Ast.{mod_name;decls} = 
  let decls = List.map visit_decl decls in
  Ast.{mod_name;decls}

and visit_decl = function
  | Ast.Exp(e) -> Ast.Exp(visit_exp e)
  | Ast.DefVar(v,e) -> Ast.DefVar(v,visit_exp e)
  | Ast.DefFun(l) -> Ast.DefFun (visit_fundecs l)
  | Ast.DefFunRec(l) -> Ast.DefFunRec (visit_fundecs l)
and visit_fundecs l = 
  List.map (fun (name,args,e) -> (name,args,visit_exp e)) l 
and visit_exp = function
| Ast.Ident name -> Ast.Ident name
| Ast.Constant c -> Ast.Constant c
| Ast.Let(v,e1,e2) -> 
                      let e1 = visit_exp e1 in
                      let e2 = visit_exp e2 in
                      (match e1 with 
                       | Ast.Assert _ | Ast.For _ | Ast.While _ 
                       | Ast.Array_assign _ | Ast.Ref_assign _ -> Ast.Seq (e1,visit_exp @@ replace v (Ast.Constant(Ast.Unit)) e2)
                       | _ ->
                      (match e1,occurences v e2 with
                      | Ast.Constant _,0 -> e2
                      | Ast.Constant _,_ | Ast.Ident _,_ -> visit_exp (replace v e1 e2)
                      | _ -> Ast.Let(v,e1,e2)))
| Ast.App(e,args) -> Ast.App(visit_exp e,List.map visit_exp args) 
| Ast.If(e1,e2,e3) -> (match visit_exp e1 with 
                      | Ast.Constant(Ast.Bool(b)) -> visit_exp (if b then e2 else e3)
                      | e -> Ast.If(e,visit_exp e2,visit_exp e3))
| Ast.BinOp(op,e1,e2) -> eval_binop (op,visit_exp e1,visit_exp e2)
| Ast.UnOp(op,e1) -> let x = visit_exp e1 in 
                     (match x with 
                      | Ast.Constant _ -> eval_unop (op,x)
                      | _ -> Ast.UnOp(op,x))
| Ast.Ref_access(e1) -> Ast.Ref_access(visit_exp e1)
| Ast.Ref_assign(e1,e2) -> Ast.Ref_assign(visit_exp e1,visit_exp e2)
| Ast.Ref(e) -> Ast.Ref(visit_exp e)
| Ast.Array_access(e1,e2) -> Ast.Array_access(visit_exp e1,visit_exp e2) 
| Ast.Array_assign(e1,e2,e3)  -> Ast.Array_assign(visit_exp e1,visit_exp e2,visit_exp e3)
| Ast.Pair(e1,e2) -> Ast.Pair(visit_exp e1,visit_exp e2) 
| Ast.Cons(e1,e2) -> Ast.Cons(visit_exp e1,visit_exp e2)
| Ast.Array_create(xs) -> Ast.Array_create(List.map visit_exp xs)
| Ast.Seq(e1,e2) -> Ast.Seq(visit_exp e1,visit_exp e2)
| Ast.While(e1,e2) -> (match visit_exp e1 with 
                       | Ast.Constant(Ast.Bool(false)) -> Ast.Constant(Ast.Unit)
                       | e -> Ast.While(e,visit_exp e2))
| Ast.For(name,e0,e1,e2) -> let e0' = visit_exp e0 in
                            let e1' = visit_exp e1 in
                            (match e0',e1' with 
                            | Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m)) 
                                when n = m -> Ast.Constant(Ast.Unit)
                            | _ -> Ast.For(name,e0',e1',visit_exp e2)) 
| Ast.Match(e,ms) ->  let rec aux acc = function
                     | Ast.Case(c,e)::t -> aux (Ast.Case(c,visit_exp e)::acc) t
                     | Ast.Otherwise(e)::_ -> List.rev (Ast.Otherwise(visit_exp e)::acc)
                     | [] -> Printf.printf "Warning 8: this pattern-matching is not exhaustive.\n"; 
                              List.rev ((Ast.Otherwise(Ast.App(Ast.Ident("Pervasives.failwith"),
                                                        [Ast.Constant(Ast.String("Match_failure, exit."))]))) :: acc) in
                     let ms' = aux [] ms in
                     (match visit_exp e with 
                      | Ast.Constant c -> (match List.find (function Ast.Case(c',_) -> c' = c | Ast.Otherwise _ -> true) ms' with
                                           | Ast.Case(_,e) -> e
                                           | Ast.Otherwise e -> e)
                      | e' -> Ast.Match(e',ms'))
| Ast.Assert(e,pos) -> 
     if !compile_assertions 
     then Ast.Assert(visit_exp e,pos) 
     else Ast.Constant(Ast.Unit)
| e -> e
and eval_binop = function
| (Ast.Add,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) -> Ast.Constant(Ast.Int(n+m))
| (Ast.Minus,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) -> Ast.Constant(Ast.Int(n-m))
| (Ast.Mult,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) -> Ast.Constant(Ast.Int(n*m))
| (Ast.Div,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) -> Ast.Constant(Ast.Int(n/m))
| (Ast.Lt,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) -> Ast.Constant(Ast.Bool(n < m))
| (Ast.Le,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) -> Ast.Constant(Ast.Bool(n <= m))
| (Ast.Neq,Ast.Constant(c1),Ast.Constant(c2)) -> Ast.Constant(Ast.Bool(c1 <> c2))
| (Ast.Eq,Ast.Constant(c1),Ast.Constant(c2)) -> Ast.Constant(Ast.Bool(c1 = c2))
| (Ast.Ge,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) -> Ast.Constant(Ast.Bool(n >= m))
| (Ast.Gt,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) -> Ast.Constant(Ast.Bool(n > m))
| (Ast.Or,Ast.Constant(Ast.Bool(p)),Ast.Constant(Ast.Bool(q))) -> Ast.Constant(Ast.Bool(p || q))
| (Ast.And,Ast.Constant(Ast.Bool(p)),Ast.Constant(Ast.Bool(q))) -> Ast.Constant(Ast.Bool(p && q))
(* land et lor dépendent de l'architecture (16 bits *)
| (op,e1,e2) -> Ast.BinOp(op,e1,e2)
and eval_unop = function
| (Ast.Not,Ast.Constant(Ast.Bool(b))) -> Ast.Constant(Ast.Bool(not b))
| (Ast.UMinus,Ast.Constant(Ast.Int(n))) -> Ast.Constant(Ast.Int(- n))
| (op,e) -> Ast.UnOp(op,e)




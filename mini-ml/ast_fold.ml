(* constant folding *)

let compile_assertions = ref false


(* rend le nombre d'occurence de la variable v dans l'expression e *)
let rec occ (v : string) e = match e with
  | Ast.Constant c -> 0
  | Ast.Let(name,e1,e2) -> if v = name then occ v e1 else occ v e1 + occ v e2
  | Ast.App(e,args) -> occ v e + occ_list v args
  | Ast.If(e1,e2,e3) -> occ v e1 + occ v e2 + occ v e3
  | Ast.BinOp(op,e1,e2) -> occ v e1 + occ v e2
  | Ast.UnOp(op,e1) -> occ v e1 
  | Ast.Ref_access(e1) -> occ v e1 
  | Ast.Ref_assign(e1,e2) -> occ v e1 + occ v e2
  | Ast.Ref(e) -> occ v e
  | Ast.Array_access(e1,e2) -> occ v e1 + occ v e2
  | Ast.Array_assign(e1,e2,e3)  -> occ v e1 + occ v e2 + occ v e3
  | Ast.Pair(e1,e2) -> occ v e1 + occ v e2
  | Ast.Cons(e1,e2) -> occ v e1 + occ v e2
  | Ast.Array_create(xs) -> occ_list v xs
  | Ast.Seq(e1,e2) -> occ v e1 + occ v e2
  | Ast.While(e1,e2) -> occ v e1 + occ v e2
  | Ast.For(name,e0,e1,e2) -> if v = name then 0 else occ v e0 + occ v e1 + occ v e2
  | Ast.Match(e,ms) -> occ v e + occ_list v (List.map (function Ast.Case(_,e) -> e | Ast.Otherwise e -> e) ms)
  | Ast.Assert(e,pos) -> if !compile_assertions then occ v e else 0
  | Ast.Ident name -> if name = v then 1 else 0
  | e -> 0
and occ_list v l = 
  List.fold_left (fun acc e -> acc + occ v e) 0 l



(* remplace les occurences de la variable locale v par x dans e. *)
let replace v x e = 
  let rec replace = function
    | Ast.Ident name -> if v = name then x else Ast.Ident name
    | Ast.Constant c -> Ast.Constant c
    | Ast.Let(name,e1,e2) -> let s = if v = name then e2 else replace e2 in (* gestion de la capture *)
                             Ast.Let(name,replace e1,s)
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


(* applique la propagation des constantes dans le modules *)
let rec rewrite mdl = 
  let Ast.{mod_name;decls} = mdl in
  let decls = List.map fold_decl decls in
  Ast.{mod_name;decls}

and fold_decl = function
  | Ast.DefVar(v,e) -> Ast.DefVar(v,fold_exp e)
  | Ast.DefFun(l) -> Ast.DefFun (fold_fundecs l)
  | Ast.DefFunRec(l) -> Ast.DefFunRec (fold_fundecs l)
  | d -> d 
and fold_fundecs l = 
  List.map (fun (name,args,e) -> (name,args,fold_exp e)) l 
and fold_exp = function
  | Ast.Ident name -> Ast.Ident name
  | Ast.Constant c -> Ast.Constant c
  | Ast.Let(v,e1,e2) -> 
     let e1 = fold_exp e1 in
     let e2 = fold_exp e2 in
     (match e1 with 
      | Ast.Assert _ | Ast.For _ | Ast.While _ 
        | Ast.Array_assign _ | Ast.Ref_assign _ -> Ast.Seq (e1,fold_exp @@ replace v (Ast.Constant(Ast.Unit)) e2)
      | _ ->
         (match e1,occ v e2 with
          | Ast.Ident (x),_ -> (replace v (Ast.Ident (x)) e2)  (* let y = x in e(x,y) ~> e(x) *)
          | Ast.Constant _,0 -> e2
          | Ast.Constant _,_ -> fold_exp (replace v e1 e2)
          | _ -> Ast.Let(v,e1,e2)))
  | Ast.App(e,args) -> Ast.App(fold_exp e,List.map fold_exp args) 
  | Ast.If(e1,e2,e3) -> (match fold_exp e1 with 
                         | Ast.Constant(Ast.Bool(b)) -> fold_exp (if b then e2 else e3)
                         | e -> Ast.If(e,fold_exp e2,fold_exp e3))
  | Ast.BinOp(op,e1,e2) -> eval_binop (op,fold_exp e1,fold_exp e2)
  | Ast.UnOp(op,e1) -> let x = fold_exp e1 in 
                       (match x with 
                        | Ast.Constant _ -> eval_unop (op,x)
                        | _ -> Ast.UnOp(op,x))
  | Ast.Ref_access(e1) -> Ast.Ref_access(fold_exp e1)
  | Ast.Ref_assign(e1,e2) -> Ast.Ref_assign(fold_exp e1,fold_exp e2)
  | Ast.Ref(e) -> Ast.Ref(fold_exp e)
  | Ast.Array_access(e1,e2) -> Ast.Array_access(fold_exp e1,fold_exp e2) 
  | Ast.Array_assign(e1,e2,e3)  -> Ast.Array_assign(fold_exp e1,fold_exp e2,fold_exp e3)
  | Ast.Pair(e1,e2) -> Ast.Pair(fold_exp e1,fold_exp e2) 
  | Ast.Cons(e1,e2) -> Ast.Cons(fold_exp e1,fold_exp e2)
  | Ast.Array_create(xs) -> Ast.Array_create(List.map fold_exp xs)
  | Ast.Seq(e1,e2) -> Ast.Seq(fold_exp e1,fold_exp e2)
  | Ast.While(e1,e2) -> (match fold_exp e1 with 
                         | Ast.Constant(Ast.Bool(false)) -> Ast.Constant(Ast.Unit)
                         | e -> Ast.While(e,fold_exp e2))
  | Ast.For(name,e0,e1,e2) -> let e0' = fold_exp e0 in
                              let e1' = fold_exp e1 in
                              (match e0',e1' with 
                               | Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m)) 
                                    when n = m -> Ast.Constant(Ast.Unit)
                               | _ -> Ast.For(name,e0',e1',fold_exp e2)) 
  | Ast.Match(e,ms) ->  
                          (* let rec aux acc = function
                          | Ast.Case(c,e)::t -> aux (Ast.Case(c,fold_exp e)::acc) t
                          | Ast.Otherwise(e)::_ -> List.rev (Ast.Otherwise(fold_exp e)::acc)
                          | [] -> Printf.printf "Warning 8: this pattern-matching is not exhaustive.\n"; 
                                  List.rev ((Ast.Otherwise(Ast.App(Ast.Ident("Pervasives.failwith"),
                                                                   [Ast.Constant(Ast.String("Match_failure, exit."))]))) :: acc) in
                        let ms = aux [] ms in *)
                        (* (match fold_exp e with 
                         | Ast.Constant c -> (match List.find (function Ast.Case(c',_) -> c' = c | Ast.Otherwise _ -> true) ms with
                                              | Ast.Case(_,e) -> e
                                              | Ast.Otherwise e -> e)
                         | e' -> Ast.Match(e',ms)) *)
    Ast.Match(e,ms)
  | Ast.Assert(e,pos) -> 
     if !compile_assertions 
     then Ast.Assert(fold_exp e,pos) 
     else Ast.Constant(Ast.Unit)
  | e -> e
and eval_binop = function
  | (Ast.Add,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) -> Ast.Constant(Ast.Int((n+m) land 0xFFFF))
  | (Ast.Minus,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) -> Ast.Constant(Ast.Int((n-m) land 0xFFFF))
  | (Ast.Mult,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) -> Ast.Constant(Ast.Int((n*m) land 0xFFFF))
  | (Ast.Div,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) -> Ast.Constant(Ast.Int((n/m) land 0xFFFF))
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




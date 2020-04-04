
let gensym = 
  let c = ref 0 in 
  (fun ~prefix -> incr c; Printf.sprintf "__%s%d" prefix !c)


type genv = { mod_name : Ast.name;
              globals : (Ast.name * int) list;
              constrs : (Ast.name * int) list;
              global_funs : string list;
              primitives : (Ast.name * Ast.name) list;
              typed_decls : Types.env;
              init : Ast.name list }
and arity = int

let empty_genv prims mod_name = {mod_name;
                                 globals=[];
                                 constrs=[];
                                 global_funs=[];
                                 primitives=List.map (fun (x,c,ty) -> (x,c)) prims; (* ignore type *)
                                 typed_decls=Typing.initial_env (Runtime.primitives);
                                 init=[]}
let genv_extend genv x = 
  let i,globals' = match genv.globals with 
    | [] -> (0,[(x,0)])
    | (_,i)::_ -> let j = i + 1 in (j,((x,j)::genv.globals)) in
  (i,{genv with globals=globals'})

let env_extends_constructors genv cstrs =
    let r = ref genv.constrs in
    List.iteri (fun i x -> r := ((genv.mod_name ^ "." ^ x),i) :: !r) cstrs;
    {genv with constrs=(!r)}

type lenv = { arguments : (Ast.name * int) list;
              locals : (Ast.name * int) list ;
              nexts : lenv option }


let empty_lenv = {arguments=[];locals=[];nexts=None}
let frame ?lenv args = 
  let a = List.mapi (fun i name -> (name,i)) args in
  {arguments=a;locals=[];nexts=lenv}

let lenv_extend x lenv = 
  let i2,locals' = match lenv.locals with 
    | [] -> (0,[(x,0)])
    | (_,i)::_ -> let i2 = i + 1 in (i2,((x,i2)::lenv.locals)) in
  (i2,{lenv with locals=locals'})


let rec (*rewrite_prog p =
  let (genv,kds) = rewrite_tmodule p in
  {modules=kds;globals= }
and *) rewrite_tmodule genv Ast.{mod_name;decls} = 
  let (genv',kds) = rewrite_decls mod_name genv decls in
  (genv',Kast.{mod_name;decls=kds;init=(List.map fst genv'.globals)})
and rewrite_decls mod_name genv ds = 
  let genv,kds = List.fold_left 
                   (fun (genv,acc) d -> 
                     match rewrite_decl mod_name genv d with
                     | (genv,[]) -> (genv,acc)
                     | (genv,kds) -> (genv,kds @ acc)) (genv,[]) ds in
  (genv,List.rev kds)
and rewrite_decl mod_name genv = function
  (*| Ast.Type (name,ty) -> (match ty with 
                           | Ast.Sum(names) -> 
                             let genv' = env_extends_constructors genv names in (genv',[])
                           | _ -> (genv,[])) *)
  | Ast.Exp (e) -> rewrite_decl mod_name genv @@
                     let name = gensym ~prefix:"voidExpr" in Ast.DefVar ((name,None),e)
  | Ast.DefVar ((name,_),e) ->
     let name_init = "__init__" ^ name in
     let i,genv' = genv_extend genv name in
     let genv0 = {genv' with init = (mod_name ^ "." ^ name_init) :: genv'.init} in
     let genv1,d1 = rewrite_decl mod_name genv0 @@
                      Ast.DefFun ([name_init,[],Ast.SetGlobal(e,i)]) in
     let genv2,d2 = rewrite_decl mod_name genv1 @@
                      Ast.DefFun ([name,[],Ast.ReadGlobal(i)]) in
     (genv2,(d1 @ d2))
  | Ast.DefFun l -> rewrite_defun mod_name genv l
  | Ast.DefFunRec l -> rewrite_defun mod_name genv ~recflag:true l
and rewrite_defun mod_name genv ?(recflag=false) dfs =
     let gnames = List.map (fun (name,args,e) -> mod_name ^ "." ^ name)  dfs in
     let genv' = {genv with global_funs = gnames @ genv.global_funs} in
     let by = List.concat (List.map (fun (name,args,e) ->
     let lenv = frame args in
     let ke = rewrite_exp lenv (if recflag then genv' else genv) e in (* genv si non recursif *) 
     let arity = List.length args in
     [Kast.DefFun (name,arity,ke)]) dfs) in (genv',by)
and rewrite_exp lenv genv = function
  | Ast.Annotation (e,_) -> rewrite_exp lenv genv e
  | Ast.Constant(Ast.String(s)) ->
    let rev_xs = ref [] in
    String.iter (fun c -> rev_xs := (c :: !rev_xs)) s;
    rewrite_exp lenv genv @@
    Ast.Array_create(List.rev_map (fun c -> Ast.Constant(Ast.Char(c))) !rev_xs)
  | Ast.Constant c -> Kast.Constant(rewrite_constant lenv genv c)
  | Ast.Ident (name) ->
     (match List.assoc_opt name lenv.locals with
      | None ->
         (match List.assoc_opt name lenv.arguments with
          | None ->
             (match List.assoc_opt name genv.globals with
              | None -> let full_name = if (match String.index_opt name '.' with None -> false | _ -> true)
                                        then name
                                        else (genv.mod_name ^ "." ^ name) in                
                 (if List.mem full_name genv.global_funs
                  then Kast.GFun(full_name)
                  else let f = (try List.assoc name genv.primitives with Not_found -> failwith ("cannot find " ^ name)) in
                       Kast.GFun(f))
              | Some i -> Kast.App (Kast.GFun(genv.mod_name ^ "." ^ name),[]))
          | Some i -> Kast.Variable(Kast.Argument (i)))
      | Some i -> Kast.Variable(Kast.Local i))
  | Ast.Let((name,_),e1,e2) ->
    let i,lenv' = lenv_extend name lenv in 
    Kast.Let(i,rewrite_exp lenv' genv e1, rewrite_exp lenv' genv e2)
  | Ast.App(e,args) -> 
     Kast.App(rewrite_exp lenv genv e, List.map (rewrite_exp lenv genv) args)
  | Ast.If(e1,e2,e3) -> Kast.If(rewrite_exp lenv genv e1,
                                rewrite_exp lenv genv e2,
                                rewrite_exp lenv genv e3)
  | Ast.BinOp(op,e1,e2) ->
     Kast.BinOp(op,rewrite_exp lenv genv e1,rewrite_exp lenv genv e2)
  | Ast.UnOp(op,e1) -> Kast.UnOp(op,rewrite_exp lenv genv e1)
  | Ast.Ref_access(e1) ->
     rewrite_exp lenv genv @@ 
       Ast.App(Ast.(Ident("Pervasives.ref_contents"),[e1]))
  | Ast.Ref_assign(e1,e2) ->
     rewrite_exp lenv genv @@  
       Ast.App(Ast.(Ident("Pervasives.ref_set_contents"),[e1;e2]))
  | Ast.Ref(e) ->
     rewrite_exp lenv genv @@ Ast.App(Ast.(Ident("Pervasives.ref"),[e]))
  | Ast.Array_access(e1,e2) ->
     rewrite_exp lenv genv @@ Ast.App(Ast.(Ident("Array.get"),[e1;e2])) 
  | Ast.Array_assign(e1,e2,e3) ->
     rewrite_exp lenv genv @@ Ast.App(Ast.(Ident("Array.set"),[e1;e2;e3])) 
  | Ast.Array_alloc(e) ->
     rewrite_exp lenv genv @@ Ast.App(Ast.(Ident("Array.create_uninitialized"),[e])) 
  | Ast.Pair(e1,e2) -> rewrite_exp lenv genv @@ Ast.App(Ast.(Ident("__internal.pair"),[e1;e2])) 
  | Ast.Cons(e1,e2) -> rewrite_exp lenv genv @@ Ast.App(Ast.(Ident("__internal.cons"),[e1;e2])) 
  | Ast.Array_create(xs) ->
     rewrite_exp lenv genv @@
       let n = List.length xs in
       let a = gensym ~prefix:"tmp" in
       Ast.Let((a,None),Ast.Array_alloc(Ast.Constant(Ast.Int(n))),
               let rec aux i = function
                 | [] -> Ast.Ident(a)
                 | e::es -> Ast.Seq(Ast.Array_assign(Ast.Ident(a),
                                                     Ast.Constant(Ast.Int(i)),e),
                                    aux (i+1) es) in aux 0 xs)                        
  | Ast.Seq(e1,e2) -> Kast.Seq(rewrite_exp lenv genv e1, rewrite_exp lenv genv e2)
  | Ast.While(e1,e2) -> Kast.While(rewrite_exp lenv genv e1, rewrite_exp lenv genv e2)
  | Ast.For(name,e0,e1,e2) -> 
     rewrite_exp lenv genv @@
       let name_zz = gensym ~prefix:name in
       let len_zz = gensym ~prefix:"L" in
       let open Ast in
       Let((name_zz,None), 
           Ref(e0),
           Let((len_zz, None),e1,
               While(BinOp (Le,Ref_access(Ident(name_zz)),Ident(len_zz)),
                     Let((name,None), Ref_access(Ident(name_zz)),
                         Seq(e2,App(Ast.Ident("Pervasives.incr"),[Ident(name_zz)]))))))
  | Ast.Match (e,ms) -> 
     let ms',otherw = let rec aux acc = function
                        | [] -> (acc,None)
                        | Ast.Otherwise e :: _ -> (acc,Some e)
                        | h::t -> aux (h::acc) t in aux [] ms in
     let sms = List.map (function Ast.Case(c,e) -> (c,e) | _ -> assert false) ms' in
     let smst = List.sort (fun (c1,_) (c2,_) -> compare c1 c2) sms in
     let var = gensym ~prefix:"L" in
     rewrite_exp lenv genv @@
       Ast.Let((var, None),e,
           let rec aux = function
             | [] -> (match otherw with 
                      | None -> Ast.App(Ast.Ident("Pervasives.exit"),
                                       [Ast.Constant (Ast.Int(1))]) (* match failure *)
                      | Some e -> e)
             | [(_,h)] -> h
             | ms -> let (md,e') = List.nth ms (List.length ms / 2) in
                     let l1,l2 = List.partition (fun (c,_) -> c < md) ms in
                     let l2 = List.tl l2 in 
                     Ast.If(Ast.BinOp(Ast.Lt,
                                      Ast.Ident(var),
                                      Ast.Constant(md)),
                            aux l1,Ast.If(Ast.BinOp(Ast.Eq,
                                                    Ast.Ident(var),
                                                    Ast.Constant(md)),e',aux l2)) in
           aux smst) 
  | Ast.Assert(e) -> rewrite_exp lenv genv @@ 
                       Ast.If(e,
                              Ast.Constant(Ast.Unit),
                              Ast.App(Ast.Ident("Pervasives.exit"),
                                      [Ast.Constant (Ast.Int(1))]))
  | Ast.SetGlobal(e,i) -> Kast.SetGlobal (rewrite_exp lenv genv e,i)
  | Ast.ReadGlobal(i) -> Kast.ReadGlobal(i)
and rewrite_constant lenv genv = function
  | Ast.Unit -> Kast.Unit
  | Ast.Int n -> Kast.Int n
  | Ast.Char c -> Kast.Int (int_of_char c)
  | Ast.Constr name -> Kast.Int (match List.assoc_opt (genv.mod_name ^ "." ^ name) genv.constrs with 
                                 | None -> (List.assoc name genv.constrs)
                                 | Some c -> c)
  | Ast.Bool b -> Kast.Bool b 
  | Ast.Array_empty -> Kast.Array_empty
  | Ast.List_empty -> Kast.List_empty
  | Ast.String _ -> assert false (* déjà traité *)


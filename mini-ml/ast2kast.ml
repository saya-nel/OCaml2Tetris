
let gensym = 
  let c = ref 0 in 
  (fun ~prefix -> incr c; Printf.sprintf "__%s%d" prefix !c)


type genv = { mod_name : Ast.name;
              globals : (Ast.name * int) list;
              constrs : (Ast.name * int) list;
              current_decls : string list;
              primitives : (Ast.name * Ast.name) list;
              init : Ast.name list }
and arity = int

let primitives () =
  ["incr",("Pervasives.incr");
   "decr",("Pervasives.decr");
   "ref",("Pervasives.ref");
   "ref_contents",("Pervasives.ref_contents");
   "ref_set_contents",("Pervasives.ref_set_contents");
   "print_char",("Output.printChar");
   "print_string",("Output.printString");
   "print_int", ("Output.printInt");
   "peek",("Memory.peek");
   "poke",("Memory.poke");
   "alloc",("Memory.alloc");
   "error",("Sys.error");]

let empty_genv primitives mod_name = {mod_name;globals=[];constrs=[];current_decls=[];primitives;init=[]}
let genv_extend genv x = 
  let i,globals' = match genv.globals with 
    | [] -> (0,[(x,0)])
    | (_,i)::_ -> let j = i + 1 in (j,((x,j)::genv.globals)) in
    (i,{genv with globals=globals'})

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
    | (_,i)::_ -> let i2 = i + 1 in (i2,((x,i2)::lenv.locals)) in (i2,{lenv with locals=locals'})


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
  (* | Ast.TypeEnum (_,names) -> let genv' = List.fold_left Env.add_constr genv names in (env',None) *)
  | Ast.Exp (e) -> rewrite_decl mod_name genv @@
                   let name = gensym ~prefix:"voidExpr" in Ast.DefVar (name,e)
  | Ast.DefVar (name,e) -> let name_init = "__init__" ^ name in
                           let i,genv' = genv_extend genv name in
                           let genv0 = {genv' with init = (mod_name ^ "." ^ name_init) :: genv'.init} in
                           let genv1,d1 = rewrite_decl mod_name genv0 @@
                                          Ast.DefFun (name_init,[],Ast.SetGlobal(e,i)) in
                           let genv2,d2 = rewrite_decl mod_name genv1 @@
                                          Ast.DefFun (name,[],Ast.ReadGlobal(i)) in
                           (genv2,(d1 @ d2))
  | Ast.DefFun (name,args,e) -> let genv' = {genv with current_decls = name :: genv.current_decls} in
                                let lenv = frame args in
                                let ke = rewrite_exp lenv genv' e in (* genv si non recursif *) 
                                let arity = List.length args in
                                (genv',[Kast.DefFun (name,arity,ke)])  
and rewrite_exp lenv genv = function
(*let len = String.length s in
                 rewrite_exp lenv genv @@ 
                 let x = gensym ~prefix:"tmp" in
                 let rec aux = function 
                 | 0 -> Ast.Bloc_alloc(Ast.Constant(Ast.Int(len)))
                 | n -> Ast.App(Ast.Ident("String.appendChar"),
                        let u = aux (n-1) in 
                        [u;Ast.Constant(Ast.Int(n));Ast.Constant(Ast.Int(Char.code s.[n]))]) in aux (len-1)*)
                       (*  let rec aux n = if n = len then Ast.Ident(x) else
                         Ast.Seq(Ast.App(Ast.Ident("String.setCharAt"),
                           [Ast.Constant(Ast.Int(n));Ast.Constant(Ast.Int(Char.code s.[n]));Ast.Ident(x);]), aux (n+1)) in aux 0) *) 
  | Ast.Constant c -> Kast.Constant(rewrite_constant lenv genv c)
  | (Ast.Ident (name) as f) ->
     (match List.assoc_opt name lenv.locals with
      | None ->
         (match List.assoc_opt name lenv.arguments with
          | None ->
             (match List.assoc_opt name genv.globals with
              | None ->
                 (if List.mem name genv.current_decls 
                  then Kast.GFun(genv.mod_name ^ "." ^ name) 
                  else (match List.assoc_opt name genv.primitives with
                        | None -> Kast.GFun(name)
                        | Some f -> Kast.GFun(f)))
              | Some i -> App (Kast.GFun(genv.mod_name ^ "." ^ name),[]))
          | Some i -> Kast.Variable(Kast.Argument (i)))
      | Some i -> Kast.Variable(Kast.Local i))
  | Ast.Let(name,e1,e2) ->
     let i,lenv' = lenv_extend name lenv in 
     Kast.Let(i,rewrite_exp lenv' genv e1, rewrite_exp lenv' genv e2)
  (*| Ast.LetFun(name,args,e1,e2) -> let lenv' = frame ~lenv args in
                                   let free_variables = f e1 in
                                   Ast.LetFun(name,free_variables @ args,e1,add_arguments e2)
   *)                         
  | Ast.App(e,args) -> Kast.App(rewrite_exp lenv genv e, List.map (rewrite_exp lenv genv) args)
  | Ast.If(e1,e2,e3) -> Kast.If(rewrite_exp lenv genv e1,
                                rewrite_exp lenv genv e2,
                                rewrite_exp lenv genv e3)
  | Ast.BinOp(op,e1,e2) -> Kast.BinOp(op,rewrite_exp lenv genv e1,rewrite_exp lenv genv e2)
  | Ast.UnOp(op,e1) -> Kast.UnOp(op,rewrite_exp lenv genv e1)
  | Ast.Ref_access(e1) -> rewrite_exp lenv genv @@ 
                          Ast.App(Ast.(Ident("Pervasives.ref_contents"),[e1]))
                           (* Ast.Array_access(e1,Ast.Constant(Ast.Int(0))) *)
  | Ast.Ref_assign(e1,e2) -> rewrite_exp lenv genv @@  
                             Ast.App(Ast.(Ident("Pervasives.ref_set_contents"),[e1;e2]))(*    Ast.Array_assign(e1,Ast.Constant(Ast.Int(0)), e2) *)
  | Ast.Ref(e) -> rewrite_exp lenv genv @@ Ast.App(Ast.(Ident("Pervasives.ref"),[e])) (* Ast.Array_create([e]) *)
  | Ast.Array_access(e1,e2) -> rewrite_exp lenv genv @@ Ast.App(Ast.(Ident("Array.get"),[e1;e2])) 
  | Ast.Array_assign(e1,e2,e3) -> rewrite_exp lenv genv @@ Ast.App(Ast.(Ident("Array.set"),[e1;e2;e3])) 
  | Ast.Array_alloc(e) -> rewrite_exp lenv genv @@ Ast.App(Ast.(Ident("Array.create_uninitialized"),[e])) 
  | Ast.Array_create(xs) -> rewrite_exp lenv genv @@
                            let n = List.length xs in
                            let a = gensym ~prefix:"tmp" in
                            Ast.Let(a,Ast.Array_alloc(Ast.Constant(Ast.Int(n))),
                            let rec aux i = function
                              | [] -> Ast.Ident(a)
                              | e::es -> Ast.Seq(Ast.Array_assign(Ast.Ident(a),Ast.Constant(Ast.Int(i)),e),
                                                 aux (i+1) es) in aux 0 xs)                        
  | Ast.Seq(e1,e2) -> Kast.Seq(rewrite_exp lenv genv e1,
                               rewrite_exp lenv genv e2)
  | Ast.While(e1,e2) -> Kast.While(rewrite_exp lenv genv e1,
                                   rewrite_exp lenv genv e2)
  | Ast.For(name,e0,e1,e2) -> 
     rewrite_exp lenv genv @@
       let name_zz = gensym ~prefix:name in
       let len_zz = gensym ~prefix:"len" in
       let open Ast in
       Let(name_zz, 
           Ref(e0),
           Let(len_zz,e1,
               While(BinOp (Le,Ref_access(Ident(name_zz)),
                            Ident(len_zz)),
                     Let(name,
                         Ref_access(Ident(name_zz)),
                         Seq(e2,Ref_assign(Ident(name_zz),
                                           BinOp(Add,Ref_access(Ident(name_zz)),
                                                 Constant(Int(1)))))))))
| Ast.Assert(e) -> rewrite_exp lenv genv @@ 
                   Ast.If(e,Ast.Constant(Ast.Unit),Ast.App(Ast.Ident("error"),[Ast.Constant (Ast.Int(1))]))
| Ast.SetGlobal(e,i) -> Kast.SetGlobal (rewrite_exp lenv genv e,i)
| Ast.ReadGlobal(i) -> Kast.ReadGlobal(i)
and rewrite_constant lenv genv = function
  | Ast.Unit -> Kast.Unit
  | Ast.Int n -> Kast.Int n
  | Ast.Constr name -> Kast.Int (List.assoc name genv.constrs)
  | Ast.Bool b -> Kast.Bool b 
  | Ast.String(s) -> Kast.String(s)
(*  | Ast.String s -> Kast.String s*)
  | Ast.Array_empty -> Kast.Array_empty



let gensym = 
  let c = ref 0 in 
  (fun ~prefix ->
    incr c;
    Printf.sprintf "__%s%d" prefix !c)

type genv = {
    mod_name : Iast.name;
    globals : (Iast.name * int) list;
    constrs : (Iast.name * int) list;
    global_funs : string list;
    primitives : (Iast.name * Iast.name) list;
    typed_decls : Types.env;
    init : Iast.name list }

let empty_genv prims mod_name =
  {
    mod_name;
    globals=[];
    constrs=[];
    global_funs=[];
    primitives=List.map (fun (x,c,ty) -> (x,c)) prims; (* ignore type *)
    typed_decls=Typing.initial_env (Runtime.primitives);
    init=[]
  }
  
let genv_extend genv x = 
  let i,globals' =
    match genv.globals with 
    | [] ->
       (0,[(x,0)])
    | (_,i)::_ ->
       let j = i + 1 in
       (j,((x,j)::genv.globals)) in
  (i,{genv with globals=globals'})

let env_extends_constructors genv cstrs =
  let r = ref genv.constrs in
  List.iteri
    (fun i x ->
      r := ((genv.mod_name ^ "." ^ x),i) :: !r)
    cstrs;
  {genv with constrs=(!r)}

type lenv = {
    arguments : (Iast.name * int) list ;
    locals : (Iast.name * int) list ;
    free : (Iast.name * int) list ;
    nexts : lenv option
  }
          

let empty_lenv =
  {arguments=[];locals=[];free=[];nexts=None}
  
let frame ?lenv args = 
  let a = List.mapi (fun i name -> (name,i)) args in
  {arguments=a;locals=[];free=[];nexts=lenv}

let lenv_extend x lenv = 
  let i2,locals' =
    match lenv.locals with 
    | [] ->
       (0,[(x,0)])
    | (_,i)::_ ->
       let i2 = i + 1 in
       (i2,((x,i2)::lenv.locals)) in
  (i2,{lenv with locals=locals'})

let lenv_extend_tail x lenv = (* réutilse les variables de même nom déjà définies *)
  let i2,locals' =
    match lenv.locals with 
    | [] -> 
       (0,[(x,0)])
    | ((_,i)::_) as l -> 
      let i2 = (match List.assoc_opt x l with 
                | None -> i + 1
                | Some i -> i) in
      (i2,((x,i2)::lenv.locals)) in
    (i2,{lenv with locals=locals'})


(* convertit l'IAST du module mdl en KAST *)
let rec rewrite genv mdl =
  let Iast.{mod_name;decls} = mdl in
  let (genv',kds) = rw_decls mod_name genv decls in
  (genv',Kast.{mod_name;decls=kds;init=(List.map fst genv'.globals)})
and rw_decls mod_name genv ds = 
  let genv,kds =
    List.fold_left 
      (fun (genv,acc) d -> 
        match rw_decl mod_name genv d with
        | (genv,[]) ->
           (genv,acc)
        | (genv,kds) ->
           (genv,kds @ acc)) (genv,[]) ds in
  (genv,List.rev kds)
and rw_decl mod_name genv = function
  | Iast.Exp(e) ->
     rw_decl mod_name genv @@
       let name = gensym ~prefix:"voidExpr" in
       Iast.DefVar (name,e)
  | Iast.DefVar (name,e) ->
     let name_init = "__init__" ^ name in
     let i,genv' = genv_extend genv name in
     let genv0 = {genv' with init = (mod_name ^ "." ^ name_init) :: genv'.init} in
     let genv1,d1 = rw_decl mod_name genv0 @@
                      Iast.DefFun ([name_init,[],Iast.Ext(Iast.SetGlobal(e,i))]) in
     let genv2,d2 = rw_decl mod_name genv1 @@
                      Iast.DefFun ([name,[],Iast.Ext(Iast.ReadGlobal(i))]) in
     (genv2,(d1 @ d2))
  | Iast.DefFun l -> rw_defun mod_name genv l
  | Iast.DefFunRec l -> rw_defun mod_name genv ~recflag:true l
and rw_defun mod_name genv ?(recflag=false) dfs =
  let gnames = List.map (fun (name,args,e) -> mod_name ^ "." ^ name)  dfs in
  let genv' = {genv with global_funs = gnames @ genv.global_funs} in
  let by = List.concat
             (List.map
                (fun (name,args,e) ->
                  let lenv = frame args in
                  let ke = rw_exp lenv
                             (if recflag
                              then genv'
                              else genv) e
                  in (* genv si non recursif *) 
                  let arity = List.length args in
                  [Kast.DefFun (name,arity,ke)]) dfs) in
  (genv',by)
and rw_exp lenv genv = function
  | Iast.Constant c -> rw_constant lenv genv c
  | Iast.Ident (name) ->
     (match List.assoc_opt name lenv.locals with
      | None ->
         (match List.assoc_opt name lenv.arguments with
          | None ->
             (match List.assoc_opt name lenv.free with
             | None ->
             (match List.assoc_opt name genv.globals with
              | None -> let full_name =
                          if (match String.index_opt name '.' with
                              | None -> false
                              | _ -> true)
                          then name
                          else (genv.mod_name ^ "." ^ name)
                        in                
                        (if List.mem full_name genv.global_funs
                         then Kast.GFun(full_name)
                         else let f =
                                try
                                  List.assoc name genv.primitives
                                with Not_found -> failwith ("cannot find " ^ name)
                              in
                              Kast.GFun(f))
              | Some i -> Kast.Variable(Kast.Global (genv.mod_name ^ "." ^ name))) (* Kast.App (Kast.GFun(genv.mod_name ^ "." ^ name),[])) *)
            | Some i -> Kast.Variable(Kast.Free (i)))
          | Some i -> Kast.Variable(Kast.Argument (i)))
      | Some i -> Kast.Variable(Kast.Local i))
  | Iast.Let(name,e1,e2) ->
     let i,lenv' = lenv_extend_tail name lenv in (* optimisation : recyclage des variables masquées, 
                                                   attention, dans, let x = e1(x) in e2, e1 doit bien utiliser l'ancienne valeur de x *)
     Kast.Let(i,rw_exp lenv genv e1, rw_exp lenv' genv e2)
  | Iast.Fun(name,e) ->
    (*let ww = List.map fst lenv.locals @ List.map fst lenv.arguments in
    let free_vars = Bindings.collect ww [name] e in
    let ke = rw_exp lenv genv @@ e in
    Kast.Fun(ke,List.length lenv.locals,List.length lenv.arguments)*)

     let lenv' = {lenv with 
                 free=lenv.locals @ 
                  (let n = List.length lenv.locals in 
                   List.map (fun (c,i) -> (c,i+n)) lenv.arguments); 
            arguments=[(name,0)];
            locals=[]} in
    let ke = rw_exp lenv' genv @@ e in
    Kast.Fun(ke,List.length lenv.locals,List.length lenv.arguments) 
    
    (* let vars = Bindings.collect ke in
    let len = List.length vars in
    let kmake = rw_exp lenv genv Iast.(Ident("Array.create_uninitialized")) in
    let kset = rw_exp lenv genv Iast.(Ident("Array.set")) in
    let i,lenv' = lenv_extend name lenv in 
    Kast.Let(i,Kast.App(kmake,[Kast.Constant(Kast.Int len)]), 
      let j,e = List.fold_left (fun (j,acc) x ->
        (j+1, 
         Kast.Seq(Kast.App(kset,[Kast.Variable(Kast.Local i);Kast.Constant(Kast.Int j);Kast.Variable(x)]),acc)))
    (0,Kast.Fun(ke,List.length lenv.locals,List.length lenv.arguments)) vars in e) *)
  | Iast.App(e,args) -> 
     Kast.App(rw_exp lenv genv e, List.map (rw_exp lenv genv) args)
  | Iast.If(e1,e2,e3) ->
     Kast.If(rw_exp lenv genv e1,
             rw_exp lenv genv e2,
             rw_exp lenv genv e3)
  | Iast.BinOp(op,e1,e2) ->
     Kast.BinOp(op,rw_exp lenv genv e1,rw_exp lenv genv e2)
  | Iast.UnOp(op,e1) -> Kast.UnOp(op,rw_exp lenv genv e1)
  | Iast.Ref_access(e1) ->
     rw_exp lenv genv @@ 
       Iast.App(Iast.(Ident("Pervasives.ref_contents"),[e1]))
  | Iast.Ref_assign(e1,e2) ->
     rw_exp lenv genv @@  
       Iast.App(Iast.(Ident("Pervasives.ref_set_contents"),[e1;e2]))
  | Iast.Ref(e) ->
     rw_exp lenv genv @@
       Iast.App(Iast.(Ident("Pervasives.ref"),[e]))
  | Iast.Array_access(e1,e2) ->
     rw_exp lenv genv @@
       Iast.App(Iast.(Ident("Array.get"),[e1;e2])) 
  | Iast.Array_assign(e1,e2,e3) ->
     rw_exp lenv genv @@
       Iast.App(Iast.(Ident("Array.set"),[e1;e2;e3])) 
  | Iast.Ext(ext) ->
    (match ext with 
     | Iast.Array_alloc(e) ->
        rw_exp lenv genv @@
          Iast.App(Iast.(Ident("Array.create_uninitialized"),[e])) 
     | Iast.SetGlobal(e,i) ->
         Kast.Ext(Kast.SetGlobal (rw_exp lenv genv e,i))
     | Iast.ReadGlobal(i) ->
         Kast.Ext(Kast.ReadGlobal(i))
     | Iast.Label(s,e) -> 
         Kast.Ext(Kast.Label(s,rw_exp lenv genv e))
     | Iast.Goto(s,xs) -> 
         Kast.Ext(Kast.Goto(s,List.map (rw_exp lenv genv) xs)))
  | Iast.Pair(e1,e2) ->
     rw_exp lenv genv @@
       Iast.App(Iast.(Ident("Internal.pair"),[e1;e2])) 
  | Iast.Cons(e1,e2) ->
     rw_exp lenv genv @@
       Iast.App(Iast.(Ident("Internal.cons"),[e1;e2])) 
  | Iast.Array_create(xs) ->
     rw_exp lenv genv @@
       let n = List.length xs in
       let a = gensym ~prefix:"tmp" in
       Iast.Let(a,Iast.Ext(Iast.Array_alloc(Iast.Constant(Iast.Int(n)))),
               let rec aux i = function
                 | [] ->
                    Iast.Ident(a)
                 | e::es ->
                    Iast.Seq(Iast.Array_assign (Iast.Ident(a),
                                              Iast.Constant(Iast.Int(i)),e),
                            aux (i+1) es) in aux 0 xs)                        
  | Iast.Seq(e1,e2) ->
     Kast.Seq(rw_exp lenv genv e1, rw_exp lenv genv e2)
  | Iast.While(e1,e2) ->
     Kast.While(rw_exp lenv genv e1, rw_exp lenv genv e2)
  | Iast.For(name,e0,e1,e2) -> 
     rw_exp lenv genv @@
       let name_zz = gensym ~prefix:name in
       let len_zz = gensym ~prefix:"L" in
       let open Iast in
       Let(name_zz, 
           Ref(e0),
           Let(len_zz,e1,
               While(BinOp (Ast.Le,Ref_access(Ident(name_zz)),Ident(len_zz)),
                     Let(name, Ref_access(Ident(name_zz)),
                         Seq(e2,App(Iast.Ident("Pervasives.incr"),[Ident(name_zz)]))))))
  | Iast.Match (e,ms) -> 
     let ms',otherw = let rec aux acc = function
                        | [] ->
                           (acc,None)
                        | Iast.Otherwise e :: _ ->
                           (acc,Some e)
                        | h::t ->
                           aux (h::acc) t in aux [] ms in
     let sms = List.map (function Iast.Case(c,e) -> (c,e)
                                | _ -> assert false) ms'
     in
     let smst = List.sort (fun (c1,_) (c2,_) -> compare c1 c2) sms in
     let var = gensym ~prefix:"L" in
     rw_exp lenv genv @@
       Iast.Let(var,e,
               let rec aux = function
                 | [] -> (match otherw with 
                          | None ->
                             Iast.App(Iast.Ident("Pervasives.exit"),
                                     [Iast.Constant (Iast.Int(1))]) (* match failure *)
                          | Some e ->
                             e)
                 | [(_,h)] -> h
                 | ms -> let (md,e') = List.nth ms (List.length ms / 2) in
                         let l1,l2 = List.partition (fun (c,_) -> c < md) ms in
                         let l2 = List.tl l2 in 
                         Iast.If(Iast.BinOp(Ast.Lt,
                                          Iast.Ident(var),
                                          Iast.Constant(md)),
                                aux l1,Iast.If(Iast.BinOp(Ast.Eq,
                                                        Iast.Ident(var),
                                                        Iast.Constant(md)),e',aux l2)) in
               aux smst) 
  | Iast.Assert(e,pos) ->
     rw_exp lenv genv @@ 
       Iast.If(e,
              Iast.Constant(Iast.Unit),
              Iast.Seq(
                Iast.App(Iast.Ident("Pervasives.print_string"),
                  [Iast.Constant
                    (Iast.String 
                      (Printf.sprintf "assertion fail [%s]"    "" (* (Print_ast.sprint_exp 0 e)))]) *)))]),
                Iast.Seq(Iast.App(Iast.Ident("Pervasives.print_newline"),[ Iast.Constant(Iast.Unit)]),
                Iast.Seq(Iast.App(Iast.Ident("Pervasives.print_string"),
                  [Iast.Constant
                    (Iast.String 
                      (Printf.sprintf "at %s : %s. exit." (genv.mod_name) (Parseutils.string_of_position pos)))]),
                Iast.App(Iast.Ident("Pervasives.exit"),
                       [Iast.Constant (Iast.Int(0))])))))
and rw_constant lenv genv c = match c with 
| Iast.String(s) ->
     let rev_xs = ref [] in
     String.iter (fun c -> rev_xs := (c :: !rev_xs)) s;
     rw_exp lenv genv @@
       Iast.Array_create(List.rev_map (fun c -> Iast.Constant(Iast.Char(c))) !rev_xs) 
| _ -> Kast.Constant(match c with 
  | Iast.Unit ->
     Kast.Unit
  | Iast.Int n ->
     Kast.Int n
  | Iast.Char c ->
     Kast.Int (int_of_char c)
  | Iast.Constr name ->
     Kast.Int (match List.assoc_opt (genv.mod_name ^ "." ^ name) genv.constrs with 
               | None -> (List.assoc name genv.constrs)
               | Some c -> c)
  | Iast.Bool b ->
     Kast.Bool b 
  | Iast.Array_empty ->
     Kast.Array_empty
  | Iast.List_empty ->
     Kast.List_empty
  | Iast.String _ ->
     assert false) (* déjà traité *)


let gensym = 
  let c = ref 0 in 
  (fun ~prefix ->
    incr c;
    Printf.sprintf "__%s%d" prefix !c)

let associate n l =
  let rec aux acc n = function
  | [] -> acc    (* inverse l'ordre, pas génant *)
  | h::t -> aux ((h,n)::acc) (n+1) t in aux [] n l

type genv = {
    mod_name : Ast.name;
    globals : (Ast.name * int) list;
    global_funs : string list;
    primitives : (Ast.name * Ast.name) list;
    typed_decls : Types.env;
    init : Ast.name list }
and arity = int
let empty_genv prims mod_name =
  {
    mod_name;
    globals=[];
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


type lenv = {
    arguments : (Ast.name * int) list ;
    locals : (Ast.name * int) list ;
    free : (Ast.name * int) list ;
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



(* convertit l'IAST du module m en KAST *)
let rec rewrite genv m =
  match m with Ast.Module(mod_name,decls) ->
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
  | Ast.DefVar ("_",e) -> 
     rw_decl mod_name genv @@
       let name = gensym ~prefix:"voidExpr" in
       Ast.DefVar (name,e)
  | Ast.DefVar (name,e) ->
     let name_init = "__init__" ^ name in
     let i,genv' = genv_extend genv name in
     let genv0 = {genv' with init = (mod_name ^ "." ^ name_init) :: genv'.init} in
     let genv1,d1 = rw_decl mod_name genv0 @@
                      Ast.DefFun ([name_init,[],Ast.Ext(Ast.SetGlobal(e,i))]) in
     let genv2,d2 = rw_decl mod_name genv1 @@
                      Ast.DefFun ([name,[],Ast.Ext(Ast.ReadGlobal(i))]) in
     (genv2,(d1 @ d2))
  | Ast.DefFun l -> rw_defun mod_name genv l
  | Ast.DefFunRec l -> rw_defun mod_name genv ~recflag:true l
  | _ -> (genv,[])
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
  | Ast.Constant c -> rw_constant lenv genv c
  | Ast.Ident (name) ->
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
              | Some i -> (* Kast.Variable(Kast.Argument (0)); *)
                 Kast.Variable(Kast.Free (i)) ) 
          | Some i -> Kast.Variable(Kast.Argument (i)))
      | Some i -> Kast.Variable(Kast.Local i))
  | Ast.Let(name,e1,e2) ->
     let i,lenv' = lenv_extend_tail name lenv in (* optimisation : recyclage des variables masquées, 
                                                   contrainte pour la génération de code : dans, C[let x = e1(x) in e2] C[e1] doit bien manipuler le nouveau [x] et [e1] l'ancien *)
     Kast.Let(i,rw_exp lenv genv e1, rw_exp lenv' genv e2)
 
 | Ast.Fun _ -> assert false (* déjà transformer en fermeture *)

 | Ast.Closure((id,c),name,v) ->
      (* après lambda-lifting *)
     let lenv_code = { lenv with 
                       free = (associate 1 (match v with 
                              | Ast.Block(addr::l) -> List.map (function (Ast.Ident(sym)) -> sym | _ -> assert false) l
                              | _ -> assert false));
                       arguments=[(name,1)];
                       locals=[]} in
     let i,lenv_v = lenv_extend name lenv in
     let kc = rw_exp lenv_code genv c in
     let kv = rw_exp lenv_v genv v in (* lenv_v *)
     Kast.Closure((id,kc),kv)
  | Ast.App(e,args) ->
     Kast.App(rw_exp lenv genv e, List.map (rw_exp lenv genv) args)
  | Ast.If(e1,e2,e3) ->
     Kast.If(rw_exp lenv genv e1,
             rw_exp lenv genv e2,
             rw_exp lenv genv e3)
  | Ast.BinOp(op,e1,e2) ->
     Kast.BinOp(op,rw_exp lenv genv e1,rw_exp lenv genv e2)
  | Ast.UnOp(op,e1) -> Kast.UnOp(op,rw_exp lenv genv e1)
  | Ast.Ext(ext) ->
     (match ext with 
      | Ast.Array_alloc(e) ->
         rw_exp lenv genv @@
           Ast.App(Ast.(Ident("Array.create_uninitialized"),[e])) 
      | Ast.SetGlobal(e,i) ->
         Kast.Ext(Kast.SetGlobal (rw_exp lenv genv e,i))
      | Ast.ReadGlobal(i) ->
         Kast.Ext(Kast.ReadGlobal(i))
      | Ast.Label(s,e) -> 
         Kast.Ext(Kast.Label(s,rw_exp lenv genv e))
      | Ast.Goto(s,xs) -> 
         Kast.Ext(Kast.Goto(s,List.map (rw_exp lenv genv) xs)))
  | Ast.Block(xs) ->
     rw_exp lenv genv @@
       let n = List.length xs in
       let a = gensym ~prefix:"tmp" in
       Ast.Let(a,Ast.Ext(Ast.Array_alloc(Ast.Constant(Ast.Int(n)))),
               let rec aux i = function
                 | [] ->
                    Ast.Ident(a)
                 | e::es ->
                    Ast.Seq(Ast.App (Ast.Ident("Internal.array_set"),
                                     [  Ast.Ident(a);
                                        Ast.Constant(Ast.Int(i));
                                        e]),
                            aux (i+1) es) in aux 0 xs)                        
  | Ast.Seq(e1,e2) ->
     Kast.Seq(rw_exp lenv genv e1, rw_exp lenv genv e2)
  | Ast.While(e1,e2) ->
     Kast.While(rw_exp lenv genv e1, rw_exp lenv genv e2)

  | Ast.Match (e,ms) -> 
     let ms',otherw = let rec aux acc = function
                        | [] ->
                           (acc,None)
                        | Ast.Otherwise e :: _ ->
                           (acc,Some e)
                        | h::t ->
                           aux (h::acc) t in aux [] ms in
     let sms = List.map (function 
                   | Ast.Case(Ast.Int(n),e) -> 
                      (* NB : on va trier les constantes : mais avant celà, *)
                      (* il faut absolument renommer les constructeurs par leur entier associé, *)
                      (* sinon, la liste ne sera pas triée correctement et cela produira des bug *)
                      (* dans la génération de code *)
                      (Ast.Int(n),e)
                   | Ast.Case(c,e) -> (c,e)
                   | _ -> assert false) ms'
     in
     let smst = List.sort (fun (c1,_) (c2,_) -> compare c1 c2) sms in
     let var = gensym ~prefix:"L" in
     rw_exp lenv genv @@
       Ast.Let(var,e,
               let rec aux = function
                 | [] -> (match otherw with 
                          | None ->
                             Ast.App(Ast.Ident("Pervasives.exit"),
                                     [Ast.Constant (Ast.Int(1))]) (* match failure *)
                          | Some e ->
                             e)
                 | [(_,h)] -> h
                 | ms -> let (md,e') = List.nth ms (List.length ms / 2) in
                         let l1,l2 = List.partition (fun (c,_) -> c < md) ms in
                         let l2' = List.tl l2 in (* je vire celui qui est égal *)
                         Ast.If(Ast.BinOp(Ast.Lt,
                                          Ast.Ident(var),
                                          Ast.Constant(md)),
                                aux l1,Ast.If(Ast.BinOp(Ast.Eq,
                                                        Ast.Ident(var),
                                                        Ast.Constant(md)),e',aux l2')) in
               aux smst) 
and rw_constant lenv genv c = match c with 
 | Ast.Unit -> Kast.Constant(Kast.Unit)
 | Ast.Int n -> Kast.Constant(Kast.Int n)
 | Ast.Char c -> Kast.Constant(Kast.Int (int_of_char c))
 | Ast.Bool b -> Kast.Constant(Kast.Bool b)
 | Ast.Array_empty -> Kast.Constant(Kast.Array_empty)
 | Ast.String(s) ->
   let rev_xs = ref [] in
   String.iter (fun c -> rev_xs := (c :: !rev_xs)) s;
   rw_exp lenv genv @@
     Ast.Block(List.rev_map (fun c -> Ast.Constant(Ast.Char(c))) !rev_xs) 

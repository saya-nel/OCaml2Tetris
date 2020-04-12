(* repris et étendu de https://www.lri.fr/~filliatr/ens/compil/td/7/corrige/corrige.ml.html *)

open Types
open Past

let get_tyopt = function
| None -> Tvar (V.create ()) 
| Some ty -> ty

let unify_opt t tyopt loc = 
match tyopt with
| None -> ()
| Some ty -> unify t ty loc

let initial_env primitives =
  List.fold_right (fun (x,c,ty) env -> add true x ty env) primitives empty_env;;

(* algorithme W *)
let rec w env decs = function
| [] -> decs
| d::ds -> let xts = w_dec env d in
     List.iter (fun (x,t) -> Printf.printf "%s : %s\n" x (Past_print.sprint_ty 0 t)) xts; 
     let env' = List.fold_left (fun env (x,t) -> add true x (canon t) env) env xts in
     w env' (decs @ xts) ds 
and w_dec env {decl_desc;decl_loc} = 
match decl_desc with
| DefVar ((x,tyopt),e) -> 
  let t = w_exp env e in
  unify_opt t tyopt decl_loc;
  ([(x,t)])
| DefFun funs -> 
  let funtys = List.map (fun f -> w_defun env f decl_loc) funs in
  (List.map2 (fun (f,_,_,_) tf -> (f,tf)) funs funtys)
| DefFunRec funs -> 
  let funcs = List.map (fun (x,_,_,_) -> x) funs in
  let env = List.fold_left (fun env f ->
                               let v0 = Tvar (V.create ()) in 
                               add true f v0 env) env funcs in
  let env' = List.fold_left (fun env ((x,args,tyopt,e) as f) -> 
                               let t = w_defun env f decl_loc in 
                               add true x t env) env funs in
  List.map (fun (x,_,_,_) -> (x,find x decl_loc env')) funs
| Type (name,_,Exp_ty ty) -> 
  (Types.alias := (name,ty) :: !Types.alias); (* A REVOIR, c'EST OK *)
  []
  | Type (name,_,Sum cs) -> 
    (Types.alias := (name,Trec name) :: !Types.alias);   (* ATTENTION ERREUR, type compatible avec tout *)
    List.map (fun (c,tys) -> 
                (c,List.fold_right (fun ty tn -> Tarrow(ty,tn)) tys (* (Tident name) *)
                                                               (Trec name))) cs
and w_defun env (f,args,tyropt,e) decl_loc = 
  let env' = List.fold_left 
               (fun env (xi,tyopt) -> 
                  let ty = get_tyopt tyopt in
                  add false xi ty env) env args in
  let tret = w_exp env' e in
  unify_opt tret tyropt decl_loc;
  let t = List.fold_right (fun (xi,_) t -> let ti = find xi decl_loc env' in Tarrow(ti,t)) args tret in
  t


and w_exp env {exp_desc;exp_loc} = 
let unify t1 t2 = unify t1 t2 exp_loc in
match exp_desc with
  | Annotation (e,ty) -> 
    let t1 = w_exp env e in
    unify ty t1;
    ty
  | Constant c -> (w_constant env exp_loc c)
  | Ident x -> 
    find x exp_loc env
  | Let ((x,tyopt), e1, e2) ->
      let t1 = w_exp env e1 in
      let ty = get_tyopt tyopt in
      unify t1 ty;
      let env' = match x with s -> add true s t1 env in
      w_exp env' e2
  | Fun ((x,tyopt), e1) ->
      let ty = get_tyopt tyopt in
      let env = add false x ty env in
      let t1 = w_exp env e1 in
      Tarrow (ty, t1)
  | App (ef, es) ->
      let t1 = w_exp env ef in
      let ts = List.map (w_exp env) es in
      let v = Tvar (V.create ()) in
      let t2 = List.fold_right (fun t acc -> Tarrow (t,acc)) ts v in
      unify t1 t2;
      v
  | BinOp (op,e1,e2) ->
    let top = w_binop op in
    let t1 = w_exp env e1 in
    let t2 = w_exp env e2 in
    let v = Tvar (V.create ()) in
    unify top (Tarrow (t1,Tarrow(t2,v)));
    v
  | UnOp (op,e1) ->
    let top = w_unop op in
    let t1 = w_exp env e1 in
    let v = Tvar (V.create ()) in
    unify top (Tarrow (t1,v));
    v
  | If (e1, e2, e3) -> 
      let t1 = w_exp env e1 in
      let t2 = w_exp env e2 in
      let t3 = w_exp env e3 in
      unify t1 Tbool;
      unify t2 t3; t2
  | Match (e1,ms) ->
      let t1 = w_exp env e1 in
      let aux t = function 
      | Case(c,args,e) -> let tc = (w_constant env exp_loc c) in
                          let tret, tyargs, arity = let rec aux acc accn = function
                                             | Tarrow (ty,t) -> aux (ty::acc) (accn+1) t
                                             | r -> (r,List.rev acc, accn) in aux [] 0 tc in
                          let len = List.length args in
                          if arity != len then
                          (Printf.printf "Error : This constructor expects %d argument(s),\n\
                                          \ but is applied here to %d argument(s)\n\n%s. exit." arity len (Parseutils.string_of_position exp_loc);
                           exit 0);
                          let env = List.fold_right2 
                                      (fun x ty env -> add false x ty env) args tyargs env in
                       (* unify t (w_constant env exp_loc c); *)
                       w_exp env e
      | Otherwise e -> w_exp env e in
      (match ms with 
       | [] -> assert false
       | ms -> let v = Tvar (V.create ()) in  
               List.iter (fun m -> unify (aux t1 m) v) ms; v)
    (* .... *)
   | Pair(e1,e2) -> 
      let t1 = w_exp env e1 in
      let t2 = w_exp env e2 in
      Tproduct (t1,t2)   
   | Cons(e1,e2) -> 
      let t1 = w_exp env e1 in
      let t2 = w_exp env e2 in
      unify t2 (Tlist t1); t2
   | Array_create (es) -> 
      let v = Tvar (V.create ()) in 
      List.iter (fun e -> unify (w_exp env e) v) es; 
      Tarray v
   | Array_assign (e1,e2,e3) ->
     let t1 = w_exp env e1 in
     let t2 = w_exp env e2 in
     let t3 = w_exp env e3 in
     unify t2 Tint;
     unify t1 (Tarray t3);
     Tunit
   | Array_access (e1,e2) ->
     let t1 = w_exp env e1 in
     let t2 = w_exp env e2 in
     let v = Tvar (V.create ()) in 
     unify t1 (Tarray v);
     unify t2 Tint;
     v
   | Ref(e1) -> 
     let t1 = w_exp env e1 in
     Tref t1
   | Ref_access(e1) ->
     let t1 = w_exp env e1 in
     let t = Tvar (V.create ()) in 
     unify t1 (Tref t);
     t
   | Ref_assign(e1,e2) ->
     let t1 = w_exp env e1 in
     let t2 = w_exp env e2 in
     unify t1 (Tref t2);
     Tunit
   (* | String ->  *)
   | Seq(e1,e2) -> 
     let t1 = w_exp env e1 in
     let t2 = w_exp env e2 in
     unify t1 Tunit;
     t2
   | While(e1,e2) ->
     let t1 = w_exp env e1 in
     let t2 = w_exp env e2 in
     unify t1 Tbool;
     unify t2 Tunit;
     Tunit
   | For(x,e1,e2,e3) ->
     let t1 = w_exp env e1 in
     let t2 = w_exp env e2 in
     unify t1 Tint;
     unify t2
      Tint;
     let env' = add true x Tint env in
     let t3 = w_exp env' e3 in
     unify t3 Tunit;
     Tunit
   | Assert (e,_) -> 
     let ty = w_exp env e in 
     unify ty Tbool; 
     Tunit
   | Magic e -> let _ = w_exp env e in Tvar (V.create ())

and w_constant env exp_loc = function
| Unit -> Tunit
| Bool _ -> Tbool
| Int _ -> Tint 
| Char _ -> Tchar
| String _ -> Tstring
| List_empty -> let v = Tvar (V.create ()) in Tlist v
| List_cons -> let v = Tvar (V.create ()) in Tarrow(v,Tlist v)
| Array_empty -> let v = Tvar (V.create ()) in Tarray v
| Constr s -> find s exp_loc env

and w_binop = function
| Add -> Tarrow(Tint,Tarrow(Tint,Tint))
| Minus -> Tarrow(Tint,Tarrow(Tint,Tint))
| Mult -> Tarrow(Tint,Tarrow(Tint,Tint))
| Div -> Tarrow(Tint,Tarrow(Tint,Tint))
| Lt -> Tarrow(Tint,Tarrow(Tint,Tbool))
| Le -> Tarrow(Tint,Tarrow(Tint,Tbool))
| Neq -> Tarrow(Tint,Tarrow(Tint,Tbool))
| Eq -> Tarrow(Tint,Tarrow(Tint,Tbool))
| Ge -> Tarrow(Tint,Tarrow(Tint,Tbool))
| Gt -> Tarrow(Tint,Tarrow(Tint,Tbool))
| Or -> Tarrow(Tbool,Tarrow(Tbool,Tbool))
| And -> Tarrow(Tbool,Tarrow(Tbool,Tbool))
| Lor -> Tarrow(Tint,Tarrow(Tint,Tint))
| Land -> Tarrow(Tint,Tarrow(Tint,Tint))

and w_unop = function
| UMinus -> Tarrow(Tint,Tint)
| Not -> Tarrow(Tbool,Tbool)

(* initial_env prims *) 

let type_check {decls;mod_name} env = 
  Printf.printf "=== module %s ==================.\n" mod_name;
  try let decs = w env [] decls in 
      let env = List.fold_left (fun env (x,t) -> 
        add true (mod_name ^ "." ^ x) (canon t) env) env decs
      in env
  with 
  | UnificationFailure (t1,t2,loc) -> 
      Printf.printf "\nError: %s\nThis expression has type %s but an expression was expected of type
         %s\n" (Parseutils.string_of_position loc) (Past_print.sprint_ty 0 t1) (Past_print.sprint_ty 0 t2); exit 0
  | Unbound_value (x,loc) -> Printf.printf "Error: %s\nUnbound value %s\n" (Parseutils.string_of_position loc)  x; exit 0


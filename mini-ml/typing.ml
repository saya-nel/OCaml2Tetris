(* repris et étendu de https://www.lri.fr/~filliatr/ens/compil/td/7/corrige/corrige.ml.html *)

open Types
open Ast

let initial_env primitives =
  List.fold_right (fun (x,c,ty) env -> add true x ty env) primitives empty_env;;

(* algorithme W *)
let rec w env decs = function
| [] -> decs
| d::ds -> let xts = w_dec env d in
     List.iter (fun (x,t) -> Printf.printf "%s : %s\n" x (Print_ast.sprint_ty 0 t)) xts; 
     let env' = List.fold_left (fun env (x,t) -> add true x (canon t) env) env xts in
     w env' (decs @ xts) ds 
and w_dec env = function
| Exp (e) -> 
  let t = w_exp env e in 
  ([("_",t)])
| DefVar ((x,tyopt),e) -> 
  let t = w_exp env e in
  (match tyopt with None -> () | Some ty -> unify t ty);
  ([(x,t)])
| DefFun funs -> 
  let funtys = List.map (w_fun env) funs in
  (List.map2 (fun (f,_,_) tf -> (f,tf)) funs funtys)
| DefFunRec funs -> 
  let env' = List.fold_left (fun env ((x,args,e) as f) -> 
                               let t = w_funrec env f in 
                               add true x t env) env funs in
  List.map (fun (x,_,_) -> (x,find x env')) funs
| Type _ -> ([]) (* failwith "todo" *)
and w_fun env (f,args,e) = 
  let env' = List.fold_left 
               (fun env xi -> 
                  let v = Tvar (V.create ()) in
                  add false xi v env) env args in
  let tret = w_exp env' e in
  let t = List.fold_right (fun xi t -> let ti = find xi env' in Tarrow(ti,t)) args tret in
  t

and w_funrec env (f,args,e) =  (* un peu trop bricoller, ne marche pas *)
  let v0 = Tvar (V.create ()) in
  let env' = List.fold_left 
               (fun env xi -> 
                  let v = Tvar (V.create ()) in
                  add false xi v env) (add true f v0 env) args in
  let tret = w_exp env' e in
  let tf = List.fold_right (fun xi t -> let ti = find xi env' in Tarrow(ti,t)) args tret in
  let env2 = List.fold_left 
               (fun env xi -> 
                  let ti = find xi env' in 
                  add false xi ti env) (add true f tf env) args in
  let tret = w_exp env2 e in
  let tf = List.fold_right (fun xi t -> let ti = find xi env' in Tarrow(ti,t)) args tret in
  tf

and w_exp env = function
  | Annotation (e,ty) -> 
    let t1 = w_exp env e in
    unify t1 ty;
    t1
  | Constant c -> (w_constant c)
  | Ident x -> 
      find x env
  | Let ((x,tyopt), e1, e2) ->
      let t1 = w_exp env e1 in
      (match tyopt with None -> () | Some ty -> unify t1 ty);
      let env' = match x with s -> add true s t1 env in
      w_exp env' e2
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
      | Case(c,e) -> unify t (w_constant c); w_exp env e
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
   | Array_alloc e1 ->
     unify (w_exp env e1) Tint;
     let v = Tvar (V.create ()) in
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
     let v = Tvar (V.create ()) in 
     unify (Tref t1) v;
     v
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
   | _ -> failwith "private"

and w_constant = function
| Unit -> Tunit
| Bool _ -> Tbool
| Int _ -> Tint 
| Char _ -> Tchar
| String _ -> Tstring
| List_empty -> let v = Tvar (V.create ()) in Tlist v
| Array_empty -> let v = Tvar (V.create ()) in Tarray v
| Constr _ -> failwith "todo" 

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

let type_check Ast.{decls;mod_name} env = 
  try let decs = w env [] decls in 
      let env = List.fold_left (fun env (x,t) -> 
        add true (mod_name ^ "." ^ x) (canon t) env) env decs
      in env
  with 
  | UnificationFailure (t1,t2) -> 
      Printf.printf "Error: This expression has type %s but an expression was expected of type
         %s\n" (Print_ast.sprint_ty 0 t1) (Print_ast.sprint_ty 0 t2); exit 0
  | Unbound_value x -> Printf.printf "Error: Unbound value %s\n" x; exit 0


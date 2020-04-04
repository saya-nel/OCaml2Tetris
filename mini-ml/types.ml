(* repris et étendu de https://www.lri.fr/~filliatr/ens/compil/td/7/corrige/corrige.ml.html *)

type typid = string

type typ =
  | Tint
  | Tbool
  | Tunit
  | Tchar
  | Tstring
  | Tvar of tvar
  | Tarrow of typ * typ
  | Tproduct of typ * typ
  | Tlist of typ
  | Tarray of typ
  | Tref of typ
  | Tconstr of (typid * typ list)


and tvar =
  { id : int;
    mutable def : typ option }

let rec copy ty = match ty with
| Tint | Tbool | Tunit | Tchar | Tstring -> ty
| Tvar {id;def} -> Tvar {id;def}
| Tarrow (t1,t2) -> Tarrow (copy t1,copy t2)
| Tproduct (t1,t2) -> Tproduct (copy t1,copy t2)
| Tlist t -> Tlist (copy t)
| Tarray t -> Tarray (copy t)
| Tref t -> Tref (copy t)
| Tconstr (c,t) -> Tconstr (c,List.map copy t)

(* module V pour les variables de type *)

module V = struct
  type t = tvar
  let compare v1 v2 = Pervasives.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

(* réduction en tête d'un type (la compression de chemin serait possible) *)
let rec head = function
  | Tvar { def = Some t } -> head t
  | t -> t

(* forme canonique d'un type = on applique head récursivement *)
let rec canon t = match head t with
  | Tvar _ | Tint | Tbool | Tunit | Tchar | Tstring as t -> t
  | Tarrow (t1, t2) -> Tarrow (canon t1, canon t2)
  | Tproduct (t1, t2) -> Tproduct (canon t1, canon t2)
  | Tlist t -> Tlist (canon t)
  | Tarray t -> Tarray (canon t)
  | Tref t -> Tref (canon t)
  | Tconstr (c,ts) -> Tconstr (c,List.map canon ts)

(* tests *)
let rec print = function
  | Tunit ->  "unit"
  | Tint -> "int"
  | Tbool -> "bool"
  | Tchar -> "char"
  | Tstring -> "string"
  | Tarrow (ty1, ty2) -> Printf.sprintf "(%s -> %s)" (print ty1) (print ty2)
  | Tproduct (ty1, ty2) -> Printf.sprintf "(%s * %s)" (print ty1) (print ty2)
  | Tvar v -> print_tvar v
  | Tlist ty -> Printf.sprintf  "(%s list)" (print ty)
  | Tarray ty -> Printf.sprintf  "(%s array)" (print ty)
  | Tref ty -> Printf.sprintf  "(%s ref)" (print ty)
  | Tconstr (c,ty1) -> failwith "todo"
and print_tvar v =
  (* Format.fprintf fmt "'%d" v.id;
  match v.def with None -> () | Some ty -> Format.fprintf fmt "[:=%a]" print ty *)
   match v.def with 
   | None -> Printf.sprintf "'v%d" v.id
   | Some ty -> print ty

let () =
  let a = V.create () in
  let b = V.create () in
  let ta = Tvar a in
  let tb = Tvar b in
  assert (head ta == ta);
  assert (head tb == tb);
  let ty = Tarrow (ta, tb) in
  a.def <- Some tb;
  assert (head ta == tb);
  assert (head tb == tb);
  b.def <- Some Tint;
  assert (head ta = Tint);
  assert (head tb = Tint);
  assert (canon ta = Tint);
  assert (canon tb = Tint);
  assert (canon ty = Tarrow (Tint, Tint))

(* unification *)

exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))

let rec occur v t = match head t with
  | Tvar w -> V.equal v w
  | Tarrow (t1, t2) | Tproduct (t1, t2) -> occur v t1 || occur v t2
  | Tlist t1 | Tarray t1 | Tref t1 -> occur v t1
  | Tconstr (_,ts) -> List.for_all (occur v) ts
  | Tunit | Tint | Tbool | Tchar | Tstring -> false

let rec unify t1 t2 = match head t1, head t2 with
  | Tunit, Tunit -> ()
  | Tint, Tint -> ()
  | Tbool, Tbool -> ()
  | Tchar, Tchar -> ()
  | Tstring, Tstring -> ()
  | Tvar v1, Tvar v2 when V.equal v1 v2 -> ()
  | Tvar v1 as t1, t2 ->
      if occur v1 t2 then unification_error t1 t2;
      assert (v1.def = None);
      v1.def <- Some t2
  | t1, Tvar v2 ->
      unify t2 t1
  | Tarrow (t11, t12), Tarrow (t21, t22)
  | Tproduct (t11, t12), Tproduct (t21, t22) ->
      unify t11 t21; unify t12 t22
  | Tlist t, Tlist (t') | Tarray t, Tarray (t') | Tref t, Tref (t')  -> 
      unify t t'
  | Tconstr (c,ts), Tconstr (c',ts') -> if c <> c' then unification_error t1 t2 else
                                      List.iter2 unify ts ts'
  | t1, t2 ->
      unification_error t1 t2

let () =
  let a = V.create () in
  let b = V.create () in
  let ta = Tvar a in
  let tb = Tvar b in
  assert (occur a ta);
  assert (occur b tb);
  assert (not (occur a tb));
  let ty = Tarrow (ta, tb) in
  assert (occur a ty);
  assert (occur b ty);
  (* unifie 'a-> 'b et int->int *)
  unify ty (Tarrow (Tint, Tint));
  assert (canon ta = Tint);
  assert (canon ty = Tarrow (Tint, Tint));
  (* unifie 'c et int->int *)
  let c = V.create () in
  let tc = Tvar c in
  unify tc ty;
  assert (canon tc = Tarrow (Tint, Tint))

let cant_unify ty1 ty2 =
  try let _ = unify ty1 ty2 in false with UnificationFailure _ -> true

let () =
  assert (cant_unify Tint (Tarrow (Tint, Tint)));
  assert (cant_unify Tint (Tproduct (Tint, Tint)));
  let a = V.create () in
  let ta = Tvar a in
  unify ta (Tarrow (Tint, Tint));
  assert (cant_unify ta Tint)

(* schéma de type *)

module Vset = Set.Make(V)

type schema = { vars : Vset.t; typ : typ }

(* variables libres *)

let rec fvars t = match head t with
  | Tunit | Tint | Tbool | Tchar | Tstring -> Vset.empty
  | Tarrow (t1, t2) | Tproduct (t1, t2) -> Vset.union (fvars t1) (fvars t2)
  | Tlist t | Tarray t | Tref t -> (fvars t)
  | Tconstr (c,ts) ->  List.fold_left (fun acc t -> Vset.union acc (fvars t)) Vset.empty ts
  | Tvar v -> Vset.singleton v

let norm_varset s =
  Vset.fold (fun v s -> Vset.union (fvars (Tvar v)) s) s Vset.empty

let () =
  assert (Vset.is_empty (fvars (Tarrow (Tint, Tint))));
  let a = V.create () in
  let ta = Tvar a in
  let ty = Tarrow (ta, ta) in
  assert (Vset.equal (fvars ty) (Vset.singleton a));
  unify ty (Tarrow (Tint, Tint));
  assert (Vset.is_empty (fvars ty))

(* environnement c'est une table bindings (string -> schema),
   et un ensemble de variables de types libres *)


module Smap = Map.Make(String)

type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty_env = { bindings = Smap.empty; fvars = Vset.empty }

let add gen x t env =
  let vt = fvars t in
  let s, fvars =
    if gen then
      let env_fvars = norm_varset env.fvars in
      { vars = Vset.diff vt env_fvars; typ = t }, env.fvars
    else
      { vars = Vset.empty; typ = t }, Vset.union env.fvars vt
  in
  { bindings = Smap.add x s env.bindings; fvars = fvars }

module Vmap = Map.Make(V)

(* find x env donne une instance fraîche de env(x) *)
let find x env =
  let tx = Smap.find x env.bindings in
  let s =
    Vset.fold (fun v s -> Vmap.add v (Tvar (V.create ())) s)
      tx.vars Vmap.empty
  in
  let rec subst t = match head t with
    | Tvar x as t -> (try Vmap.find x s with Not_found -> t)
    | Tunit -> Tunit
    | Tint -> Tint
    | Tbool -> Tbool
    | Tchar -> Tchar
    | Tstring -> Tstring
    | Tarrow (t1, t2) -> Tarrow (subst t1, subst t2)
    | Tproduct (t1, t2) -> Tproduct (subst t1, subst t2)
    | Tlist t -> Tlist (subst t)
    | Tarray t -> Tarray (subst t)
    | Tref t -> Tref (subst t)
    | Tconstr (c,ts) -> Tconstr (c,List.map subst ts)
  in
  subst tx.typ

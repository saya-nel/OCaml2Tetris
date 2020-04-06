
type collect = Kast.var list 

let coll = ref []

let rec collect e =
  coll := [];
  collect_exp e;
  !coll

and collect_exp = function
  | Kast.Constant c -> ()
  | Kast.Variable v -> collect_var v 
  | Kast.If(e1,e2,e3) ->
    collect_exp e1; 
    collect_exp e2; 
    collect_exp e3
  | Kast.While(e1,e2) ->
    collect_exp e1; 
    collect_exp e2
  | Kast.Fun(e,ka,kl) -> 
      collect_exp e
  (* une valeur fonctionnelle (close) est l'entier
     associé au code de la fonction dans Apply.apply *)
  | Kast.Let(n,e1,e2) ->
    collect_exp e1; 
    collect_exp e2
  | Kast.Seq(e1,e2) ->
    collect_exp e1; 
    collect_exp e2
  | Kast.App(f,args) ->
    collect_exp f; 
    List.iter collect_exp args
  | Kast.BinOp(op,e1,e2) ->
    collect_exp e1; 
    collect_exp e2
  | Kast.UnOp(op,e1) ->
    collect_exp e1
  | Kast.SetGlobal (e1,i) ->
    collect_exp e1
  | Kast.ReadGlobal (i) -> ()
  | Kast.GFun (name) -> ()
and collect_var v = match v with
| Kast.Argument _ | Kast.Local _ | Kast.Free _ -> coll := v :: !coll
| Kast.Global _ -> ()
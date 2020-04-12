(* extrait dans des variables globales les chaînes du programme, 
pour éviter qu'elles ne soient allouées plusieurs fois *)

let gensym = 
  let c = ref 0 in 
  (fun () ->
    incr c;
    Printf.sprintf "__lambda%d" !c)


let create () = ref []

(* globalise les chaînes de caractères dans le module mdl *)  

let rec rewrite ?(depth_max=5) m =
  match m with Ast.Module(mod_name,decls) ->
    
    let decls = List.concat @@
                List.map (fun d -> let collect = create () in 
                                   let d = rw_decl collect d in
                                   List.rev_append (!collect) [d]) decls in(* rev important *)
    Ast.Module(mod_name,decls)


and rw_decl cl = function
  | Ast.DefVar(v,e) -> Ast.DefVar(v,rw_exp cl [] [] e)
  | Ast.DefFun(l) -> Ast.DefFun (rw_fundecs cl l)
  | Ast.DefFunRec(l) -> Ast.DefFunRec (rw_fundecs cl l)
  | d -> d 
and rw_fundecs cl lf = 
  List.map (fun (name,args,e) -> (name,args,rw_exp cl [] [] e)) lf
and rw_exp cl env lenv = function
  | Ast.Fun (name,e) -> let env = lenv @ env in
                        let lenv = [name] in
                        let e = rw_exp cl env lenv e in
                        let sym = gensym () in
                        let vars = Freevr.collect env lenv e in
                        (match vars with 
                         |[] -> Ast.Fun (name,e) 
                         (* let d = Ast.DefFun([(sym,[name],e)]) in
                                cl := d :: !cl;
                                Ast.Ident (sym) *)
                         | _ ->  (* let d = Ast.DefFun([(sym,vars @ [name], e)]) in  (y aurait il moyen d'en faire de vraies fonctions globales ...? *)
                                 let d = Ast.DefVar(sym,List.fold_right (fun x e -> Ast.Fun (x,e)) (vars @ [name]) e) in
                                 cl := d :: !cl;
                                 Ast.App(Ast.Ident (sym),List.map (fun v -> Ast.Ident(v)) vars))
  (* cl := Ast.DefFun([(sym,name::vars,e')]) :: !cl;*)       
  | Ast.Let(name,e1,e2) -> let lenv = name :: lenv in
                           Ast.Let(name,rw_exp cl env lenv e1,rw_exp cl env lenv e2)
  | Ast.Ident name -> Ast.Ident name
  | Ast.Constant c -> Ast.Constant c
  | Ast.App(e,args) -> Ast.App(rw_exp cl env lenv e,List.map (rw_exp cl env lenv) args) 
  | Ast.If(e1,e2,e3) -> Ast.If(rw_exp cl env lenv e1,rw_exp cl env lenv e2,rw_exp cl env lenv e3)
  | Ast.BinOp(op,e1,e2) -> Ast.BinOp(op,rw_exp cl env lenv e1,rw_exp cl env lenv e2)
  | Ast.UnOp(op,e1) -> Ast.UnOp(op,rw_exp cl env lenv e1)
  | Ast.Block(xs) -> Ast.Block(List.map (rw_exp cl env lenv) xs)
  | Ast.Seq(Ast.Constant(Ast.Unit),e2) -> rw_exp cl env lenv e2
  | Ast.Seq(e1,e2) -> Ast.Seq(rw_exp cl env lenv e1,rw_exp cl env lenv e2)
  | Ast.While(e1,e2) -> Ast.While(rw_exp cl env lenv e1,rw_exp cl env lenv e2)
  | Ast.Match(e,ms) -> Ast.Match (rw_exp cl env lenv e,List.map (function Ast.Case(c,e) -> Ast.Case(c,rw_exp cl env lenv e) | Ast.Otherwise e -> Ast.Otherwise(rw_exp cl env lenv e)) ms) 
  | e -> e

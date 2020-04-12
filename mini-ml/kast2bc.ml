open Bc

let mapcat f l = List.concat (List.map f l)
let rev_mapcat f l = List.concat (List.rev_map f l)

let nb_local bc =
  List.fold_left
    (function n ->
               function
               | Pop(Local i) -> max (i+1) n
               | _ -> n) 0 bc

let gensym = 
  let c = ref 0 in 
  (fun prefix ->
    incr c;
    Printf.sprintf "%s%d" prefix !c)

let lambda_code = ref []

let apply_code = ref [Label "EndApply";Return] 


let next_closure bc_e adr = 
  let l = ("A" ^ string_of_int adr) in
  let f = ("Apply.closure" ^ string_of_int adr) in
  lambda_code := ([Function (f,nb_local bc_e)] @ bc_e @ [Return]) @ !lambda_code;
  apply_code := [Push(Argument(0));   (* arg0 : [|code;addr;vars...|] *)
                 Push(Constant(0));  
                 Call("Internal.array_get",2);
                 Push(Constant(adr)); 
                 BinOp(Eq);
                 UnOp(Not);         (* si (pointeur de code /= adr) *)   
                 IfGoto l;          (* chercher à l'adresse suivante *)
                 (* Push(Argument(1)); *) (* sinon, appele le code à l'adresse adr, en lui passant le paramètre effectif (arg1) et l'environnement (arg0)) *)
                 Push(Argument(0)); (* la raison pour laquel l'environnement est passé en 2ème (et pas en premier) *)
                                    (* est que le code de la fermeture suppose que l'argument est en position 0 (voir ast2kast.ml) *)
                 Push(Argument(1));
                 Call(f,2);
                 Goto "EndApply";
                 Label l] @ !apply_code


let indent_string = Past_print.indent_string

let rec bc_of_prog bc_mdls =
  let accgb = ref [] in
  let files =
    List.map (function {mod_name;bc_body;init} ->
                accgb := init @ !accgb; 
                (mod_name,bc_body)) bc_mdls in
  let init_globals = List.rev (mapcat (fun g -> [Call (g,0)]) !accgb) in
  let main = ("Start",([Function ("Start.main",0)] @ init_globals @ 
                         [Push(Constant(0));Return])) in
  let apply_file = ("Apply",(Function ("Apply.apply",0) :: !apply_code) @ !lambda_code) in

  apply_file :: main :: files
and bc_of_tmodule genv Kast.{mod_name;decls} = 
  let bc_body = bc_of_decls mod_name decls in
  {mod_name;bc_body;init=Ast2kast.(genv.init)}
and bc_of_decls mod_name ds = 
  mapcat (bc_of_decl mod_name) ds
and bc_of_decl mod_name = function 
  | Kast.DefFun (name,arity,e) ->
     let bc_e = bc_of_exp e in
     let full_name = mod_name ^ "." ^ name in
     (* if full_name = "Main.main" then failwith "nom de fonction reservée" else *)
     [Function (full_name,nb_local bc_e)] @ bc_e @ [Return]
and bc_of_exp = function
  | Kast.Constant c -> bc_of_constant c
  | Kast.Variable v -> bc_of_variable v
  | Kast.If(e1,e2,e3) ->
         let bc_e1 = bc_of_exp e1 
         and bc_e2 = bc_of_exp e2
         and bc_e3 = bc_of_exp e3 in
         let lbl_if_false = gensym "IfFalse"
         and lbl_end = gensym "IfEnd" in
         bc_e1 @ [UnOp Not; IfGoto lbl_if_false] @
           bc_e2 @ [Goto lbl_end; Label lbl_if_false] @
             bc_e3 @ [Label lbl_end]
  | Kast.While(e1,e2) ->
         let bc_e1 = bc_of_exp e1 
         and bc_e2 = bc_of_exp e2 in
         let lbl_begin = gensym "WhileBegin"
         and lbl_end = gensym "WhileEnd" in
         [Label lbl_begin] @ bc_e1 @ [UnOp Not; IfGoto lbl_end] @
           bc_e2 @ [Goto lbl_begin; Label lbl_end]
  | Kast.Closure ((addr,ke),closure_env) ->
     (* let n = next_lambda (bc_of_exp lvl e) *)
     next_closure (bc_of_exp ke) addr;
     bc_of_exp closure_env 
     (* la fermeture est un tableau (module Array)) *)
     (* dont le premier élément est l'id (~+/- l'adresse) de la closure *) 
     (* et les élément suivant sont les "variables libres" (l'environnement). *)
     (* Les fonctions sont unaires : on les utilises avec Apply.apply (code autogénéré) *)
     (* en passant en [argument 0] la fermeture elle même et en [argument 1] son argument. *)
     (* NB : l'argument de la fonction ne fait pas parti de l'environnement stocké dans la fermeture. *)
  | Kast.Let(n,e1,e2) ->
         let bc_e1 = bc_of_exp e1
         and bc_e2 = bc_of_exp e2 in
         bc_e1 @ [Pop(Local n)] @ bc_e2
  | Kast.Seq(e1,e2) ->
         let bc_e1 = bc_of_exp e1 
         and bc_e2 = bc_of_exp e2 in
         bc_e1 @ [Pop Anywhere] @ bc_e2
  | Kast.App(e,args) ->
           (match e with
              (* appel de fonctions globales *)
            | Kast.GFun (name) -> let arity = List.length args in 
                                  mapcat bc_of_exp args @ [Call(name,arity)]
              (* mécanisme générale : application de fonctions unaires *)
            | _ -> (* calcul de l'expression en position fonctionnelle *)
                   let eB = bc_of_exp e 
                   in (* puis calcul des arguments (contrairement à OCAML) *)
                   let argsB = mapcat (fun e -> bc_of_exp e @ [Call("Apply.apply",2)]) args in         
              eB @ argsB)
           (* C[(e0 e1 ... ek)]            *)
           (* ~> C[(e0 e1 ... e(k-1))];    *)  
           (*    C[ek];                    *)
           (*    Call(Apply.apply,2);      *) 

           (* en encore :                  *)
           (* ~> C[(e0)]                   *)  
           (*    C[(e1)]                   *) 
           (*    Call(Apply.apply,2);      *) 
           (*    C[(e2)]                   *)  
           (*    Call(Apply.apply,2);      *) 
           (*    ...                       *)
           (*    C[(ek)]                   *)  
           (*    Call(Apply.apply,2);      *) 
  | Kast.BinOp(op,e1,e2) ->
         let bc_e1 = bc_of_exp e1 
         and bc_e2 = bc_of_exp e2 in
         bc_e1 @ bc_e2 @ bc_of_binop op
  | Kast.UnOp(op,e1) ->
         let bc_e1 = bc_of_exp e1  in
         bc_e1 @ bc_of_unop op
  | Kast.GFun (name) -> [Call (name,0)] (* assert false  *)
  (* Comme il n'y a pas d'adresse en Nand2Tetris, une fonction globale n'a pas de  *)
  (* valeur (même pas un pointeur). *)

  | Kast.Ext(ext) -> 
     (match ext with 
      | Kast.SetGlobal (e1,i) ->
         let bc_e1 = bc_of_exp e1 in
         bc_e1 @ [Pop (Static(i))] @ [Push (Static(i));Pop (Temp(7))]
      | Kast.ReadGlobal (i) -> 
         [Push (Static(i))]
      | Kast.SetLocal(n,e) -> 
         (bc_of_exp e) @ [Pop (Local n)]
      | Kast.Label (s,e) -> 
         [Label s] @ (bc_of_exp e)
      | Kast.Goto (s,args) -> 
         let xs = mapcat bc_of_exp (List.rev args) in
         let m = List.mapi (fun i _ -> Pop(Argument(i))) args in
         xs @ m @ [Goto s])
and bc_of_constant = function
  | Kast.Unit ->
     [Push (Constant 0)]
  | Kast.Int n ->
     if n >= 0
     then [Push (Constant n)]
     else [Push (Constant 0); Push (Constant (- n)); BinOp(Sub)]
  | Kast.Array_empty ->
     [Push (Constant 0)]
  | Kast.Bool b ->
     if b then [True] else [False]
and bc_of_variable = function
  | Kast.Global name -> 
    [Call (name,0)] (* les variables globales sont des fonction globales d'arité 0  *) 
                     (* avec une étape d'initialisation au démarrage du programme.   *)
                     (* --> la fonction name est un simple "getter"                  *)
                     (* NB : pas de recalcule de l'expression à chaque accès.        *)
  | Kast.Argument (n) ->
     [ Push(Argument n) ]
  | Kast.Local (n) ->
     [ Push(Local n) ]
  | Kast.Free (n) ->
     [Push(Argument 0);Push(Constant (n));Call("Array.get",2)] (* la nieme valeur de l'env, (sans compter l'addresse en position 0) *)
and bc_of_binop = function
  | Ast.Add -> [BinOp Add]
  | Ast.Minus -> [BinOp Sub]
  | Ast.Mult -> [BinOp Mult]
  | Ast.Div -> [BinOp Div]
  | Ast.Eq -> [BinOp Eq] 
  | Ast.Neq -> [BinOp Eq;UnOp Not] 
  | Ast.Gt -> [BinOp Gt]
  | Ast.Lt -> [BinOp Lt]
  | Ast.Le -> [BinOp Gt;UnOp Not]
  | Ast.Ge -> [BinOp Lt;UnOp Not]
  | Ast.And | Ast.Land -> [BinOp And]
  | Ast.Or | Ast.Lor -> [BinOp Or]

and bc_of_unop = function
  | Ast.Not ->
     [UnOp Not]
  | Ast.UMinus ->
     [Pop (Temp(0));Push(Constant(0));Push(Temp(0));BinOp Sub]

open Bc

exception Cannot_generate_bytecode of string

type trace = { mutable status : status }
and status = On | Off

let _TRACE = { status = Off }

let mapcat f l = List.concat (List.map f l)

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

let next_lambda = 
  let c = ref (-1) in
  (fun bc_e -> 
     incr c;
     let k = !c in
     let l = ("A" ^ string_of_int k) in
     let f = ("Apply.lambda" ^ string_of_int k) in
     lambda_code := ([Function (f,nb_local bc_e)] @ bc_e @ [Return]) @ !lambda_code;
     apply_code := [Push(Argument(1));
                    Push(Constant(k));
                    BinOp(Eq);
                    UnOp(Not);
                    IfGoto l;
                    Push(Argument(0));
                    Call(f,1);
                    Goto "EndApply";
                  Label l] @ !apply_code;
    k)

let indent_string = Past_print.indent_string

(* annote le bc *)
let comment name lvl by = 
  match _TRACE with
  | {status=On} ->
     let s = indent_string lvl in
     [Comment (s ^ "(" ^ name) ] @ by @ [Comment (s ^ ")") ]
  | _ -> by

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
  (* let lambda_lifting_file = ("Lambda",*)
   apply_file :: main :: files
and bc_of_tmodule genv Kast.{mod_name;decls} = 
  let bc_body = bc_of_decls mod_name decls in
  {mod_name;bc_body;init=Iast2kast.(genv.init)}
and bc_of_decls mod_name ds = 
  mapcat (bc_of_decl mod_name) ds
and bc_of_decl mod_name = function 
  | Kast.DefFun (name,arity,e) ->
     let bc_e = bc_of_exp 0 e in
     let full_name = mod_name ^ "." ^ name in
     (* if full_name = "Main.main" then failwith "nom de fonction reservée" else *)
     [Function (full_name,nb_local bc_e)] @ bc_e @ [Return]
and bc_of_exp lvl = function
  | Kast.Constant c ->
     comment "<const>" lvl (bc_of_constant c)
  | Kast.Variable v ->
     comment "<var>" lvl (bc_of_variable v)
  | Kast.If(e1,e2,e3) ->
     comment "<if>" lvl (
         let bc_e1 = bc_of_exp (lvl+1) e1 
         and bc_e2 = bc_of_exp (lvl+1) e2
         and bc_e3 = bc_of_exp (lvl+1) e3 in
         let lbl_if_false = gensym "IfFalse"
         and lbl_end = gensym "IfEnd" in
         bc_e1 @ [UnOp Not; IfGoto lbl_if_false] @
           bc_e2 @ [Goto lbl_end; Label lbl_if_false] @
             bc_e3 @ [Label lbl_end])
  | Kast.While(e1,e2) ->
     comment "<while>" lvl (
         let bc_e1 = bc_of_exp (lvl+1) e1 
         and bc_e2 = bc_of_exp (lvl+1) e2 in
         let lbl_begin = gensym "WhileBegin"
         and lbl_end = gensym "WhileEnd" in
         [Label lbl_begin] @ bc_e1 @ [UnOp Not; IfGoto lbl_end] @
           bc_e2 @ [Goto lbl_begin; Label lbl_end])
  | Kast.Fun(e,ka,kl) -> 
      let n = next_lambda (bc_of_exp lvl e) in
      (match ka,kl with 
      | 0,0 -> [Push (Constant n)]
      | _ -> failwith "fonction close uniquement")
  (* une valeur fonctionnelle (close) est l'entier
     associé au code de la fonction dans Apply.apply *)
  | Kast.Let(n,e1,e2) ->
     comment "<let>" lvl (
         let bc_e1 = bc_of_exp (lvl+1) e1
         and bc_e2 = bc_of_exp (lvl+1) e2 in
         bc_e1 @ [Pop(Local n)] @ bc_e2)
  | Kast.Seq(e1,e2) ->
     comment "<seq>" lvl (
         let bc_e1 = bc_of_exp (lvl+1) e1 
         and bc_e2 = bc_of_exp (lvl+1) e2 in
         bc_e1 @ [Pop Anywhere] @ bc_e2)
  | Kast.App(f,args) ->
     comment "<app>" lvl (
         let arity = List.length args in
         mapcat (bc_of_exp (lvl+1)) args @
           (match f with
            | Kast.GFun ("ML_obj.magic") -> [] (* cas particulier : fonction pour influer sur le typeur => pas de calcul *)
            | Kast.GFun (name) -> [Call(name,arity)]
            | _ -> (bc_of_exp (lvl+1) f) @
                   List.map (fun _ -> Call("Apply.apply",2)) args))
          (*  raise (Cannot_generate_bc "limite d'implantation : seules les fonctions globales peuvent être appliquées"))) *)
  | Kast.BinOp(op,e1,e2) ->
     comment "<binop>" lvl (
         let bc_e1 = bc_of_exp (lvl+1) e1 
         and bc_e2 = bc_of_exp (lvl+1) e2 in
         bc_e1 @ bc_e2 @ bc_of_binop op)
  | Kast.UnOp(op,e1) ->
     comment "<unop>" lvl (
         let bc_e1 = bc_of_exp (lvl+1) e1  in
         bc_e1 @ bc_of_unop op)
  | Kast.GFun (name) ->
     [Call (name,0)] (* !!!!! variables globales, bof *)
  | Kast.Ext(ext) -> 
    (match ext with 
     | Kast.SetGlobal (e1,i) ->
       let bc_e1 = bc_of_exp (lvl+1) e1 in
        bc_e1 @ [Pop (Static(i))] @ [Push (Static(i));Pop (Temp(7))]
     | Kast.ReadGlobal (i) -> 
        [Push (Static(i))]
     | Kast.SetLocal(n,e) -> 
       (bc_of_exp (lvl+1) e) @ [Pop (Local n)]
     | Kast.Label (s,e) -> 
        [Label s] @ (bc_of_exp (lvl+1) e)
     | Kast.Goto (s,args) -> 
          let xs = mapcat (bc_of_exp (lvl+1)) (List.rev args) in
          let m = List.mapi (fun i _ -> Pop(Argument(i))) args in
          xs @ m @ [Goto s])
and bc_of_constant = function
  | Kast.Unit ->
     [Push (Constant 0)]
  | Kast.Int n ->
     if n >= 0
     then [Push (Constant n)]
     else [Push (Constant 0); Push (Constant (- n)); BinOp(Sub)]
  | Kast.List_empty ->
     [Push (Constant 0)]
  | Kast.Array_empty ->
     [Push (Constant 0)]
  | Kast.Bool b ->
     if b then [True] else [False]
and bc_of_variable = function
  | Kast.Global name -> [Call (name,0)] (* bc_of_exp 0 @@ Kast.App (Kast.GFun(name),[]) *)  (* les variables globales  sont des fonction d'arité 0 *)
  | Kast.Argument (n) ->
     [ Push(Argument n) ]
  | Kast.Local (n) ->
     [ Push(Local n) ]
  | Kast.Free (n) ->
     [ Push(Local n) ]
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

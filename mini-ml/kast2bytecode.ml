open Bytecode

let mapcat f l = List.concat (List.map f l)

let gensym = 
  let c = ref 0 in 
  (fun prefix -> incr c; Printf.sprintf "%s%d" prefix !c)


let rec bytecode_of_prog bc_mdls =
  let accgb = ref [] in
  let files = List.map (function {mod_name;bc_decls;init} ->
                         accgb := init @ !accgb; 
                        (mod_name,bc_decls)) bc_mdls in
  let init_globals = List.rev (mapcat (fun g -> [Call (g,0)]) !accgb) in
  let main = ("Main",([Function ("Main.main",0)] @ init_globals @ 
                      [Push(Constant(0));Return])) in
 main :: files
and bytecode_of_tmodule genv Kast.{mod_name;decls} = 
  let bc_decls = bytecode_of_decls mod_name decls in
  {mod_name;bc_decls;init=Ast2kast.(genv.init)}
and bytecode_of_decls mod_name ds = 
  mapcat (bytecode_of_decl mod_name) ds
and bytecode_of_decl mod_name = function 
| Kast.DefFun (name,arity,e) -> let bc_e = bytecode_of_exp e in
                                let full_name = mod_name ^ "." ^ name in
                                (* if full_name = "Main.main" then failwith "nom de fonction reservée" else *)
                                let nb_local bc = List.fold_left (function n -> function Pop(Local i) -> max (i+1) n | _ -> n) 0 bc in
                                [Function (full_name,nb_local bc_e)] @ bc_e @ [Return]
and bytecode_of_exp = function
| Kast.Constant c -> bytecode_of_constant c
| Kast.Variable v -> bytecode_of_variable v
| Kast.If(e1,e2,e3) -> let bc_e1 = bytecode_of_exp e1 
                       and bc_e2 = bytecode_of_exp e2
                       and bc_e3 = bytecode_of_exp e3 in
                       let lbl_if_false = gensym "IfFalse"
                       and lbl_end = gensym "IfEnd" in
                       bc_e1 @ [Op Not; IfGoto lbl_if_false] @
                       bc_e2 @ [Goto lbl_end; Label lbl_if_false] @
                       bc_e3 @ [Label lbl_end]
| Kast.While(e1,e2) -> let bc_e1 = bytecode_of_exp e1 
                       and bc_e2 = bytecode_of_exp e2 in
                       let lbl_begin = gensym "WhileBegin"
                       and lbl_end = gensym "WhileEnd" in
                       [Label lbl_begin] @ bc_e1 @ [Op Not; IfGoto lbl_end] @
                        bc_e2 @ [Goto lbl_begin; Label lbl_end]
| Kast.Let(n,e1,e2) -> let bc_e1 = bytecode_of_exp e1
                       and bc_e2 = bytecode_of_exp e2 in
                       bc_e1 @ [Pop(Local n)] @ bc_e2
| Kast.Seq(e1,e2) -> let bc_e1 = bytecode_of_exp e1 
                     and bc_e2 = bytecode_of_exp e2 in
                     bc_e1 @ [Pop Anywhere] @ bc_e2
| Kast.App(f,args) -> let arity = List.length args in
                      mapcat bytecode_of_exp args @ 
                      (match f with
                       | Kast.GFun (name) -> [Call(name,arity)]
                       | _ -> assert false)
| Kast.BinOp(op,e1,e2) -> let bc_e1 = bytecode_of_exp e1 
                          and bc_e2 = bytecode_of_exp e2 in
                          bc_e1 @ bc_e2 @ bytecode_of_binop op
| Kast.UnOp(op,e1) -> let bc_e1 = bytecode_of_exp e1  in
                          bc_e1 @ bytecode_of_unop op
| Kast.SetGlobal (e1,i) -> let bc_e1 = bytecode_of_exp e1 in
                           bc_e1 @ [Pop (Static(i))] @ [Push (Static(i));Pop (Temp(7))]
| Kast.ReadGlobal (i) -> [Push (Static(i))]
| Kast.GFun (name) -> [Call (name,0)] (* !!!!! variables globales *)
and bytecode_of_constant = function
| Kast.Unit -> [Push (Constant 0)]
| Kast.Int n -> [Push (Constant n)]
| Kast.Bool b -> if not b then [Push(Constant 0)] else [Push(Constant 0);Op(Not)]
| Kast.String(s) -> let n = String.length s in
                   [Push (Constant(n));
                    Call ("String.new",1)] @ 
                     (let rec aux acc k =
                        if k = n then acc
                        else aux (acc @ [Push (Constant (Char.code s.[k]));
                                         Call ("String.appendChar", 2)]) (k+1) in
                      aux [] 0)
and bytecode_of_variable = function
| Kast.Argument (n) -> [ Push(Argument n) ]
| Kast.Local (n) -> [ Push(Local n) ]

and bytecode_of_binop = function
| Ast.Add -> [Op Add]
| Ast.Minus -> [Op Sub]
| Ast.Mult -> [Op Mult]
| Ast.Eq -> [Op Eq] 
| Ast.Gt -> [Op Gt]
| Ast.Lt -> [Op Lt]
| Ast.Le -> [Op Gt;Op Not]
| Ast.Ge -> [Op Lt;Op Not]
| Ast.And | Ast.Land -> [Op And]
| Ast.Or | Ast.Lor -> [Op Or]

and bytecode_of_unop = function
| Ast.Not -> [Op Not]
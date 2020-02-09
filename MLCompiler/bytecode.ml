open Utils
(* open Ast *)

type code = Push of segment
        | Pop of segment
        | Label of label
        | IfGoto of label
        | Goto of label
        | Return
        | Function of fun_name * int (* nb locales *)
        | Call of fun_name * int (* arité *)
        | Prim of string
and segment = Argument of int | Constant of int | Local of int | That of int | Temp of int | Pointer of int
and label = string
and fun_name = string

(*
let gensym = 
let c = ref 0 in 
(fun s -> incr c; s ^ string_of_int !c) 
*)
let prim_lookup = function
| "+" -> "add"
| "-" -> "sub" (* manque - unaire *)
| "=" -> "eq"
| ">" -> "gt"
| "<" -> "lt"
| "&&" -> "and"
| "||" -> "or"  
| "not" -> "not" 
| _ -> assert false

module Env = struct
  module SMap = Map.Make(String)
  type ('a,'b) t = {smap : 'a SMap.t ; local : int; mod_name : string; prims : 'b SMap.t ; constructors : int SMap.t }

  let prims_env () = 
    SMap.(empty 
      |> add "print_int" [Call ("Output.printInt",1)]
      |> add "print_newline" [Pop (Temp 0); Call ("Output.println",0)]
      |> add "print_string" [Call ("Output.printString",1)]
      |> add "Array.length"  [Pop (Pointer 1); Push (Temp 0); Push (That 0)]
    )

  let create ~mod_name () =
    {smap=SMap.empty; local=0 ; mod_name=mod_name; prims=prims_env () ; constructors = SMap.empty }

  let extends env args =
    let r = ref env.smap in
    List.iteri (fun i x -> r := SMap.add x (`Local (env.local + i)) !r) args;
    {env with smap=(!r)}

  let extends_args env args =
    let r = ref env.smap in
    List.iteri (fun i x -> r := SMap.add x (`Argument (env.local + i)) !r) args;
    {env with smap=(!r)}

  let find env x =
    SMap.find_opt x env.smap

  let find_primitive env x =
    SMap.find_opt x env.prims

  let mod_name env = env.mod_name

  let local env = env.local
  let local_next env = {env with local=env.local+1}

  let extends_constructors env cstrs =
    let r = ref env.constructors in
    List.iteri (fun i x -> r := (SMap.add x i !r)) cstrs;
    {env with constructors=(!r)}
  
  let find_constructor env c =
    SMap.find_opt c env.constructors
end




let prim_lookup = function
| "+" -> Prim "add"
| "-" -> Prim "sub"
| "=" -> Prim "eq"
| "<" -> Prim "lt"
| "and" -> Prim "and"
| "or" -> Prim "or"
| _ -> assert false

let gensym = let c = ref 0 in (fun s -> incr c; s ^ string_of_int !c)
let next_label = let c = ref 0 in (fun s -> incr c; s ^ string_of_int !c)
let mapcat f l = List.concat (List.map f l)

let nb_local by = List.fold_left (fun n b -> 
                                     (match b with 
                                      | Pop (Local k) -> max n (k+1)
                                      | _ -> n)) 0 by
(* préfixe la chaîne f (nom de fonction) du nom du module courrant (notation qualifiée) si et seulement si 
   f n'est pas déjà de la forme M.g *)
let f_with_module env f =
  match String.index_opt f '.' with None -> Env.mod_name env ^ "." ^ f | Some _ -> f 

let rec vm_prog mod_name p = 
  let env = Env.create ~mod_name:mod_name () in 
  let rec aux acc env = function
| [] -> acc 
| h::t-> let env',by = vm_decl env h in
         aux (acc @ by) env' t in aux [] env p 
and vm_constant env = function
| Ast.Unit -> [Pop (Temp(0))]
| Ast.Int(n) -> [Push (Constant n)]
| Ast.Bool(b) -> if b then [Push (Constant 0); Prim "not"] else [Push (Constant 0)]
| Ast.String(s) -> let n = String.length s in
                 [Push (Constant(n))] @
                 [Call ("String.new",1)] @
                 (let rec aux acc k = if k = n then acc
                  else aux (acc @ [Push (Constant (Char.code s.[k])); Call ("String.appendChar", 2)]) (k+1) in aux [] 0)
| Ast.Constructor (s) -> match Env.find_constructor env s with None -> failwith "constructeur inconnu" | Some n -> [Push (Constant n)]
and vm_decl env = function
| (Ast.Decl (f,args,e,_)) | (Ast.RecDecl (f,args,e,_)) -> 
  let env' = Env.extends_args env args in
  let body = vm_expr env' e in
  let n = nb_local body in
  let mf = f_with_module env f in  
  (env, [Function (mf,n)] @ body @ [Return])
| Type (t,Sum ctrs,_) -> let env' = Env.extends_constructors env ctrs in (env',[])
and vm_expr env = function
| Ast.Constant (c,_) -> vm_constant env c
| Ast.BinOp(op,e1,e2,_) -> (vm_expr env e1) @ (vm_expr env e2) @ [prim_lookup op]
| Ast.UnOp(op,e,_) -> (vm_expr env e) @ [prim_lookup op]
| Ast.App(f,l,_) -> (mapcat (vm_expr env) l) @ 
                 (match f with
                  | Ident (name,_) -> (match Env.find_primitive env name with
                                      | Some b -> b
                                      | None -> [(Call ((f_with_module env name),(List.length l)))])
                  | _ -> failwith "not yet implemented")
(* prevoir LET IN (push local ?) *)
| Ast.Let(x,e1,e2,_) -> (* prévoir cas variable '_' ramasse tout, sans empiler *)
                    (vm_expr (Env.local_next env) e1) @ [(Pop (Local (Env.local env)))] @ 
                    let env' = Env.extends env [x] in
                    (vm_expr (Env.local_next env') e2)
| Ast.Ident(x,_) -> (match Env.find env x with 
                 | None -> (* [Push (PushLocal (Env.local env - 1))] *) failwith "globales pas encore implémentées"
                 | Some (`Local i) -> [Push (Local i)]
                 | Some (`Argument i) -> [Push (Argument i)])
| Ast.Seq (l,_) -> let l',last = match List.rev l with 
                             | [] -> assert false
                             | h::t -> (List.rev t,h) in (mapcat (fun x -> (vm_expr env x) @ [Pop (Temp 0)]) l') @ (vm_expr env last)
| Ast.If (e1,e2,e3,_) -> let k = next_label "" in
                     let true_label = ("IF_TRUE" ^ k)
                     and false_label = ("IF_FALSE" ^ k)
                     and end_label = ("IF_END" ^ k) in
                    (vm_expr env e1) @ 
                    [IfGoto true_label; 
                     Goto false_label;
                     Label true_label] @
                     (vm_expr env e2) @
                     [Goto end_label] @
                     [Label false_label] @
                     (vm_expr env e3) @
                     [Label end_label]
| Ast.Match (e,cases,pos) -> let x = gensym "tmp" in 
                         let body = let rec transform = function 
                                    | [] -> assert false
                                    | Ast.Otherwise (e,pos)::_ -> e
                                    | Ast.Case (c,e,pos)::cases -> Ast.If(Ast.Constant(c,pos),e,(transform cases),pos) in 
                            transform cases in
                         Ast.Let(x,e,body,pos) |> (vm_expr env)
| Ast.While (e1,e2,_) -> let k = next_label "" in
                     let while_exp_label = ("WHILE_EXP" ^ k)
                     and while_end_label = ("WHILE_END0" ^ k) in
 [Label while_exp_label] @
 (vm_expr env e1) @
 [Prim "not"] @
 [IfGoto while_end_label] @
 (vm_expr env e2) @
 [Pop (Temp 0)] @
 [Goto while_exp_label] @
 [Label while_end_label]
| Ast.Ref(e,_) -> [Push (Constant 1); Call ("Array.new", 1)] @ (vm_expr env e) @ 
              [Pop (Temp 0);
               Pop (Pointer 1);
               Push (Temp 0);
               Pop (That 0);
               Push (That 0);
               Push (Pointer 1);]
(* push local 0
add
push constant 17*)
| Ast.Access(e,_) -> (vm_expr env e) @ [Pop (Pointer 1); Push (Temp 0); Push (That 0);]
| Ast.Assign(x,e,_) -> (vm_expr env x) @ (* [Push (PushConstant 0)] @ (vm_expr env e) @ add *)
                   (vm_expr env e) @
                   [Pop (Temp 0);
                    Pop (Pointer 1);
                    Push (Temp 0);
                    Pop (That 0);
                    Push (That 0);
                    Push (Pointer 1);
                    (* Pop (Temp 0) *)]
| Ast.Array_create (l,_) -> let size = List.length l in
                        let bloc = (Ast.Constant(Ast.Int size,Parseutils.default_position)) :: l in
                        let env' = (Env.local_next env) in
                        [Push (Constant (size+1));   (* + 1 pour la taille *)
                         Call ("Array.new", 1)] @
                        [(Pop (Local (Env.local env)))] @
                        (List.concat (List.mapi (fun i e -> (* Pop (Temp 0);*)
                          [Push (Constant i);Push (Local (Env.local env)); Prim "add"] @ 
                          (vm_expr env' e) @ 
                                  [Pop (Temp 0);
                                   Pop (Pointer 1);
                                   Push (Temp 0);
                                   Pop (That 0)]) bloc)) @ [Push (Local (Env.local env))]
| Ast.Array_get (e1,e2,_) -> (vm_expr env e1) @ (vm_expr env e2) @ [Prim "add"] @ [Push (Constant 1); Prim "add"] 
                                              @ [Pop (Pointer 1); Push (Temp 0); Push (That 0);]
| Ast.Array_assign (e1,e_offset,e3,_) -> 
                                  (vm_expr env e_offset)@(vm_expr env e1)@ [Prim "add"] @ 
                                  [Push (Constant 1); Prim "add"] @
                                  (vm_expr env e3) @
                                  [Pop (Temp 0);
                                   Pop (Pointer 1);
                                   Push (Temp 0);
                                   Pop (That 0)]
let string_of_segment = function
| Argument n -> sptf "argument %d" n
| Constant n -> sptf "constant %d" n
| Local n -> sptf "local %d" n
| That n -> sptf "that %d" n
| Temp n -> sptf "temp %d" n
| Pointer n -> sptf "pointer %d" n
let string_of_instr = function
| Push pt -> "push " ^ string_of_segment pt
| Pop pt -> "pop " ^ string_of_segment pt
| Label k -> sptf "label %s" k
| IfGoto k -> sptf "if-goto %s" k
| Goto k -> sptf "goto %s" k
| Return -> "return"
| Function (f,n) -> sptf "function %s %d" f n
| Call (f,n) -> sptf "call %s %d" f n
| Prim s -> sptf "%s" s

let string_of_code b =
  String.concat "\n" (List.map string_of_instr b)

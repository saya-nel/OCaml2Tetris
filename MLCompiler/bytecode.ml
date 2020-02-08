open Utils
open Ast

type code = Push of push_type
        | Pop of pop_type
        | Label of label
        | IfGoto of label
        | Goto of label
        | Return
        | Function of fun_name * int (* nb locales *)
        | Call of fun_name * int (* arité *)
        | Prim of string
and push_type = PushArgument of int | PushConstant of int | PushLocal of int
and pop_type = PopLocal of int | PopTemp of int
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
  type ('a,'b) t = {smap : 'a SMap.t ; local : int; mod_name : string; prims : 'b SMap.t }

  let prims_env () = 
    SMap.(empty 
      |> add "print_int" [Call ("Output.printInt",1)]
      |> add "print_newline" [Call ("Output.println",0)]
      |> add "output_string" [Call ("Output.printString",1)]
    )

  let create ?(mod_name="Main") () =
    {smap=SMap.empty; local=0 ; mod_name=mod_name; prims=prims_env ()}

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
end




let prim_lookup = function
| "+" -> Prim "add"
| "-" -> Prim "sub"
| "=" -> Prim "eq"
| "<" -> Prim "lt"
| _ -> assert false

let gensym = let c = ref 0 in (fun s -> incr c; s ^ string_of_int !c)
let next_label = let c = ref 0 in (fun s -> incr c; s ^ string_of_int !c)
let mapcat f l = List.concat (List.map f l)

let nb_local by = List.fold_left (fun n b -> 
                                     (match b with 
                                      | Pop (PopLocal k) -> max n (k+1)
                                      | _ -> n)) 0 by

let rec vm_prog ?(env=Env.create()) p = mapcat (vm_decl env) p
and vm_constant env = function
| Unit -> [Pop (PopTemp(0))]
| Int(n) -> [Push (PushConstant n)]
| Bool(b) -> let n = if b then 1 else 0 in [Push (PushConstant n)]
| String(s) -> let n = String.length s in
                 [Push (PushConstant(n))] @
                 [Call ("String.new",1)] @
                 (let rec aux acc k = if k = n then acc
                  else aux (acc @ [Push (PushConstant (Char.code s.[k])); Call ("String.appendChar", 2)]) (k+1) in aux [] 0)
and vm_decl env = function
| (Decl (f,args,e,_)) | (RecDecl (f,args,e,_)) -> 
  let env' = Env.extends_args env args in
  let body = vm_expr env' e in
  let n = nb_local body in
  [Function ((Env.mod_name env ^ "." ^ f),n)] @ body @ [Return]
and vm_expr env = function
| Constant (c,_) -> vm_constant env c
| BinOp(op,e1,e2,_) -> (vm_expr env e1) @ (vm_expr env e2) @ [prim_lookup op]
| UnOp(op,e,_) -> (vm_expr env e) @ [prim_lookup op]
| App(f,l,_) -> (mapcat (vm_expr env) l) @ 
                 (match f with
                  | Ident (name,_) -> (match Env.find_primitive env name with
                                      | Some b -> b
                                      | None -> [(Call ((Env.mod_name env ^ "." ^ name),(List.length l)))])
                  | _ -> failwith "not yet implemented")
(* prevoir LET IN (push local ?) *)
| Let(x,e1,e2,_) -> (* prévoir cas variable '_' ramasse tout, sans empiler *)
                    (vm_expr (Env.local_next env) e1) @ [(Pop (PopLocal (Env.local env)))] @ 
                    let env' = Env.extends env [x] in
                    (vm_expr (Env.local_next env') e2)
| Ident(x,_) -> (match Env.find env x with 
                 | None -> (* [Push (PushLocal (Env.local env - 1))] *) failwith "globales pas encore implémentées"
                 | Some (`Local i) -> [Push (PushLocal i)]
                 | Some (`Argument i) -> [Push (PushArgument i)])
| Seq (l,_) -> mapcat (fun x -> (vm_expr env x) @ [Pop (PopTemp 0)]) l (* à vérifier *)
| If (e1,e2,e3,_) -> let k = next_label "" in
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
| Match (e,cases,pos) -> let x = gensym "tmp" in 
                         let body = let rec transform = function 
                                    | Otherwise (e,pos)::_ -> e
                                    | Case (c,e,pos)::cases -> If(Constant(c,pos),e,(transform cases),pos) in 
                            transform cases in
                         Let(x,e,body,pos) |> (vm_expr env)
| While (e1,e2,_) -> let k = next_label "" in
                     let while_exp_label = ("WHILE_EXP" ^ k)
                     and while_end_label = ("WHILE_END0" ^ k) in
 [Label while_exp_label] @
 (vm_expr env e1) @
 [IfGoto while_end_label] @
 (vm_expr env e2) @
 [Pop (PopTemp 0)] @
 [Goto while_exp_label] @
 [Label while_end_label]

let string_of_push_type = function
| PushArgument n -> sptf "argument %d" n
| PushConstant n -> sptf "constant %d" n
| PushLocal n -> sptf "local %d" n

let string_of_pop_type = function
| PopLocal n -> sptf "local %d" n
| PopTemp n -> sptf "temp %d" n

let string_of_instr = function
| Push pt -> "push " ^ string_of_push_type pt
| Pop pt -> "pop " ^ string_of_pop_type pt
| Label k -> sptf "label %s" k
| IfGoto k -> sptf "if-goto %s" k
| Goto k -> sptf "goto %s" k
| Return -> "return"
| Function (f,n) -> sptf "function %s %d" f n
| Call (f,n) -> sptf "call %s %d" f n
| Prim s -> sptf "%s" s

let string_of_code b =
  String.concat "\n" (List.map string_of_instr b)

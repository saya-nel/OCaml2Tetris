open Utils

type bc = vm_instr list            
and vm_instr =
  | Push of (vm_segment * int)
  | Pop of (vm_segment * int)
  | Prim of vm_prim
  | Label of vm_label
  | Ifgoto of vm_label
  | Goto of vm_label
  | Function of (vm_label * int) (* nb locales *)
  | Return
  | Call of (vm_label * int)     

and vm_segment =
  |  Argument
  | Constant
  | Local
  | That
  | Temp
  | Pointer
  | Static
  
and vm_prim =
  Or
| And
| Not
| Add
| Sub
| Lt
| Gt
| Eq
| Peek

and vm_label = string



             
module Gensym = struct

  let next ?(prefix="label") () =
    let n = ref 0 in
    incr n;
    prtf "%s%d" prefix !n
    
end


(* type env = { globals = (string * bc) list; } *)

let rec bc_exp closure_defs n penv genv e =  
  let rec compile arity k = function
    | Kast.Val n -> [Push(Constant,n)]
    | Kast.Var Kast.{index=m} -> if arity <= 0 && (m = k - 1)
                                 then [Push(Argument,0)]
                                 else [Push(Static,m)]
    | Kast.Lam e ->
       if arity > 0 
       then [Pop (Static,k)] @ compile (arity-1) (k+1) e
       else let name = Printf.sprintf "Main.closure%d" k in begin 
                closure_defs := !closure_defs @ [(k,name,([Function (name,1)]
                                @ (compile (arity-1) (k+1) e) @ [Return]))];
                [Push (Constant,k); Call ("ML.make_closure",1)]
            end
    | Kast.App(e1,e2) -> let ce2 = compile (arity) k e2 in
                         let ce1 = compile (arity+1) k e1 in
                         ce2 @ ce1 @ [Call("Main.apply",1)]
    | Kast.If (e1,e2,e3) -> let bc1 = compile arity k e1 in
                            let bc2 = compile arity k e2 in
                            let bc3 = compile arity k e3 in
                            let kthen = Gensym.next ~prefix:"THEN" () in
                            let kfi = Gensym.next ~prefix:"FI" () in
                            bc1
                            @ [Prim Not; Ifgoto kthen]
                            @ bc2 @ [Goto kfi;Label kthen]
                            @ bc3 @ [Label kfi]
    | Kast.Global name -> (match List.assoc_opt name penv with
                           | Some v -> v 
                           | None -> (match List.assoc_opt name genv with 
                       	              | Some e -> e
                       	              | None -> failwith "unbound")) in compile 0 0 e
and bc_decl closure_defs penv genv (d,e) = 
  let bce = bc_exp closure_defs 0 penv genv e in
  let name = "Main." ^ d in
  [Function (name,0)] @ bce @ [Return]
and bc_of_kast penv p =
  let closure_defs = ref [] in
  let rec aux acc genv = function 
    | [] -> acc
    | h::t -> aux (bc_decl closure_defs penv genv h) genv t
  in [Function ("Main.apply",1)] @
  [Push (Argument,0);Push (Constant,32767);Prim Not;Prim And;Ifgoto "fin"] @
     (List.concat (List.map (fun (k,name,_) ->  
      let lbl = Printf.sprintf "tmp%d" k in
     [Push(Argument,0); Push (Constant,k); Prim Eq;Prim Not;Ifgoto lbl;Call(name,1);Goto "fin"; Label lbl; ]) !closure_defs)) @
     [ Push (Argument,0) ] @
     [Label "fin"; Return]
      (* ((List.fold_left (fun acc (k,name,_) -> Ast.(Lam("zz",If(App(App(Var("eq"),Val(k)),Var("zz")),Var(name),acc)))) Ast.(Val 0) !closure_defs) 
       |> Kast.kast |> bc_of_kast [("eq",[Prim Eq])]) *)
  @ (List.concat (List.map (fun (_,_,c) -> c) !closure_defs)) @ aux [] [] p


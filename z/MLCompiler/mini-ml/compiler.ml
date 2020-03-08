(* type name = string

type value = Im of int | Bloc of bloc 
and bloc = { tag : tag_bloc;
           color : color_bloc;
           data : value list } 
and tag_bloc = String_tag | Closure_tag | Const of int
and color_bloc = int 
*)
open Utils

module Compile = struct
	open Bytecode
	let sptf = Printf.sprintf
	
	let rec string_of_bc p = 
    String.concat "\n" (List.map string_of_bc_instr p) 
	and string_of_bc_instr = function
	| Function (name,n) -> sptf "function %s %d" name n
	| Return -> "return"
	| Push (seg,pos) -> sptf "push %s %d" (string_of_vm_segment seg) pos
	| Pop  (seg,pos) -> sptf "pop %s %d" (string_of_vm_segment seg) pos
	| Prim prim -> (string_of_bc_prim prim)
	| Label lbl -> sptf "label %s" lbl
	| Ifgoto lbl -> sptf "if-goto %s" lbl
	| Goto lbl -> sptf "goto %s" lbl
	| Function (lbl,n) -> sptf "function %s %d" lbl n
	| Return -> "return"
	| Call (lbl,n) -> sptf "call %s %d" lbl n
	and string_of_vm_segment = function
	| Argument -> "argument"
	| Constant -> "constant"
	| Local -> "local"
	| That -> "that"
	| Temp -> "temp"
	| Pointer -> "pointer"
	| Static -> "static"
	and string_of_bc_prim = function
	| Or -> "or"
	| And -> "and" 
	| Not -> "not"
	| Add -> "add"
	| Sub -> "sub"
	| Lt -> "lt"
	| Gt -> "gt"
	| Eq -> "eq"
	(* | Peek -> string_of_vm_bc vm_peek*)
end


module Custom = struct
   open Bytecode
 open Compile
let x8000 = 32768
let x7fff = 32767
let x4000 = 16384
(** construire un pointeur vers un bloc *)
let make_bloc =
	[Push (Constant, x8000);
	 Prim Or]

(** construire un immédiat bool / entier positif *)
let make_imm =
	[ Push (Constant, x7fff);
	  Prim And]

(** construire un immédiat int négatif (-n) à partir de n *)
let make_neg =
	[Pop (Temp, 0);
     Push (Constant, 0);
	 Push (Temp,0);
	 Prim Sub;
	 Push (Constant, x4000);
	 Prim Or] @
	make_imm

let ml_add =
  [Prim Add] @ make_imm

let ml_sub = 
	make_neg @ [Prim Add]

let ml_not =
  [Prim Not] @ make_imm

let vm_peek =
	[Push (Static, 0);
	Prim Add;
	Pop (Pointer,1);
	Push (That,0)]

let ml_peek = (* attend une adresse, ie bit 15 à 1 *)
	make_imm @ vm_peek


let custom_env = [("sub",[Prim Sub]);
                  ("add",[Prim Add]);
                  ("eq",[Prim Eq]);
                  ("lt",[Prim Lt]);
                  ("gt",[Prim Gt]);
                  ("false",[Push (Constant,0)]);
                  ("true",[Push (Constant,0);Prim Not]);
                  ("print_int",[Call ("Output.printInt",1)])]
let compile ast = 
Kast.kast ast
|> Bytecode.bc_of_kast custom_env |> Compile.string_of_bc
end


(* 

Kast.kast_of_ast Kast.empty_lenv Ast.(Let (("x",Val 5),
      App(Lam ("y",App(App(Var "+",Var "x"),Var "x")),Var "x"))) 

      |> Bc.bc_of_kast 0 [("+",Bc.[Prim Add])]|> Compile.string_of_bc


     Custom.compile  Ast.(Let (("x",Val 5),
     App(Lam ("y",App(App(Var "+",Var "x"),Var "x")),Var "x"))) 
*)
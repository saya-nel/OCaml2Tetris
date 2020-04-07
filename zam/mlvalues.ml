type t = int

(* ************************************* *)
(* blk = [|TAG;COLOR;F1;F2;...|]         *)
(* size blk = Array.length blk - 2       *)
(* ************************************* *)

(* !!! la notation 0x... de mini-ml est erroné !!! *)

(* transforme un entier en mlvalue *)
let val_long (n : t) : t = n lor 0x8000 (* pack *)
(* transforme un mlvalue en entier *)
let long_val (n : t) : t = n land 0x7FFF (* unpack *)
(*  *)
let val_ptr (n : t) : t = n land 0x7FFF
let ptr_val (n : t) : t = n

let blk_size (b : t) = Array.length b - 2

let is_imm (n : t) : bool = n < 0

let size b = Array.length b - 2 

let unit = 0

let new_block tag sz = (* (tag : t) (sz : t) : t = *)
  let a = Array.make ((long_val sz) + 2) 0 in
  a.(0) <- long_val tag;
  (* a.(1) <- tag color; *)
  val_ptr a

let get_field b i = (* (b : t) (i : int) (x : t) : unit = *)
  (ptr_val b).(i+2)

let set_field b i x = (* (b : t) (i : int) (x : t) : unit = *)
  (ptr_val b).(i+2) <- x

let addr_closure c = long_val (get_field c 1)
let env_closure c = get_field c 2

(*
let f n = 
	assert ((long_val (val_long n)) = n)
let _ = f 42; f (- 42)

~~~~> erreur
*)
type t = int

(* ************************************* *)
(* blk = [|TAG;COLOR;F1;F2;...|]         *)
(* size blk = Array.length blk - 2       *)
(* ************************************* *)

(* !!! la notation 0x... de mini-ml est erroné !!! *)

(* transforme un entier en mlvalue *)
let val_long (n : t) : t = n lor  0x8000 (* pack *)
(* transforme un mlvalue en entier *)
let long_val (n : t) : t = n land 0x7FFF;; (* unpack *)
(*  *)
let val_ptr (n : t) : t = n land 0x7FFF
let ptr_val (n : t) : t = n

let blk_size (b : t) = Array.length b - 2

let is_imm (n : t) : bool = (n land 0x8000) = 0x8000

let size b = Array.length b - 2 

let unit = 0

let new_block tag sz = (* (tag : int) (sz : int) : t = *)
  let a = Array.make (sz+2) 0 in
  a.(0) <- tag;
  (* a.(1) <- tag color; *)
  val_ptr a

let get_field b i = (* (b : t) (i : int) (x : t) : unit = *)
  (ptr_val b).(i+2)

let set_field b i x = (* (b : t) (i : int) (x : t) : unit = *)
  (ptr_val b).(i+2) <- x

let addr_closure c = long_val (get_field c 1)
let env_closure c = get_field c 2


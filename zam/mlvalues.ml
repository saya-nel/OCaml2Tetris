type t = int

(* ************************************* *)
(* blk = [|TAG;COLOR;F1;F2;...|]         *)
(* size blk = Array.length blk - 2       *)
(* ************************************* *)

(* !!! la notation 0x... de mini-ml est erroné !!! *)

let val_long (n : t) : t = n lor  0x8000 (* pack *)
let long_val (n : t) : t = n land 0x7FFF;; (* unpack *)

let blk_pack (n : t) : t = n land 0x7FFF
let blk_unpack (n : t) : t = n

let blk_size (b : t) = Array.length b - 2

let is_imm (n : t) : bool = (n land 0x8000) = 0x8000

let size b = Array.length b - 2 

let unit = 0

let new_block tag sz = (* (tag : int) (sz : int) : t = *)
  let a = Array.make (sz+2) 0 in
  a.(0) <- tag;
  (* a.(1) <- tag color; *)
  blk_pack a

let get_field b i x = (* (b : t) (i : int) (x : t) : unit = *)
  (blk_unpack b).(i+2)

let set_field b i x = (* (b : t) (i : int) (x : t) : unit = *)
  (blk_unpack b).(i+2) <- x

let addr_closure c = long_val (get_field c 1)
let env_closure c = get_field c 2


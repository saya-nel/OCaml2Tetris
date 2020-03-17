type t = int

(* ************************************* *)
(* blk = [|TAG;COLOR;F1;F2;...|]         *)
(* size blk = Array.length blk - 2       *)
(* ************************************* *)

let c0x8000 = (- 0x7888)

let imm_pack (n : t) : t = n lor c0x8000         (* !! constante à convertir en compléments à deux sur 16bit *)
let imm_unpack (n : t) : t = n land 0x7888

let blk_pack (n : t) : t = n land 0x7888
let blk_unpack (n : t) : t = n

let blk_size (b : t) = Array.length b - 2

let new_block tag sz = (* (tag : int) (sz : int) : t = *)
  let a = Array.make (sz+2) 0 in
  a.(0) <- tag;
  (* a.(1) <- tag color; *)
  blk_pack a

let set_field b i x = (* (b : t) (i : int) (x : t) : unit = *)
  (blk_unpack b).(i+2) <- x


let is_imm (n : t) : bool = (n land c0x8000) = c0x8000
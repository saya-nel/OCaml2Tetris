type value = int 
type long = int
type ptr = int

(* ************************************* *)
(* blk = [|TAG;COLOR;F1;F2;...|]         *)
(* size blk = Array.length blk - 2       *)
(* ************************************* *)

(* transforme un entier en mlvalue *)
let val_long (n : long) : value = n

(* transforme un mlvalue en entier *)
let long_val (v : value) : long = v

(* transforme un pointeur en mlvalue *)
let val_ptr (p : ptr) : value =
  (- (p + 16384))

(* transforme un mlvalue en pointeur *)
let ptr_val (v : value) : ptr =
  (- (v + 16384))

let is_ptr (v : value) : bool = 
  v < (- 16384)

let size (b : ptr) = 
  (* a priori, problème si le bloc a taille >= 128 *)
  Alloc.heap.(b) / 256
(* (# b).(0) / 256 *)

let tag (b : ptr) = 
  (* a priori, problème si le bloc a taille >= 128 *)
  Alloc.heap.(b) land 255
(* (# b).(0) land 255 *)

let unit = 0

let make_header (tag : long) (sz : long) =
  val_long (tag + 256 * sz)

let make_block (tag : long) (sz : long) =
  let a = Alloc.alloc (sz + 1) in
  Alloc.heap.(a) <- val_long (tag + 256 * sz);
  val_ptr a
(* let a = Array.make (sz + 1) 0 in
   a.(0) <- val_long (tag + 256 * sz);
   val_ptr (# a) *)

let get_field (v : value) (i : int) =
  Alloc.heap.((ptr_val v) + i + 1)
(* (#(ptr_val v)).(i+1) *)

let set_field (v : value) (i : int) (x : value) = 
  Alloc.heap.((ptr_val v) + i + 1) <- x
(* (#(ptr_val v)).(i+1) <- x *)

let get_bytes (v : value) (i : int) = 
  (* ici, on place un char par mot *)
  Alloc.heap.((ptr_val v) + i + 1)
(* (#(ptr_val v)).(i+1) *)

let set_bytes (v : value) (i : int) (x : value) =  (* cf get_bytes. *)
  Alloc.heap.((ptr_val v) + i + 1) <- x
(* (#(ptr_val v)).(i+1) <- x *)

let closure_tag = 247
let env_tag = 2 (* quel est le bon numéro ??? *)
let infix_tag = 249

let make_closure pc size =
  let res = make_block closure_tag size in
  set_field res 0 pc;
  res

(* 
  let make_closure pc env = 
  val_ptr (# [|val_long closure_tag;val_long pc;env|]) 

   let make_env sz =
   make_block env_tag sz *)

let addr_closure (c : value) = get_field c 0
let env_closure (c : value) = val_long ((long_val c) + 2)


let val_codeptr o = val_long o (* ??? *)

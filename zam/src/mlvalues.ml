type value = int 
type long = int
type ptr = int
(* ************************************* *)
(* blk = [|TAG;COLOR;F1;F2;...|]         *)
(* size blk = Array.length blk - 2       *)
(* ************************************* *)

(* transforme un entier en mlvalue *)
let val_long (n : long) : value = 
  (* assert (n >= 0); *)
  (- n)
    
(* transforme un mlvalue en entier *)
let long_val (n : value) : long = 
  (* assert (n >= 0); *)
  (- n)

(* transforme un pointeur en mlvalue *)
let val_ptr (n : ptr) : value =
  #n + 16384

(* transforme un mlvalue en pointeur *)
let ptr_val (n : value) : ptr =
  #n - 16384

let is_ptr (n : value) : bool = 
  n < (- 16384)

let size (b : ptr) = 
  Array.length (# b) - 2 

let tag (b : ptr) = 
  (# b).(0)

let unit = 0

let make_block (tag : long) (sz : long) =
  (* assert (tag >= 0 && sz >= 0); *)
  let a = Array.make (sz + 2) 0 in
  a.(0) <- val_long tag;
  a.(1) <- val_long 0; (* color *)
  val_ptr (# a)

let get_field (v : value) (i : int) =
  (#(ptr_val v)).(i+2)

let set_field (v : value) (i : int) (x : value) = 
  (#(ptr_val v)).(i+2) <- x

let get_bytes (v : value) (i : int) = 
  (* à revoir : caser 2 chars par mlvalues (2 * 1 octet) ? *)
  (#(ptr_val v)).(i+2)

let set_bytes (v : value) (i : int) (x : value) =  (* à revoir. cf get_bytes. *)
  (#(ptr_val v)).(i+2) <- x


let closure_tag = 1 (* ??? *)
let env_tag = 2
let make_closure pc env = 
  val_ptr (# [|val_long closure_tag;val_long pc;env|])   (* ? *)

let make_env sz =
  make_block env_tag sz

let addr_closure (c : value) = get_field c 0
let env_closure (c : value) = get_field c 1

(* ********************************************** *)

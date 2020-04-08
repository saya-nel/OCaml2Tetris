type value = int 
type long = int
type ptr = int
(* ************************************* *)
(* blk = [|TAG;COLOR;F1;F2;...|]         *)
(* size blk = Array.length blk - 2       *)
(* ************************************* *)

(* transforme un entier en mlvalue *)
let val_long (n : long) : value = 
  assert (n >= 0);
  #n
    
(* transforme un mlvalue en entier *)
let long_val (n : value) : long = 
  assert (n >= 0);
  #n

(* transforme un pointeur en mlvalue *)
let val_ptr (n : ptr) : value =
  assert (n > 0); (* addresse 0 réservée *)
  (- n)

(* transforme un mlvalue en pointeur *)
let ptr_val (n : value) : ptr =
  assert (n < 0);
  (- n)

let is_ptr (n : value) : bool = 
  n < 0

let size (b : ptr) = Array.length (# b) - 2 

let unit = 0

let new_block (tag : long) (sz : long) =
  assert (tag >= 0 && sz >= 0);
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




let addr_closure (c : value) = long_val (get_field c 1)
let env_closure (c : value) = get_field c 2

let _ = 
  assert (long_val (val_long 42) = 42);
  assert (ptr_val (val_ptr 42) = 42);
  assert (not (is_ptr (val_long 42)));
  assert (is_ptr (val_ptr 42));
  assert (not (is_ptr (val_long 0)))
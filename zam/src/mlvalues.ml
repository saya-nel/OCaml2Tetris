type t = int

(* ************************************* *)
(* blk = [|TAG;COLOR;F1;F2;...|]         *)
(* size blk = Array.length blk - 2       *)
(* ************************************* *)

(* !!! la notation 0x... de mini-ml est erroné !!! *)

(* transforme un entier en mlvalue *)
let val_long (n : t) : t = 
  assert (n >= 0);
  n
    
(* transforme un mlvalue en entier *)
let long_val (n : t) : t = 
  assert (n >= 0);
  n

(* transforme un pointeur en mlvalue *)
let val_ptr (n : t) : t =
  assert (n > 0); (* addresse 0 réservée *)
  (- n)

(* transforme un mlvalue en pointeur *)
let ptr_val (n : t) : t =
  assert (n < 0);
  (- n)

let is_ptr (n : t) : bool = 
  n < 0

let size b = Array.length b - 2 

let unit = 0

let new_block (tag : int) (sz : int) = (* (tag : t) (sz : t) : t = *)
  assert (tag >= 0 && sz >= 0);
  let a = Array.make (sz + 2) 0 in
  a.(0) <- tag;
  a.(1) <- 0; (* color *)
  val_ptr a

let get_field b i = (* (b : t) (i : int) (x : t) : unit = *)
  (ptr_val b).(i+2)

let set_field b i x = (* (b : t) (i : int) (x : t) : unit = *)
  (ptr_val b).(i+2) <- x

let addr_closure c = long_val (get_field c 1)
let env_closure c = get_field c 2

let _ = 
  assert (long_val (val_long 42) = 42);
  assert (ptr_val (val_ptr 42) = 42);
  assert (not (is_ptr (val_long 42)));
  assert (is_ptr (val_ptr 42));
  assert (not (is_ptr (val_long 0)))
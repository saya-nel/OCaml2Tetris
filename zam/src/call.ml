let not_available () = failwith "The external function is not available"

(* ************************ CALL1 ***************************** *)

let caml_print_int = 0

let caml_print_int_code v = 
	Pervasives.print_int (Mlvalues.long_val v); 
	Block.unit

let caml_print_newline = 1
let caml_print_newline_code _ = 
	Pervasives.print_newline (); 
	Block.unit

(* [array_leng a] renvoie la taille du tableau a *)
let caml_array_length = 2

let caml_array_length_code a =
  Mlvalues.val_long (Block.size (Mlvalues.ptr_val a))

(* ************************ CALL2 ***************************** *)

(* [caml_make_vect z x] renvoie un bloc (tableau) de taille z *)
(* dont tout les champs sont initialisé à x *)
let caml_make_vect = 0

let caml_make_vect_code sz init =
  let n = Mlvalues.long_val sz in
  if sz < 0 then failwith "caml_make_vect" else
  let arr = Alloc.make_block 0 n in (* array : tag 0 *)
  for i = 0 to n - 1 do
    Block.set_field arr i init
  done;
  arr

(* [array_get a n] renvoie le n-ième élement du tableau a *)
let caml_array_get_addr = 1

let caml_array_get_addr_code a v =
    Block.get_field a (Mlvalues.long_val v)

(* ************************ CALL3 ***************************** *)

(* [array_get a n x] affecte x au n-ième élement du tableau a *)
let caml_array_set_addr = 0

let caml_array_set_addr_code a v x =
    Block.set_field a (Mlvalues.long_val v) x; 
    Block.unit

(* [array_sub a ofs len] ... *)
let caml_array_sub = 1

let caml_array_sub_code a v1 v2 =
	let ofs = Mlvalues.long_val v1 in
	let len = Mlvalues.long_val v2 in
	if ofs < 0 || len < 0 || ofs > Block.size (Mlvalues.ptr_val a) - len
    then failwith "caml_array_sub" else
    let arr = Alloc.make_block 0 len in (* array : tag 0 *)
    for i = ofs to ofs + len - 1 do
      Block.set_field arr i (Block.get_field a i)
    done;
    arr
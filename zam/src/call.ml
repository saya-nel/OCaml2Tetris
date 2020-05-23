let not_available () = failwith "The external function is not available"

(* ************************ CALL1 ***************************** *)

let n2t_print_int = 0

let n2t_print_int_code v = 
	Pervasives.print_int (Mlvalues.long_val v); 
	Block.unit

let n2t_print_newline = 1
let n2t_print_newline_code _ = 
	Pervasives.print_newline (); 
	Block.unit

let n2t_print_char = 2
let n2t_print_char_code v = 
  Pervasives.print_char (Obj.magic (Mlvalues.long_val v)); 
  Block.unit

let n2t_print_string = 3
let n2t_print_string_code v = 
  let p = Mlvalues.ptr_val v in
  let z = Block.size p in
  for i = 0 to z - 1 do

    Pervasives.print_char (Obj.magic (Mlvalues.long_val (Block.get (p+i+1))))
  done; 
  Block.unit

(* [array_leng a] renvoie la taille du tableau a *)
let n2t_array_length = 4

let n2t_array_length_code a =
  Mlvalues.val_long (Block.size (Mlvalues.ptr_val a))

let n2t_fresh_oo_id = 5 (* définition d'exception *)
let n2t_fresh_oo_id_code _ = Block.unit

(* ************************ CALL2 ***************************** *)

(* [n2t_make_vect z x] renvoie un bloc (tableau) de taille z *)
(* dont tout les champs sont initialisé à x *)
let n2t_make_vect = 0

let n2t_make_vect_code sz init =
  let n = Mlvalues.long_val sz in
  if n < 0 then failwith "n2t_make_vect" else
  let arr = Alloc.make_block 0 n in (* array : tag 0 *)
  for i = 0 to n - 1 do
    Block.set_field arr i init
  done;
  arr

(* [array_get a n] renvoie le n-ième élement du tableau a *)
let n2t_array_get_addr = 1

let n2t_array_get_addr_code a v =
    Block.get_field a (Mlvalues.long_val v)

(* ************************ CALL3 ***************************** *)

(* [array_get a n x] affecte x au n-ième élement du tableau a *)
let n2t_array_set_addr = 0

let n2t_array_set_addr_code a v x =
    Block.set_field a (Mlvalues.long_val v) x; 
    Block.unit

(* [array_sub a ofs len] ... *)
let n2t_array_sub = 1

let n2t_array_sub_code a v1 v2 =
	let ofs = Mlvalues.long_val v1 in
	let len = Mlvalues.long_val v2 in
	if ofs < 0 || len < 0 || ofs > Block.size (Mlvalues.ptr_val a) - len
    then failwith "n2t_array_sub" else
    let arr = Alloc.make_block 0 len in (* array : tag 0 *)
    for i = ofs to ofs + len - 1 do
      Block.set_field arr i (Block.get_field a i)
    done;
    arr
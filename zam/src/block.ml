
let size ptr = 
  (* a priori, problème si le bloc a taille >= 128 *)
  (Mlvalues.long_val (!Domain.from_space).(ptr)) / 256

let tag ptr = 
  (* a priori, problème si le bloc a taille >= 128 *)
  (Mlvalues.long_val (!Domain.from_space).(ptr)) land 255

let unit = Mlvalues.val_long 0 

let get_global i =
  Domain.global.(i)

let set_global i vx =
  Domain.global.(i) <- vx

let make_header tag sz =
  Mlvalues.val_long (tag + 256 * sz)

let get_field v i =
  (!Domain.from_space).((Mlvalues.ptr_val v) + i + 1)

let set_field v i vx = 
  (!Domain.from_space).((Mlvalues.ptr_val v) + i + 1) <- vx

let get_bytes v i = 
  (* ici, on place  un char par mot *)
  (!Domain.from_space).((Mlvalues.ptr_val v) + i + 1)

let set_bytes v i vx =  (* cf get_bytes. *)
  (!Domain.from_space).((Mlvalues.ptr_val v) + i + 1) <- vx

let closure_tag = 247
let env_tag = 250 (* quel est le bon numéro ??? *)
let infix_tag = 249
let fwd_ptr_tag = 248

let addr_closure cv = get_field cv 0
let env_closure cv = Mlvalues.val_ptr ((Mlvalues.ptr_val cv) + 2)

let val_codeptr o = Mlvalues.val_long o (* ??? *)

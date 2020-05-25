let bounds_ok i a =
  i >= 0 && i < Array.length a

let get ptr = 
  if ptr >= Domain.heap_start
  then let i = ptr - Domain.heap_start in 
       (if bounds_ok i !Domain.from_space 
        then (!Domain.from_space).(i) 
        else failwith "en dehors du tas")
  else
    if ptr >= Domain.global_start
    then let i = ptr - Domain.global_start in 
       (if bounds_ok i Domain.global 
        then Domain.global.(i) 
        else failwith "en dehors des globales")
  else (if bounds_ok ptr Domain.data 
        then Domain.data.(ptr)
        else failwith "en dehors du segment data")

let set ptr x = 
  if ptr >= Domain.heap_start
  then (let i = ptr - Domain.heap_start in 
        if bounds_ok i !Domain.from_space 
        then (!Domain.from_space).(i) <- x
        else failwith "en dehors du tas")
  else
    if ptr >= Domain.global_start
    then (let i = ptr - Domain.global_start in 
          if bounds_ok i Domain.global 
          then Domain.global.(i) <- x
          else failwith "en dehors des globales")
  else (if bounds_ok ptr Domain.data 
        then Domain.data.(ptr) <- x
        else failwith "en dehors du segment data")

let size ptr = 
  let hd = get ptr in 
  (Mlvalues.long_val hd) / 256

  (* a priori, problème si le bloc a taille >= 128 *)
let tag ptr = 
  (* a priori, problème si le bloc a taille >= 128 *)
  let hd = get ptr in 
  (Mlvalues.long_val hd) land 255

let unit = Mlvalues.val_long 0 

let get_global i =
  (Domain.global).(i)

let set_global i vx =
  (Domain.global).(i) <- vx

let make_header tag sz =
  Mlvalues.val_long (tag + 256 * sz)

let get_field v i =
  get ((Mlvalues.ptr_val v) + i + 1)

let set_field v i vx = 
  set ((Mlvalues.ptr_val v) + i + 1) vx

let get_bytes v i = (* ici, on place un char par mot *)
  get_field v i

let set_bytes v i vx =  (* cf get_bytes. *)
  set_field v i vx

let no_scan_tag = 251
let string_tag = 252
let closure_tag = 247
let infix_tag = 249
let fwd_ptr_tag = 248

let addr_closure cv = get_field cv 0
let env_closure cv = Mlvalues.val_ptr ((Mlvalues.ptr_val cv) + 2)

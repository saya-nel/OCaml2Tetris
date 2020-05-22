let get ptr = 
  if ptr >= Domain.heap_start
  then (!Domain.from_space).(ptr-Domain.heap_start)
  else if ptr >= Domain.global_start then (Domain.global).(ptr-Domain.global_start)
  else (Domain.data).(ptr)

let set ptr x = 
  if ptr >= Domain.heap_start 
  then (!Domain.from_space).(ptr-Domain.heap_start) <- x
  else if ptr >= Domain.global_start then (Domain.global).(ptr-Domain.global_start) <- x
  else (Domain.data).(ptr) <- x

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
let env_tag = 250 (* quel est le bon numéro ??? *)
let infix_tag = 249
let fwd_ptr_tag = 248

let addr_closure cv = get_field cv 0
let env_closure cv = Mlvalues.val_ptr ((Mlvalues.ptr_val cv) + 2)

let val_codeptr o = Mlvalues.val_long o (* ??? *)

let push_global v = 
  Domain.global.(!Domain.global_top) <- v;
  incr Domain.global_top

let make_global_long n =
  let p = !Domain.data_top in
  Domain.data.(!Domain.data_top) <- Mlvalues.val_long n;
  incr Domain.data_top;
  push_global (Mlvalues.val_long p)

let make_global_block tag sz =
  let p = !Domain.data_top in
  Domain.data.(p) <- Mlvalues.val_long (make_header tag sz);
  Domain.data_top := p + sz + 1;
  push_global (Mlvalues.val_ptr p)

let data_string s =
  let p = !Domain.data_top in
  let z = String.length s in
  make_global_block string_tag z;
  for i = 0 to z - 1 do
    Domain.data.(p+i+1) <- Mlvalues.val_long (Obj.magic (String.get s i))
  done

 
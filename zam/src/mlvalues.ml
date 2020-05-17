(* alloc définitions *)

let heap_size = 100

let from_space = Array.make heap_size 0
let to_space = Array.make heap_size 0

let heap_top = ref 0




(* mlvalues sauf makeblock / makeclosure *)

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
  to_space.(b) / 256

let tag (b : ptr) = 
  (* a priori, problème si le bloc a taille >= 128 *)
  to_space.(b) land 255

let unit = 0

let make_header (tag : long) (sz : long) =
  val_long (tag + 256 * sz)

let get_field (v : value) (i : int) =
  to_space.((ptr_val v) + i + 1)

let set_field (v : value) (i : int) (x : value) = 
  to_space.((ptr_val v) + i + 1) <- x

let get_bytes (v : value) (i : int) = 
  (* ici, on place un char par mot *)
  to_space.((ptr_val v) + i + 1)

let set_bytes (v : value) (i : int) (x : value) =  (* cf get_bytes. *)
  to_space.((ptr_val v) + i + 1) <- x

let closure_tag = 247
let env_tag = 250 (* quel est le bon numéro ??? *)
let infix_tag = 249
let fwd_ptr_tag = 248

let addr_closure (c : value) = get_field c 0
let env_closure (c : value) = val_long ((long_val c) + 2)

let val_codeptr o = val_long o (* ??? *)




(* registres de interp *)
let stack_size = 1024
let stack = Array.make stack_size (val_long 0)

let acc = ref (val_long 0)
let env = ref (val_long 0)



(* ALLOC fonctions *)

let heap_can_alloc size =
  (!heap_top) + size <= heap_size

(* let move_addr value =
   if is_ptr (!value) then (* val pointe vers un bloc *)
    if tag (!value) == fwd_ptr_tag then (* le bloc pointé est un fwd *)
      value := 
    else 
      begin
        ()
      end
   else () *)

(* lance le gc *)
let run_gc () =
  print_string "lancement gc";
  print_newline ()


(* Alloue si possible, sinon lance le GC puis alloue *)
let alloc size = 
  print_string "try alloc ";
  print_int size;
  print_newline ();
  if heap_can_alloc size then
    begin
      print_string "can alloc";
      print_newline ();
      let res = heap_top in
      heap_top := (!heap_top) + size;
      !res  
    end
  else 
    begin
      print_string "cant alloc";
      print_newline ();
      run_gc ();
      if heap_can_alloc size then 
        begin
          let res = heap_top in
          heap_top := (!heap_top) + size;
          !res  
        end
      else 
        begin
          print_string "plus de mémoire";
          print_newline ();
          -1
        end
    end



(* mlvalues makebloc / makeclosure *)

let make_block (tag : long) (sz : long) =
  let sz = if sz = 0 then 1 else sz in
  let a = alloc (sz + 1) in
  to_space.(a) <- val_long (tag + 256 * sz);
  val_ptr a

let make_closure pc size =
  let res = make_block closure_tag size in
  set_field res 0 pc;
  res
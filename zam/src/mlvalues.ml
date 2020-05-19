(* alloc définitions *)

let heap_size = 5

let from_space = ref (Array.make heap_size 0)
let to_space = ref (Array.make heap_size 0)
let heap_top = ref 0



(* mlvalues sauf makeblock / makeclosure *)

type value = int 
type long = int
type ptr = int

let global_size = 10
let global = Array.make (global_size) 0

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
  (!from_space).(b) / 256

let tag (b : ptr) = 
  (* a priori, problème si le bloc a taille >= 128 *)
  (!from_space).(b) land 255

let unit = 0

let get_global (i : int) =
  global.(i)

let set_global (i : int) (x: value) =
  global.(i) <- x

let make_header (tag : long) (sz : long) =
  val_long (tag + 256 * sz)

let get_field (v : value) (i : int) =
  (!from_space).((ptr_val v) + i + 1)

let set_field (v : value) (i : int) (x : value) = 
  (!from_space).((ptr_val v) + i + 1) <- x

let get_bytes (v : value) (i : int) = 
  (* ici, on place  un char par mot *)
  (!from_space).((ptr_val v) + i + 1)

let set_bytes (v : value) (i : int) (x : value) =  (* cf get_bytes. *)
  (!from_space).((ptr_val v) + i + 1) <- x

let closure_tag = 247
let env_tag = 250 (* quel est le bon numéro ??? *)
let infix_tag = 249
let fwd_ptr_tag = 248

let addr_closure (c : value) = get_field c 0
let env_closure (c : value) = val_long ((long_val c) + 2)

let val_codeptr o = val_long o (* ??? *)




(* registres de interp *)
let stack_size = 1024
let sp = ref 0
let stack = Array.make stack_size (val_long 0)

let acc = ref (val_long 0)
let env = ref (val_long 0)


(* ALLOC fonctions *)

let heap_can_alloc size =
  (!heap_top) + size <= heap_size


let next = ref 0 (* premiere pos disponible dans to_space lors de la copie *)

(* 
  Traite le déplacement d'un bloc de to_space vers from_space si nécéssaire, 
  sinon suit le fwd_ptr
  value est le mlvalue pointeur vers le bloc, 
  is_array dis si la source est un tableau ou pas : true si pile/tas , faux si env/acc
  source_reg est le registre qui contient value si is_array est false (acc, env)
  source_arr est le tableau contenant value a la pos pos_arr si is_array est true (pile, tas)
*)
let move_addr value is_array source_reg source_arr pos_arr =
  (* print_string "move_addr : ";
     print_int value;
     print_newline (); *)
  if is_ptr value then (* val pointe vers un bloc *)
    begin
      (* print_string "is_ptr\n"; *)
      if tag (ptr_val value) == fwd_ptr_tag then (* le bloc pointé est un fwd_ptr *)
        (* on fais pointé value sur la nouvelle destination *)
        begin
          (* print_string "is_fwd\n"; *)
          if is_array then source_arr.(pos_arr) <- get_field value 0
          else source_reg := get_field value 0
        end
      else (* le bloc n'a pas été déplacé, on le copie *)
        begin
          (* print_string "isnt_fwd\n"; *)
          let old = !next in (* sauvegarde de l'endroit où on va copier dans to_space *)
          (* print_string "old : ";
             print_int old;
             print_newline (); *)
          (* on copie tout le bloc, header compris dans to_space *)
          (!to_space).(old) <- get_field value (-1); (* copie le header *)
          for j = 0 to (size (val_ptr value)) - 1 do (* copie tout les fields *)
            (!to_space).(old + j + 1) <- get_field value j
          done;
          (* print_string "copie : \n";
             for i = 0 to (size (val_ptr value)) do
             print_string "(o : ";
             print_int (get_field value (i-1));
             print_string ", n :";
             print_int ((!to_space).(old + i));
             print_string ") ";
             done;
             print_newline (); *)

          next := (size (val_ptr value)) + 1; (* prochaine pos dispo dans to_space *)
          (* print_string "next : ";
             print_int !next;
             print_newline (); *)

          (* on change le tag du bloc en fwd_ptr car il a été déplacé  *)
          set_field value (-1) (make_header fwd_ptr_tag (size (ptr_val value)));
          (* ajoute le fwd_ptr dans from_space vers la nouvelle position dans to_space *)
          set_field value 0 (val_ptr old);
          (* on fait pointé value vers le nouveau bloc dans to_sapce *)
          if is_array then source_arr.(pos_arr) <- val_ptr old
          else source_reg := val_ptr old
        end
    end
  else 
    (* print_string "isnt ptr\n"; *)
    ()

(* lance le gc *)
let run_gc () =
  print_string "lancement gc\n";
  (* on parcours les éléments de la pile *)
  (* print_string "sp : ";
     print_int !sp;
     print_newline (); *)
  for i = 0 to !sp - 1 do
    let value = stack.(i) in
    move_addr value true (ref 0) stack i;
  done;
  (* print_string "fin pile\n"; *)

  (* on traite l'accu *)
  move_addr !acc false acc stack (-1);
  (* print_string "fin acc\n"; *)
  (* on traite l'env *)
  move_addr !env false env stack (-1);
  (* print_string "fin env\n"; *)

  (* maintenant on parcours les fields de tout les objets
     qu'on a bougé dans to_space *)
  (* print_string "debut to_space\n"; *)
  let i = ref 0 in
  while !i < !next do (* parcours les headers *)
    (* print_string "size : ";  *)
    let size = (!to_space).(!i) / 256 in
    (* print_int size; *)
    for j = !i + 1 to size do (* parcours les fields du bloc courant *)
      let value = (!to_space).(!i) in
      move_addr value true (ref 0) !to_space !i
    done;
    i := !i + size + 1 (* passe au header du bloc suivant dans to_space *)
  done;
  (* print_string "fin to_space\n"; *)

  (* on echange from_space et to_space *)
  let tmp = !from_space in
  from_space := !to_space;
  to_space := tmp;
  heap_top := !next;
  next := 0;
  print_string "fin gc";
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
      let res = !heap_top in
      heap_top := (!heap_top) + size;
      res  
    end
  else 
    begin
      print_string "cant alloc";
      print_newline ();
      run_gc ();
      if heap_can_alloc size then 
        begin
          let res = !heap_top in
          heap_top := (!heap_top) + size;
          res  
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
  (!from_space).(a) <- val_long (tag + 256 * sz);
  val_ptr a

let make_closure pc size =
  let res = make_block closure_tag size in
  set_field res 0 pc;
  res
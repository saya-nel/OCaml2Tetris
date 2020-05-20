let debug = false

(* ALLOC fonctions *)

let heap_can_alloc size =
  (!Domain.heap_top) + size <= !Domain.heap_size


let next = ref 0 (* premiere pos disponible dans to_space lors de la copie *)


(* OCaml a un gc : la fonction ignore ne fait rien. *)
(* mini-ml n'a pas de gc : la fonction ignore libère un bloc mémoire *)
let free a = ignore a 

let resize_spaces size =
  (* on traite le redimensionnement des semi spaces si nécéssaire *)
  let half = !Domain.heap_size / 2 in (* nombre d'éléments a la moitié d'un semi space *)
  let quarter = half / 2 in (* nombre d'élements au quart d'un semi space *)
  (* définition de la nouvelle taille *)
  let old_size = !Domain.heap_size in
  (* si il n'y a pas assez de place pour l'allocation
     on redimensionne en rajoutant a la taille la place de l'allocation
     puis multiplie le tout par deux *)
  if !Domain.heap_top + size > !Domain.heap_size then
    Domain.heap_size := (!Domain.heap_size + size) * 2
  else 
    begin
      (* si taille + allocation < 25% de la taille de base, on diminue la taille par deux *)
      if quarter > (!Domain.heap_top + size) then (* si remplis à moins de 25% *)
        Domain.heap_size := !Domain.heap_size / 2
      else ()
    end;
  (* si la taille à changée *)
  if old_size <> !Domain.heap_size then
    begin
      if debug then
        begin
          print_string "resize spaces, old size : ";
          print_int old_size;
          print_string ", new size : ";
          print_int !Domain.heap_size;
          print_newline ()
        end
      else ();
      (* création du nouveau from_space à la bonne taille *)
      let new_from_space = Array.make !Domain.heap_size (Mlvalues.val_long 0) in
      (* copie de l'ancien from_space dans le nouveau *)
      for i = 0 to !Domain.heap_top - 1 do
        new_from_space.(i) <- (!Domain.from_space).(i)
      done;
      Pervasives.ignore !Domain.from_space;
      Domain.from_space := new_from_space;
      Pervasives.ignore !Domain.to_space;
      Domain.to_space := Array.make !Domain.heap_size (Mlvalues.val_long 0) 
    end
  else ()


(* Traite le déplacement d'un bloc de to_space vers from_space si nécéssaire,                  *)
(* sinon suit le fwd_ptr                                                                       *)
(* value est le mlvalue pointeur vers le bloc,                                                 *)
(* is_array dis si la source est un tableau ou pas : true si pile/tas , faux si env/acc        *)
(* source_reg est le registre qui contient value si is_array est false (acc, env)              *)
(* source_arr est le tableau contenant value a la pos pos_arr si is_array est true (pile, tas) *)

let move_addr value is_array source_reg source_arr pos_arr =
  if Mlvalues.is_ptr value then (* val pointe vers un bloc *)
    begin
      if Block.tag (Mlvalues.ptr_val value) = Block.fwd_ptr_tag then (* le bloc pointé est un fwd_ptr *)
        (* on fais pointé value sur la nouvelle destination *)
        begin
          if is_array then source_arr.(pos_arr) <- Block.get_field value 0
          else source_reg := Block.get_field value 0
        end
      else (* le bloc n'a pas été déplacé, on le copie *)
        begin
          let old = !next in (* sauvegarde de l'endroit où on va copier dans to_space *)
          (* on copie tout le bloc, header compris dans to_space *)
          (!Domain.to_space).(old) <- Block.get_field value (-1); (* copie le header *)
          for j = 0 to (Block.size (Mlvalues.ptr_val value)) - 1 do (* copie tout les fields *)
            (!Domain.to_space).(old + j + 1) <- Block.get_field value j
          done;
          next := (Block.size (Mlvalues.ptr_val value)) + 1; (* prochaine pos dispo dans to_space *)
          (* on change le tag du bloc en fwd_ptr car il a été déplacé  *)
          Block.set_field value (-1) (Block.make_header Block.fwd_ptr_tag (Block.size (Mlvalues.ptr_val value)));
          (* ajoute le fwd_ptr dans from_space vers la nouvelle position dans to_space *)
          Block.set_field value 0 (Mlvalues.val_ptr old);
          (* on fait pointé value vers le nouveau bloc dans to_space *)
          if is_array then source_arr.(pos_arr) <- Mlvalues.val_ptr old
          else source_reg := Mlvalues.val_ptr old
        end
    end
  else ()

(* lance le gc *)
let run_gc size =
  if debug then print_string "lancement gc\n" else ();
  (* on parcours les éléments de la pile *)
  for i = 0 to !Domain.sp - 1 do
    let value = Domain.stack.(i) in
    move_addr value true (ref (Mlvalues.val_long 0)) Domain.stack i
  done;

  (* on traite l'accu *)
  move_addr !Domain.acc false Domain.acc Domain.stack (-1);
  (* on traite l'env *)
  move_addr !Domain.env false Domain.env Domain.stack (-1);

  (* maintenant on parcours les fields de tout les objets qu'on a bougé dans to_space *)
  let i = ref 0 in
  while !i < !next do (* parcours les headers *)
    let size = (Mlvalues.long_val (!Domain.from_space).(!i)) / 256 in
    for j = !i + 1 to size do (* parcours les fields du bloc courant *)
      let value = (!Domain.from_space).(!i) in
      move_addr value true (ref (Mlvalues.val_long 0)) !Domain.from_space !i
    done;
    i := !i + size + 1 (* passe au header du bloc suivant dans to_space *)
  done;

  (* on echange from_space et to_space *)
  let tmp = !Domain.from_space in
  Domain.from_space := !Domain.from_space;
  Domain.from_space := tmp;
  Domain.heap_top := !next;
  next := 0;
  (* on redimensionne les espaces si nécéssaire *)
  resize_spaces size;

  if debug then print_string "fin gc" else ();
  print_newline ()


(* Alloue si possible, sinon lance le GC puis alloue *)
let alloc size = 
  if debug then
    begin
      print_newline ();
      print_string "try alloc ";
      print_int size;
      print_newline ()
    end
  else ();
  if heap_can_alloc size then
    begin
      if debug then (print_string "can alloc"; print_newline ()) else ();

      let res = !Domain.heap_top in
      Domain.heap_top := (!Domain.heap_top) + size;
      res  
    end
  else 
    begin
      if debug then 
        begin
          print_string "cannot alloc";
          print_newline ()
        end 
      else ();
      run_gc size;
      if heap_can_alloc size then 
        begin
          let res = !Domain.heap_top in
          Domain.heap_top := (!Domain.heap_top) + size;
          res  
        end
      else 
        begin
          if debug then 
            begin
              print_string "plus de mémoire";
              print_newline ()
            end 
          else ();
          -1
        end
    end

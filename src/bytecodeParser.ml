open String
open Str
open List

(* Verifie si str contient un label *)
let contains_label (str : string) = 
  contains str ':'

(* Renvoie le label de la str, str doit contenir un label *)
let get_label (str : string) : string =
  hd (split_on_char ':' str)

(* Renvoie la string contenant l'instruction et ses arguments *)
let get_instr_and_args (str: string) : string = 
  if contains_label str then 
    trim (nth (split_on_char ':' str) 1)
  else trim str

(* Renvoie l'instruction de str *)
let get_instruction (str : string) : string = 
  hd (split_on_char ' ' (get_instr_and_args str))

(* 
  Renvoie la liste des arguments de str 
*)
let get_instr_args (str : string) : string list = 
  let instr_and_args = get_instr_and_args str in
  let remove_dots = global_replace (regexp ",") "" instr_and_args in
  tl (split_on_char ' ' remove_dots)

let rec print_string_list (str_list : string list) : unit = match str_list with
  | [] -> print_newline ()
  | t::q -> print_string t; print_char ' '; print_string_list q

let string_list_to_string (str_list : string list) : string = 
  let rec aux (str_list : string list) : string = match str_list with
    | [] -> ""
    | t::[""] -> t ^ "|]"
    | t::q -> t ^ "; " ^ aux(q)
  in "[|" ^ aux str_list

let () =
  print_endline "LECTURE BYTECODE : \n";
  (* (string * int) list pour les labels et leur position *)
  let labels = ref [] in
  (* string list d'instructions resultat *)
  let instructions = ref [] in 
  try 
    (* numero d'instruction, utile pour les labels *)
    let index = ref 0 in 
    while true do 
      let line = read_line () in
      print_endline line;
      (* si il y a un label, on let met dans la map avec son index *)
      if contains_label line then 
        labels := ((get_label line), !index)::(!labels);
      (* on ajoute l'instruction dans la liste resultat *)
      instructions := (get_instruction line)::(!instructions);
      (* on ajoute les arguments dans la liste resultat *)
      let args = rev (get_instr_args line) in
      instructions := args@(!instructions);
      (* on incrémente l'index de 1 (instruction) + nb args *)
      index := !index + 1 + length args
    done
  with End_of_file -> 
    (* on reverse la liste pour l'avoir dans le bon sens *)
    instructions := rev !instructions;
    (* on iter sur la liste des labels
    et pour chaque label existant dans instructions le remplace par son index *)
    iter 
    (
      fun (label, pos) -> 
        instructions := 
          map 
            (fun instruction -> if instruction = label then string_of_int pos else instruction)
            !instructions
    )
    !labels;
    (* affiche la liste resultat *)
    print_endline "\nTABLEAU D'INSTRUCTIONS RESULTAT :\n";
    print_endline (string_list_to_string !instructions)
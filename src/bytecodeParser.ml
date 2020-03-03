open String
open Str
open List
open Printf

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

(* Retourne l'entier correspondant au code d'une instruction, sinon renvoie le parametre sous forme d'entier *)
let get_op_code_from_instr (instr : string) : int = match instr with 
  | "acc0" -> 0 | "acc1" -> 1 | "acc2" -> 2 | "acc3" -> 3 | "acc4" -> 4 | "acc5" -> 5 | "acc6" -> 6 | "acc7" -> 7 
  | "acc" -> 8
  | "push" -> 9
  | "pushacc0" -> 10 | "pushacc1" -> 11 | "pushacc2" -> 12 | "pushacc3" -> 13 | "pushacc4" -> 14 | "pushacc5" -> 15 | "pushacc6" -> 16 | "pushacc7" -> 17 | "pushacc8" -> 18
  | "pushacc" -> 18
  | "pop" -> 19
  | "assign" -> 20
  | "envacc1" -> 21 | "envacc2" -> 22 | "envacc3" -> 23 | "envacc4" -> 24 
  | "envacc" -> 25
  | "pushenvacc1" -> 26 | "pushenvacc2" -> 27 | "pushenvacc3" -> 28 | "pushenvacc4" -> 29
  | "pushenvacc" -> 30
  | "push-retaddr" -> 31
  | "apply" -> 32
  | "apply1" -> 33 | "apply2" -> 34 | "apply3" -> 35
  | "appterm" -> 36 
  | "appterm1" -> 37 | "appterm2" -> 38 | "appterm3" -> 39
  | "return" -> 40
  | "restart" -> 41
  | "grab" -> 42
  | "closure" -> 43
  | "closurerec" -> 44
  | "offsetclosurem2" -> 45
  | "offsetclosure0" -> 46 | "offsetclosure2" -> 47
  | "offsetclosure" -> 48
  | "pushoffsetclosurem2" -> 49
  | "pushoffsetclosure0" -> 50 | "pushoffsetclosure2" -> 51
  | "pushoffsetclosure" -> 52
  | "getglobal" -> 53
  | "pushgetglobal" -> 54
  | "getglobalfield" -> 55
  | "pushgetglobalfield" -> 56
  | "setglobal" -> 57
  | "atom0" -> 58
  | "atom" -> 59
  | "pushatom0" -> 60
  | "pushatom" -> 61
  | "makeblock" -> 62
  | "makeblock1" -> 63 | "makeblock2" -> 64 | "makeblock3" -> 65
  | "makefloatblock" -> 66
  | "getfield0" -> 67 | "getfield1" -> 68 | "getfield2" -> 69 | "getfield3" -> 70
  | "getfield" -> 71
  | "getfloatfield" -> 72
  | "setfield0" -> 73 | "setfield1" -> 74 | "setfield2" -> 75 | "setfield3" -> 76 
  | "setfield" -> 77
  | "setfloatfield" -> 78
  | "vectlength" -> 79
  | "getvectitem" -> 80
  | "setvectitem" -> 81
  | "getstringchar" -> 82
  | "setstringchar" -> 83
  | "branch" -> 84
  | "branchif" -> 85
  | "branchifnot" -> 86
  | "switch" -> 87
  | "boolnot" -> 88
  | "pushtrap" -> 89
  | "poptrap" -> 90
  | "raise" -> 91
  | "check-signals" -> 92
  | "c-call1" -> 93 | "c-call2" -> 94 | "c-call3" -> 95 | "c-call4" -> 96 | "c-call5" -> 97 | "c-calln" -> 98
  | "const0" -> 99 | "const1" -> 100 | "const2" -> 101 | "const3" -> 102
  | "constint" -> 103
  | "pushconst0" -> 104 | "pushconst1" -> 105 | "pushconst2" -> 106 | "pushconst3" -> 107 
  | "pushconstint" -> 108
  | "negint" -> 109
  | "addint" -> 110
  | "subint" -> 111
  | "mulint" -> 112
  | "divint" -> 113
  | "modint" -> 114
  | "andint" -> 115
  | "orint" -> 116
  | "xorint" -> 117
  | "lslint" -> 118
  | "lsrint" -> 119
  | "asrint" -> 120
  | "eq" -> 121
  | "neq" -> 122
  | "ltint" -> 123
  | "leint" -> 124
  | "gtint" -> 125
  | "geint" -> 126
  | "offsetint" -> 127
  | "offsetref" -> 128
  | "isint" -> 129
  | "getmethod" -> 130
  | "beq" -> 131
  | "bneq" -> 132
  | "bltint" -> 133
  | "bleint" -> 134
  | "bgtint" -> 135
  | "bgeint" -> 136
  | "ultint" -> 137
  | "ugeint" -> 138
  | "bultint" -> 139
  | "bugeint" -> 140
  | "getpubmet" -> 141
  | "getdynmet" -> 142
  | "stop" -> 143
  | "event" -> 144
  | "break" -> 145
  | e -> 
    try   
      print_endline (e ^ " n'est pas une instruction, transformation en entier."); 
      int_of_string e
    with ex -> 
      - 1

(* Affiche un liste de string *)
let rec print_string_list (str_list : string list) : unit = match str_list with
  | [] -> print_endline "\n"
  | t::[""] -> print_string ("\"" ^ t ^ "\"|]"); print_string_list []
  | t::q -> print_string ("\"" ^ t ^ "\"; "); print_string_list q

(* Transforme une string list d'instructions en string representant un tableau d'instructions sous forme d'entiers *)
let string_list_to_string (str_list : string list) : string = 
  let rec aux (str_list : string list) : string = match str_list with
    | [] -> ""
    | t::[""] -> string_of_int (get_op_code_from_instr t)^ "|]"
    | t::q -> string_of_int (get_op_code_from_instr t) ^ "; " ^ aux(q)
  in "[|" ^ aux str_list

(* écrit le tableau d'instructions dans un nouveau fichier src/vm.ml *)
let write_instr_array (instr_array : string) : unit =
  let oc = open_out "../src/vm.ml" in
    fprintf oc "(* etat de la vm *)\nlet code = %s\n" instr_array;
    close_out oc

(* écrit le contenu du fichier src/vm_code.ml a la fin du fichier src/vm.ml *)
let write_vm_code () : unit =
  (* string du fichier src/vm_code.ml *)
  let ic = open_in "../src/vm_code.ml" in
  let to_write = really_input_string ic (in_channel_length ic) in
  close_in ic;
  (* on écrit le contenu de to_write dans src/vm.ml *)
  let oc = open_out_gen [Open_append] 0o666 "../src/vm.ml" in
    fprintf oc "%s\n" to_write
    

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
      if trim line != "" then 
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
    print_string "[|"; print_string_list !instructions;
    (* transforme la liste en string avec les instructions sous formes d'entiers *)
    print_endline "CONVERTION STRING LIST -> STRING (avec instructions sous forme d'entiers) :\n";
    let res = string_list_to_string !instructions in
    (* on écrit dans le fichier le tableau d'instructions *)
    print_endline "\nEcriture du tableau d'instructions dans src/vm.ml ok.";
    write_instr_array res;
    (* on écrit a la fin du fichier le contenu de src/vm_code.ml *)
    print_endline "Ecriture du contenu de src/vm_code.ml dans src/vm.ml ok.";
    write_vm_code ()
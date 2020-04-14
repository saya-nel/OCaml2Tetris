open OByteLib
open OByteLib.Instr
open Printf

(* 
  Retourne l'op code d'une instruction donnée en string, si  
  l'instruciton n'existe pas, retourne int_of_string de l'argument  
*)
let get_op_code (instr : string) : int = match instr with  
| "ACC0"                      -> 0
| "ACC1"                      -> 1
| "ACC2"                      -> 2
| "ACC3"                      -> 3
| "ACC4"                      -> 4
| "ACC5"                      -> 5
| "ACC6"                      -> 6
| "ACC7"                      -> 7
| "ACC"                    -> 8
| "PUSH"                      -> 9 
| "PUSHACC0"                  -> 10
| "PUSHACC1"                  -> 11
| "PUSHACC2"                  -> 12
| "PUSHACC3"                  ->  13
| "PUSHACC4"                  ->  14
| "PUSHACC5"                  ->  15
| "PUSHACC6"                  ->  16
| "PUSHACC7"                  ->  17
| "PUSHACC"                 ->  18
| "POP"                     ->  19
| "ASSIGN"                  ->  20
| "ENVACC1"                   ->  21
| "ENVACC2"                   ->  22
| "ENVACC3"                   ->  23
| "ENVACC4"                   ->  24
| "ENVACC"                ->  25
| "PUSHENVACC1"               ->  26
| "PUSHENVACC2"               ->  27
| "PUSHENVACC3"               ->  28
| "PUSHENVACC4"               ->  29
| "PUSHENVACC"              ->  30
| "PUSH_RETADDR"          ->  31
| "APPLY"                   ->  32
| "APPLY1"                    ->  33
| "APPLY2"                    ->  34
| "APPLY3"                    ->  35
| "APPTERM"             ->  36
| "APPTERM1"                 ->  37
| "APPTERM2"                 ->  38
| "APPTERM3"                 ->  39
| "RETURN"                   ->  40
| "RESTART"                   ->  41
| "GRAB"                    ->  42
| "CLOSURE"           ->  43
| "CLOSUREREC"    ->  44
| "OFFSETCLOSUREM2"           ->  45
| "OFFSETCLOSURE0"            ->  46
| "OFFSETCLOSURE2"            ->  47
| "OFFSETCLOSURE"            ->  48
| "PUSHOFFSETCLOSUREM2"       ->  49
| "PUSHOFFSETCLOSURE0"        ->  50
| "PUSHOFFSETCLOSURE2"        ->  51
| "PUSHOFFSETCLOSURE"        ->  52
| "GETGLOBAL"                ->  53
| "PUSHGETGLOBAL"            ->  54
| "GETGLOBALFIELD"      ->  55
| "PUSHGETGLOBALFIELD"  ->  56
| "SETGLOBAL"                ->  57
| "ATOM0"                     ->  58
| "ATOM"                   ->  59
| "PUSHATOM0"                 ->  60
| "PUSHATOM"               ->  61
| "MAKEBLOCK"        ->  62
| "MAKEBLOCK1"            ->  63
| "MAKEBLOCK2"             ->  64
| "MAKEBLOCK3"             ->  65
| "MAKEFLOATBLOCK"          ->  66
| "GETFIELD0"                 ->  67
| "GETFIELD1"                 ->  68
| "GETFIELD2"                 ->  69
| "GETFIELD3"                 ->  70
| "GETFIELD"                 ->  71
| "GETFLOATFIELD"            ->  72
| "SETFIELD0"                 ->  73
| "SETFIELD1"                 ->  74
| "SETFIELD2"                 ->  75
| "SETFIELD3"                 ->  76
| "SETFIELD"                ->  77
| "SETFLOATFIELD"            ->  78
| "VECTLENGTH"                ->  79
| "GETVECTITEM"               ->  80
| "SETVECTITEM"               ->  81
| "GETBYTESCHAR"              ->  82
| "SETBYTESCHAR"              ->  83
| "BRANCH"                ->  84
| "BRANCHIF"               ->  85
| "BRANCHIFNOT"           ->  86
| "SWITCH"           ->  87
| "BOOLNOT"                   ->  88
| "PUSHTRAP"              ->  89
| "POPTRAP"                   ->  90
| "RAISE"                     ->  91
| "CHECK_SIGNALS"             ->  92
| "C_CALL1"                ->  93
| "C_CALL2"                ->  94
| "C_CALL3"               ->  95
| "C_CALL4"               ->  96
| "C_CALL5"                ->  97
| "C_CALLN"        ->  98
| "CONST0"                    ->  99
| "CONST1"                    ->  100
| "CONST2"                    ->  101
| "CONST3"                    ->  102
| "CONSTINT"                 ->  103
| "PUSHCONST0"                ->  104
| "PUSHCONST1"                ->  105
| "PUSHCONST2"                ->  106
| "PUSHCONST3"                ->  107
| "PUSHCONSTINT"            ->  108
| "NEGINT"                    ->  109
| "ADDINT"                    ->  110
| "SUBINT"                    ->  111
| "MULINT"                    ->  112
| "DIVINT"                    ->  113
| "MODINT"                    ->  114
| "ANDINT"                    ->  115
| "ORINT"                     ->  116
| "XORINT"                    ->  117
| "LSLINT"                    ->  118
| "LSRINT"                    ->  119
| "ASRINT"                    ->  120
| "EQ"                        ->  121
| "NEQ"                       ->  122
| "LTINT"                     ->  123
| "LEINT"                     ->  124
| "GTINT"                     ->  125
| "GEINT"                     ->  126
| "OFFSETINT"                ->  127
| "OFFSETREF"                ->  128
| "ISINT"                     ->  129
| "GETMETHOD"                 ->  130
| "BEQ"               ->  131
| "BNEQ"              ->  132
| "BLTINT"            ->  133
| "BLEINT"            ->  134
| "BGTINT"            ->  135
| "BGEINT"            ->  136
| "ULTINT"                    ->  137
| "UGEINT"                    ->  138
| "BULTINT"           ->  139
| "BUGEINT"           ->  140
| "GETPUBMET"     ->  141
| "GETDYNMET"                 ->  142
| "STOP"                      ->  143
| "EVENT"                     ->  144
| "BREAK"                     ->  145
| "RERAISE"                   ->  146
| "RAISE_NOTRACE"             ->  147
| "GETSTRINGCHAR"             ->  148
| s -> int_of_string s 

(* 
  Transforme une string list d'instructions en string representant un tableau d'instructions sous forme d'entiers 
  ex : ["CONST3"; "PUSHACC0"; "OFFSETINT2"] deviens "[|102; 10; 127; 2|]"
*)
let string_list_to_string (str_list : string list) : string = 
  let rec aux (str_list : string list) : string = match str_list with
    | [] -> ""
    | t::[] -> string_of_int (get_op_code t)^ "|]"
    | t::q -> string_of_int (get_op_code t) ^ "; " ^ aux(q)
  in "[|" ^ aux str_list

(* 
  Serialize le tableau d'instruction sous la forme "CONST3 PUSHACC0 BNEQ 3 6 CONST3" etc
*)
let instr_array_to_string (instrs : Instr.t array) : string = 
  let rec aux (instrs : Instr.t list) : string = match instrs with
    | [] -> ""
    | t::q -> (to_string t) ^ " " ^ (aux q)
  in
  aux (Array.to_list instrs)

(* 
  Renvoie le tableau des instructions sous forme ["CONST3"; "PUSHACC0"; "BNEQ 3 6"; "CONST3"] etc
*)
let instr_string_with_args (instrs : Instr.t array) : string list = 
  let rec aux (instrs : Instr.t list) : string list  = match instrs with
    | [] -> []
    | t::q -> (to_string t)::(aux q)
  in aux (Array.to_list instrs)

(* 
  Renvoie le nombre d'arguments se situant avant une instruction d'index donnée dans la liste d'instructions
  ex : nb_args_before_ind ["CONST3"; "PUSHACC0"; "BNEQ 3 6"; "CONST3"] 3 donnera 2
*)
let nb_args_before_ind (instrs : string list) (ind : int) : int =
  let res = ref 0 in
  List.iteri 
    (fun i s -> 
      let splitted = String.split_on_char ' ' s in
      if i < ind then res := !res + (List.length splitted) - 1
    ) 
    instrs
  ; 
  !res

(* 
  renvoie l'instruction contenue dans une string  
  ex : get_instr "CONSTINT 3" renvoie "CONSTINT"
*)
let get_instr (s : string) : string =
  List.hd (String.split_on_char ' ' s)
  
(* 
  renvoie l'argument de position i dans s (l'instruction compte dans la "position")
  ex : get_arg_nth "BENEQ 3 6" 2 renvoie 6
*)
let get_arg_nth (s : string) (i : int) : string =
  List.nth (String.split_on_char ' ' s) i

(* 
  Met la valeur d'un label à jour si nécéssaire, 
  par rapport au nombres d'arguments le precedent dans instrs
  instrs est une liste de la forme ["INSTR1 arg1 arg2", "INSTR2"] etc
  instr est une instruction avec ses arguments, de la forme "INSTR1 arg1 arg2"
*)
let replace_label_index (instrs : string list) (instr : string) : string = match (get_instr instr) with
  | "PUSH_RETADDR"
  | "BRANCH" 
  | "BRANCHIF"
  | "BRANCHIFNOT"
  | "PUSHTRAP" ->
      let old_val = int_of_string (get_arg_nth instr 1) in
      let new_val = old_val + nb_args_before_ind instrs (old_val) in
      (get_instr instr) ^ " " ^ string_of_int new_val
  | "CLOSURE"
  | "BEQ"
  | "BNEQ"
  | "BLINT"
  | "BLEINT"
  | "BGTINT"
  | "BGEINT"
  | "BULTINT"
  | "BUGEINT" ->
      let old_val = int_of_string (get_arg_nth instr 2) in
      let new_val = old_val + nb_args_before_ind instrs (old_val) in
      (get_instr instr) ^ " " ^ (get_arg_nth instr 1) ^ " " ^ string_of_int new_val 
  | _ -> instr

(* 
  Met la valeur de tout les labels à jour
  instrs est une liste de la forme ["INSTR1 arg1 arg2", "INSTR2"] etc
*)
let replace_labels_indexes (instrs : string list) : string list =
  let rec aux curr_instrs = match curr_instrs with
    | [] -> []
    | t::q -> (replace_label_index instrs t)::(aux q)
  in aux instrs

(* écrit le tableau d'instructions dans un nouveau fichier src/zam/interp.ml *)
let write_instr_array ?(dst="./zam/input.ml") (instr_array : string) : unit =
  let oc = open_out dst in
    fprintf oc "(* etat de la vm *)\nlet code = %s\n" instr_array;
    close_out oc

(* MAIN *)
let () = 
  (* chemin du fichier .cmo *)
  let inpath =
    match Sys.argv with
    | [| _; inpath |] -> inpath
    | _ -> failwith "Erreur chemin fichier" in

  (* résultats de obytelib *)
  let cmofile = 
    Cmofile.read inpath in

  (* on récupère les champs *)
  let data, symb, prim, code = Cmofile.reloc cmofile in

  (* on va traiter la partie code *)

  (* serialize le code sous forme ["instr1 arg1 arg2"; "instr2"; "instr3 arg1"] etc *)
  let with_args = instr_string_with_args code in

  (* met à jour les indexs des labels *)
  let replaced = replace_labels_indexes with_args in

  (* met les instructions sous la forme ["instr1"; "arg1"; "arg2"; "instr2"] ect *)
  let to_send = String.split_on_char ' ' (String.concat " " replaced) in

  (* on recupère le tableau serializé, avec instructions remplacés par op codes *)
  let serial = string_list_to_string to_send in

   (* on écrit dans le fichier ../zam/input.ml le tableau d'instructions *)
  write_instr_array ~dst:"./zam/input.ml" serial;

  (* pour afficher le bytecode :  *)
  Code.print data symb prim stdout code
let inputs = ref []
let action = ref (`Compile : [ `Compile | `PrintAst])
let stdlib = ref "stdlib"

let type_check = ref false
let inline_depth = ref 10
let print_ast = ref false
let print_past = ref false
let globalize = ref true
let lifting = ref true
and folding = ref true

let set_action a () = action := a

let add_file f = inputs := !inputs @ [f] 
let source_dir = ref ""
let destination_dir = ref "generated_files"

let () =
  Arg.parse [
      ("-printpast", Arg.Set print_past,
       " : affiche l'AST en syntaxe Caml");
      ("-printast", Arg.Set print_ast,
       " : affiche l'AST simplifié en syntaxe Caml (après typage et optimisation)");
      ("-typecheck", Arg.Set type_check, 
       " : type le programme est abandonne si celui-ci est mal typé");
      ("-inline", Arg.Set_int inline_depth,
       " : profondeur d'inlining");
      ("-noglobalize", Arg.Clear globalize,
       " : désactive la globalisation des valeurs immutables allouées.");
      ("-nofolding", Arg.Clear folding,
       " : désactive la propagation des constantes");
      ("-compile", Arg.Unit (set_action `Compile), 
       " : compile vers le langage de la VM Nand2Tetris");
      ("-src", Arg.Set_string source_dir,
       " : spécifie où chercher les fichiers sources à compiler");
      ("-dst", Arg.Set_string destination_dir, 
       " : spécifie le dossier où seront placés les fichiers compilés");
      ("-stdlib",Arg.Set_string stdlib, 
       "chemin vers la bibliothèque d'execution de mini-ml");
      ("-assert", Arg.Set Ast_fold.compile_assertions,
       " : embarque les assertions dans le code.")
    ] add_file "Usage:\n  ./compile [options] <filenames ..>"	

let parse filename = 
  let ic = open_in filename in
  let mod_name = String.capitalize_ascii @@ 
                   Filename.remove_extension @@ 
                     Filename.basename filename in
  try 
    let lexbuf = Lexing.from_channel ic in
    let decls = Parser.tmodule Lexer.token lexbuf in 
    let mdl = Past.{mod_name;decls} in
    (* Print_ast.sprint_module 0 mdl |> Printf.printf "%s"; *)
    close_in ic;
    mdl
  with Parseutils.Parse_Exception(s,pos) -> 
    (close_in ic; Parseutils.error_exit pos s) ;;


let parse_modules fs = 
  List.map parse fs

let compile genv (mdl : Ast.tmodule) = 
  try 
    if !print_ast then print_string @@ Ast_print.sprint_module 0 mdl;
    let mdl = Ast2iast.rewrite mdl in
    let mdl = Iast_closure.rewrite mdl in
    let mdl = Iast_tailrec.rewrite mdl in
    let genv0 = Iast2kast.{genv with mod_name=Iast.(mdl.mod_name); init=[]} in
    let genv',kast = Iast2kast.rewrite genv0 mdl in
    let bc_mdl = Kast2bc.bc_of_tmodule genv' kast in
    let bc_mdl = Bc_fold.rewrite bc_mdl in
    (genv',bc_mdl)
  with Kast2bc.Cannot_generate_bytecode msg -> 
    (Printf.printf "cannot generate bytecode.\n%s\n" msg; exit 1)


(* compile le programme formés des modules mdls *)
let compile_all mdls =
  let genv = Iast2kast.empty_genv Runtime.primitives "" in
  if !print_past then List.iter (fun mdl -> print_string @@ Past_print.sprint_module 0 mdl) mdls;
  if !type_check then (let env = ref (Iast2kast.(genv.typed_decls)) in
                       List.iter (fun mdl -> env := Typing.type_check mdl Iast2kast.(!env)) mdls);
  let mdls = List.map Past2ast.visit_tmodule mdls in

  (* lambda lifting *)
  let mdls = List.map Ast_lift.rewrite mdls in
  
  (* globalisation des valeurs immutables allouées *)
  let mdls = if !globalize then List.map Ast_globz.rewrite mdls else mdls in

  (* intégration des appels de fonctions *)
  let mdls = List.map (Ast_inline.visit_tmodule ~depth_max:!inline_depth) mdls in
  
  (* propagation de constantes *)
  let mdls = if !folding then List.map Ast_fold.rewrite mdls else mdls in
  let (genv2,bc_mdls) = 
    List.fold_left (fun (genv,acc) mdl -> 
        let genv',bc_mdl = compile genv mdl in 
        (genv',acc @ [bc_mdl])) (genv,[])  mdls in 
  Kast2bc.bc_of_prog bc_mdls


(* point d'entrée du compilateur *)
let () = 
  let dir = !destination_dir in
  let files = List.map (Filename.concat !source_dir) !inputs in
  let mdls = parse_modules files in
  List.iter (fun (name,bc) ->
      let oc = open_out (Filename.concat dir (Bc_print.prefix ^ name ^ ".vm")) in
      Printf.fprintf oc "%s\n" (Bc_print.string_of_instrs bc);
      close_out oc) (compile_all mdls);
  
  Runtime.init dir 
    

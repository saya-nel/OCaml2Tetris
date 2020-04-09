let inputs = ref []
let action = ref (`Compile : [ `Compile | `PrintAst])
let stdlib = ref "stdlib"

let type_check = ref false
let print_ast = ref false

let set_action a () = action := a

let add_file f = inputs := !inputs @ [f] 
let source_dir = ref ""
let destination_dir = ref "generated_files"

let () =
  Arg.parse [
    ("-printast", Arg.Set print_ast,
       " : affiche l'AST en syntaxe Caml (après typage et optimisation)");
    ("-typecheck", Arg.Set type_check, 
      " : type le programme est abandonne si celui-ci est mal typé");
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

let compile genv (mdl : Past.tmodule) = 
  try 
    let genv = if not !type_check then genv 
      else let env = Typing.type_check mdl Iast2kast.(genv.typed_decls) in    
         Iast2kast.{genv with typed_decls = env} in
    let mdl = Past2ast.visit_tmodule mdl in
    let mdl = Ast_fold.visit_tmodule mdl in
    let mdl = Ast_lift.visit_tmodule mdl in
    
    if !print_ast then print_string @@ Ast_print.sprint_module 0 mdl;
    
    let mdl = Ast2iast.visit_tmodule mdl in
    let genv0 = Iast2kast.{genv with mod_name=Iast.(mdl.mod_name); init=[]} in
    let genv',kast = Iast2kast.rewrite_tmodule genv0 mdl in
    let bc_mdl = Kast2bc.bytecode_of_tmodule genv' kast in
    (genv',bc_mdl)
  with Kast2bc.Cannot_generate_bytecode msg -> 
       (Printf.printf "cannot generate bytecode.\n%s\n" msg; exit 1)

let compile_all mdls =
 let (genv2,bc_mdls) = List.fold_left (fun (genv,acc) mdl -> 
                    let genv',bc_mdl = compile genv mdl in 
                    (genv',acc @ [bc_mdl]))
   (Iast2kast.empty_genv Runtime.primitives "",[]) mdls in 
  Kast2bc.bytecode_of_prog bc_mdls

let () = 
  let dir = !destination_dir in
  let files = List.map (Filename.concat !source_dir) !inputs in
  let mdls = parse_modules files in
  List.iter (fun (name,bc) ->
    let oc = open_out (Filename.concat dir (Bc_print.prefix ^ name ^ ".vm")) in
    Printf.fprintf oc "%s\n" (Bc_print.string_of_instrs bc);
    close_out oc) (compile_all mdls);
 
  Runtime.init dir 
  
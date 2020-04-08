let inputs = ref []
let action = ref (`Compile : [ `Compile | `ParseOnly])
let stdlib = ref "stdlib"

let type_check = ref false

let set_action a () = action := a

let add_file f = inputs := !inputs @ [f] 
let source_dir = ref ""
let destination_dir = ref "generated_files"

let () =
  Arg.parse [
    ("-ast", Arg.Unit (set_action `ParseOnly),
       " : affiche l'AST en syntaxe Caml");
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
    ("-assert", Arg.Set Ast2kast.compile_assertions,
       " : empbarque les assert dans le code.")
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
      else let env = Typing.type_check mdl Ast2kast.(genv.typed_decls) in    
         Ast2kast.{genv with typed_decls = env} in
    let mdl = Past2ast.visit_tmodule mdl in
    let mdl = Lifting.visit_tmodule mdl in
    let genv0 = Ast2kast.{genv with mod_name=Ast.(mdl.mod_name); init=[]} in
    let genv',kast = Ast2kast.rewrite_tmodule genv0 mdl in
    let bc_mdl = Kast2bytecode.bytecode_of_tmodule genv' kast in
    (genv',bc_mdl)
  with Kast2bytecode.Cannot_generate_bytecode msg -> 
       (Printf.printf "cannot generate bytecode.\n%s\n" msg; exit 1)

let compile_all mdls =
 let (genv2,bc_mdls) = List.fold_left (fun (genv,acc) mdl -> 
                    let genv',bc_mdl = compile genv mdl in 
                    (genv',acc @ [bc_mdl]))
   (Ast2kast.empty_genv Runtime.primitives "",[]) mdls in 
  Kast2bytecode.bytecode_of_prog bc_mdls

let () = 
  let dir = !destination_dir in
  let files = List.map (Filename.concat !source_dir) !inputs in
  let mdls = parse_modules files in
  (* let (genv,bc_mdls) = compile_all mdls in
  let bc = List.map (function Bytecode.{bc_decls} -> bc_decls) bc_mdls in *)
  List.iter (fun (name,bc) ->
    let oc = open_out (Filename.concat dir (Bytecode2string.prefix ^ name ^ ".vm")) in
    Printf.fprintf oc "%s\n" (Bytecode2string.string_of_instrs bc);
    close_out oc) (compile_all mdls);
 
  Runtime.init dir 
  
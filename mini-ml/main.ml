let inputs = ref []
let stdlib = ref "stdlib"

let type_check = ref false
let inline_depth = ref 10
let print_ast = ref false
let print_past = ref false
let globalize = ref true
let lifting = ref true
and folding = ref true

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
      ("-src", Arg.Set_string source_dir,
       " : spécifie où chercher les fichiers sources à compiler");
      ("-dst", Arg.Set_string destination_dir, 
       " : spécifie le dossier où seront placés les fichiers compilés");
      ("-stdlib",Arg.Set_string stdlib, 
       "chemin vers la bibliothèque d'execution de mini-ml");
      ("-assert", Arg.Set Past2ast.compile_assertions,
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
(* env *)

let compile cstrenv genv (mdl : Past.tmodule) = 
    let cstrenv,mdl = Past2ast.visit_tmodule cstrenv mdl in
    (* lambda lifting *)
    let mdl = Ast_lift.rewrite mdl in
    (* globalisation des valeurs immutables allouées *)
    let mdl = if !globalize then Ast_globz.rewrite mdl else mdl in

    (* intégration des appels de fonctions *)
    
    (* let mdl = Ast_inline.visit_tmodule ~depth_max:!inline_depth mdl in *) 
    (* apparament, dans cette version du compilateur avec bootstrap, l'inlining cause des plantage en simulation. Les déclarations sont elles bien dans l'ordre ? *)
    

    (* propagation de constantes *)
    let mdl = if !folding then Ast_fold.rewrite mdl else mdl in

    let mdl = Ast_closure.rewrite mdl in
    let mdl = Ast_tailrec.rewrite mdl in
    if !print_ast then print_string @@ Ast_print.sprint_module 0 mdl;
    let genv = match genv with
              | Ast2kast.Genv (md,globals,g,p,init) -> 
                let mod_name = (match mdl with Ast.Module(mod_name,_) -> mod_name) in
                Ast2kast.Genv (mod_name,globals,g,p,[]) in
    let genv,kast = Ast2kast.rewrite genv mdl in
    let bc_mdl = match genv with
                 | Ast2kast.Genv (_,_,_,_,init) ->  
                   Kast2bc.bc_of_tmodule init kast in
    let bc_mdl = Bc_fold.rewrite bc_mdl in
    (cstrenv,genv,bc_mdl)


(* compile le programme formés des modules mdls *)
let compile_all mdls =
  let genv = let prims = List.map (fun (x,c,_) -> (x,c)) Runtime.primitives in
             Ast2kast.empty_genv prims "__init__" in
  if !print_past then List.iter (fun mdl -> print_string @@ Past_print.sprint_module 0 mdl) mdls;
  if !type_check then (let env = ref (Typing.initial_env (Runtime.primitives)) in
                       List.iter (fun mdl -> env := Typing.type_check mdl Ast2kast.(!env)) mdls);
  
  let (_,_,bc_mdls) = 
    List.fold_left (fun (cstrenv,genv,acc) mdl -> 
        let cstrenv,genv,bc_mdl = compile cstrenv genv mdl in 
        (cstrenv,genv,acc @ [bc_mdl])) (([],[]),genv,[]) mdls in 
  Kast2bc.bc_of_prog bc_mdls


(* point d'entrée du compilateur *)
let () = 
  let dir = !destination_dir in
  let files = List.map (Filename.concat !source_dir) !inputs in
  let mdls = parse_modules files in
  List.iter (fun (name,bc) ->
      let oc = open_out (Filename.concat dir (Bc_print.prefix ^ name ^ ".vm")) in
      let _ = Bc_print.string_of_instrs (Printf.fprintf oc "%s\n") bc in
      close_out oc) (compile_all mdls);
  
  Runtime.init dir 
    

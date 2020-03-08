let inputs = ref []
let action = ref (`Compile : [ `Compile | `ParseOnly])

let set_action a () = action := a

let add_file f = inputs := !inputs @ [f] 
let destination_folder = ref None

let () =
  Arg.parse [
    ("-ast", Arg.Unit (set_action `ParseOnly),
       " : affiche l'AST en syntaxe Caml");
    ("-compile", Arg.Unit (set_action `Compile), 
       " : compile vers le langage de la VM Nand2Tetris");
    ("-dst", Arg.String (fun s -> destination_folder := (Some s)), 
      " : spécifie le dossier ou seront placés les fichiers compilés");
    ] add_file "Usage:\n  ./compile [options] <filenames ..>"	


let parse filename = 
  let ic = open_in filename in
  let mod_name = String.capitalize_ascii @@ 
                 Filename.remove_extension @@ 
                 Filename.basename filename in
  let lexbuf = Lexing.from_channel ic in
  let decls = Parser.tmodule Lexer.token lexbuf in 
  let mdl = Ast.{mod_name;decls} in
  Print_ast.sprint_module 0 mdl |> Printf.printf "%s";
  close_in ic;
  mdl

let parse_modules fs = List.map parse fs

let compile genv mdl = 
  let genv0 = Ast2kast.{genv with mod_name=Ast.(mdl.mod_name); init=[]} in
  let genv',kast = Ast2kast.rewrite_tmodule genv0 mdl in
  let bc_mdl = Kast2bytecode.bytecode_of_tmodule genv' kast in
  (genv',bc_mdl)

let compile_all mdls =
 let (genv2,bc_mdls) = List.fold_left (fun (genv,acc) mdl -> 
                    let genv',bc_mdl = compile genv mdl in 
                    (genv',acc @ [bc_mdl]))
   (Ast2kast.empty_genv (Ast2kast.primitives()) "",[]) mdls in 
  Kast2bytecode.bytecode_of_prog bc_mdls


let () = 
  let dir = match !destination_folder with 
              | None -> (match !inputs with 
                         | [] -> "" 
                         | file::_ -> Filename.dirname file)
              | Some s -> s in
  let mdls = parse_modules !inputs in
  (* let (genv,bc_mdls) = compile_all mdls in
  let bc = List.map (function Bytecode.{bc_decls} -> bc_decls) bc_mdls in *)
  List.iter (fun (name,bc) ->
    let oc = open_out (Filename.concat dir (name ^ ".vm")) in
    Printf.fprintf oc "%s\n" (Bytecode2string.string_of_instrs bc);
    close_out oc) (compile_all mdls);
  begin
    let oc = open_out (Filename.concat dir ("Main.tst")) in
    Printf.fprintf oc "%s\n" "load, output-file Main.out, output-list RAM[12]%D1.6.1;

repeat 2500000 { vmstep; }";
    close_out oc
  end
 (* set sp 256, set local 300, set argument 500, set this 590, set that 600,
let parse0 file_name = 
	let ic = open_in file_name in
	let lexbuf = Lexing.from_channel ic in
	  (* close_in ic;*)
  let result = Parser.tmodule Lexer.token lexbuf in 
  
  let mod_name = String.capitalize_ascii @@ Filename.remove_extension @@ Filename.basename file_name in
  (file_name,mod_name,result)
  (*Printf.printf "(* module %s *)\n%s\n" mod_name txt;
  match !action with 
  | `ParseOnly -> ()
  | `Compile ->
    let by = Bytecode.vm_prog mod_name result |> Bytecode.string_of_code in
    let dir = match !destination_folder with 
              | None -> Filename.dirname input 
              | Some s -> s in
    let name = Filename.concat dir (mod_name ^ ".vm") in
    let oc = open_out name in
    Printf.fprintf oc "%s\n\n" by;
    close_out oc *)

let compile (lfile : ('a * 'b * 'c) list) =
  let bys,main = Bytecode.vm_prog lfile in
  List.iter2 (fun by (file_name,mod_name,result) -> 

               let txt = Ast.string_of_prog result in 
                Printf.printf "(* module %s *)\n%s\n" mod_name txt;
                
                let dir = match !destination_folder with 
                 | None -> Filename.dirname file_name 
                 | Some s -> s in
                let name = Filename.concat dir (mod_name ^ ".vm") in
                let oc = open_out name in
                let by' = if mod_name = "Main" then by @ main else by in
                Printf.fprintf oc "%s\n\n" (Bytecode.string_of_code by');
                close_out oc) bys lfile

c
   try 
     compile @@ List.map parse0 !inputs 
   with Parseutils.Parse_Exception (s,pos) -> 
        Printf.printf "Parse_error : %s   at %s\n" s (Parseutils.string_of_position pos); exit 1

      *)

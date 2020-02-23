let inputs = ref []
let action = ref (`Compile : [ `Compile | `Parse])

let add_file f = inputs := !inputs @ [f] 
let destination_folder = ref None

(* let add_action a = actions := !actions @ [a]  *)
let () =
  Arg.parse [
    ("-parse", (Arg.Unit (fun () -> action := `Parse)),
       " : affiche l'AST en syntaxe Caml");
    ("-dst", (Arg.String (fun s -> destination_folder := (Some s))), 
      " : spécifie le dossier ou seront placés les fichiers compilés");
    (* ("-ast", (Arg.Unit (fun () -> add_action `Ast)), 
      " : affiche l'AST en syntaxe Caml");
    ("-compile", (Arg.Unit (fun () -> add_action `Compile)), 
      " : compile vers le langage de la VM Nand2Tetris"); *)
    ] add_file "Usage:\n  ./algav [options]"	


let compile input = 

	let ic = open_in input in
	let lexbuf = Lexing.from_channel ic in
	(* close_in ic; *)
    let result = Parser.prog Lexer.token lexbuf in 
    let () = Ast.string_of_ast result |> Printf.printf "%s\n" in
    let by = Compiler.Custom.compile result in
    let mod_name = String.capitalize_ascii @@ Filename.remove_extension @@ Filename.basename input in
    let dir = match !destination_folder with None -> Filename.dirname input | Some s -> s in
    let name = Filename.concat dir (mod_name ^ ".vm") in
    (*
    match !action with 
     `Parse -> Ast.string_of_prog result |> Printf.printf "## AST reconnu ######################\n%s####################################\n"; exit 0
    | _ -> let mod_name = String.capitalize_ascii @@ Filename.remove_extension @@ Filename.basename input in
    let by = Bytecode.vm_prog mod_name result |> Bytecode.string_of_code in
    let dir = match !destination_folder with None -> Filename.dirname input | Some s -> s in
    let name = Filename.concat dir (mod_name ^ ".vm") in
     *)
    let oc = open_out name in
    Printf.fprintf oc "%s\n\n" by;
    close_out oc

let () = 
   try 
     List.iter compile !inputs
   with Parseutils.Parse_Exception (s,pos) -> Printf.printf "Parse_error : %s   at %s\n" s (Parseutils.string_of_position pos); exit 1
(* 
let () = 
	match action with 
	| [] -> List.iter compile inputs
	| h::t -> List.iter (fun input -> List.iter (function `Compile -> compile input | `Ast -> print_ast input)

let () =
    if Array.length Sys.argv < 2 then () else
    let ic = open_in (Sys.argv.(1)) in
    (* let oc = open_out ("simu/" ^ Sys.argv.(2) ^ ".jack") in *)
    let lexbuf = Lexing.from_channel ic in
    let result = Parser.prog Lexer.token lexbuf in
     Ast.string_of_prog result |> Printf.printf "%s---------------\n";
     Bytecode.vm_prog result |> Bytecode.string_of_code |> Printf.printf "%s\n\n";
    ()

 
let () = 
  match !mode with
  | `Compile -> Tests.draw ()
 *)
open Ast

let sptf = Printf.sprintf

let next n = n + 2

let indent_string (level:int) : string =
  String.make level ' '

let mapcat c f l = String.concat c (List.map f l)

let get_indent_level s lvl =
  let lvl' = match String.rindex_opt s '\n' with
    | None -> lvl + String.length s
    | Some i -> String.length s - i in lvl' + 1

let rec sprint_prog ms = List.map (sprint_module 0) ms
and sprint_module lvl {mod_name;decls} =
  sptf "%smodule %s = struct\n%s ;;\n%send\n\n" (indent_string lvl) mod_name 
    (mapcat " ;;\n\n" (sprint_decl (next lvl)) decls) (indent_string lvl) 
and sprint_decl lvl = function
  | Type (name,ty) -> sptf "%stype %s = " (indent_string lvl)  name ^ (sprint_ty lvl ty)
  | DefVar (name,e) ->
     sptf "%slet %s = %s" (indent_string lvl) name (sprint_exp (next lvl) e)
  | Exp (e) ->  sptf "%slet _ = %s" (indent_string lvl) (sprint_exp lvl e)
  | DefFun (dfs) ->
      sprint_fun lvl dfs
  | DefFunRec (dfs) -> sprint_fun ~recflag:true lvl dfs
and sprint_fun ?(recflag=false) lvl l =
    (indent_string lvl) ^ (if recflag then "let rec" else "let") ^
    String.concat ("\n" ^ indent_string lvl ^ "and") @@
    List.map (fun (name,args,e) ->
              let s = sptf " %s %s = " name (String.concat " " args) in
               s ^ "\n" ^ (indent_string (next lvl)) ^ (sprint_exp (next lvl) e)) l
and sprint_exp lvl = function
  | Constant(c) -> sprint_constant lvl c
  | Ident name -> name
  | Let(name,e1,e2) -> let w = sptf "(let %s = " name in
                       let z = get_indent_level w lvl in
                       sptf "%s%s in%s%s)" w
                         (sprint_exp (lvl + z) e1)
                         (indent_string (next lvl))
                         (sprint_exp (next lvl) e2)
  | BinOp(op,e1,e2) -> let lvl = lvl + 1 in (* pour la parenthèse ouvrante *)
                       let s = "(" ^ sprint_exp lvl e1 in
                       let lvl' = get_indent_level s lvl in
                       let ops = " " ^ sprint_binop lvl' op ^ " " in
                       let opz = get_indent_level ops lvl in
                       s ^ ops ^ (sprint_exp (lvl'+opz) e2) ^ ")"
  | UnOp(op,e1) -> let lvl = lvl + 1 in (* pour la parenthèse ouvrante *)
                   let s = "(" ^ sprint_unop lvl op ^ " " in
                   let lvl' = get_indent_level s lvl in
                   sptf "%s%s)" s (sprint_exp lvl' e1)
  | App(e,args) -> let s = sptf "(%s " (sprint_exp lvl e) in
                   let lvl' = get_indent_level s lvl in 
                     s ^ (mapcat " " (sprint_exp lvl') args) ^ ")"    (* à revoir, indentation pas terrible si plusieurs "gros" arguments *)
  | If(e1,e2,e3) -> let lvl = lvl + 1 in (* pour la parenthèse ouvrante *)
                    sptf "(if %s\n%sthen %s \n%selse %s)"
                      (sprint_exp lvl e1)
                      (indent_string lvl) (sprint_exp lvl e2)
                      (indent_string lvl) (sprint_exp lvl e3)
  | Ref(e) -> sptf "(ref %s)" (sprint_exp 0 e)
  | Ref_access(e1) -> sptf "(! %s)" (sprint_exp 0 e1)
  | Ref_assign(e1,e2) -> sptf "(%s := %s)" (sprint_exp 0 e1) (sprint_exp 0 e2)
  | Array_create(es) -> "[|" ^ mapcat "; " (sprint_exp 0) es ^ "|]"
  | Array_assign(e1,e2,e3) -> sptf "((%s).(%s) <- %s)" 
                                (sprint_exp lvl e1)
                                (sprint_exp (next lvl) e2)
                                (sprint_exp (next lvl) e3)
  | Array_access(e1,e2) -> sptf "%s.(%s)" (sprint_exp 0 e1) (sprint_exp 0 e2)
  | String s -> sptf "\"%s\"" s
  | Seq(e1,e2) ->
     sptf "(%s;\n%s%s)" (sprint_exp lvl e1) (indent_string lvl) (sprint_exp (lvl) e2)
  | While(e1,e2) -> sptf "while %s do\n%s\n%sdone"
                      (sprint_exp lvl e1) (sprint_exp (lvl+1) e2) (indent_string lvl)
  | For(x,e1,e2,e3) -> sptf "for %s = %s to %s do\n%s\n%sdone" x
                         (sprint_exp 0 e1) (sprint_exp lvl e2) (sprint_exp (next lvl) e3) 
                         (indent_string lvl)
  | Assert(e) -> let s = sptf "(assert " in
                 let lvl' = get_indent_level s lvl in 
                  s ^ (sprint_exp lvl' e) ^ ")"
  | Match(e1,cases) ->
     let lvl = lvl + 1 in (* pour la parenthèse ouvrante *)
     sptf "(match %s with\n%s"
       (sprint_exp 0 e1)
       (String.concat "\n"
          (List.map 
             (function 
              | Case (c,e) ->
                 let s = sptf "%s| %s -> "
                           (indent_string lvl) (sprint_constant lvl c) in
                 let lvl' = get_indent_level s lvl in
                 s ^ (sprint_exp lvl' e)
              | Otherwise (e) ->
                 let s = sptf "%s| _ -> "
                           (indent_string lvl) in
                 let lvl' = get_indent_level s lvl in
                 s ^ (sprint_exp lvl' e))
             cases)) ^ ")"
  | Array_alloc _ | SetGlobal _ | ReadGlobal _ -> 
    failwith "bug : AST interne (noeud temporaire pour la réécriture en Kast), ne devrait pas pouvoir être construit par le parseur, ni affiché ..."
and sprint_constant lvl = function
  | Unit -> sptf "()"
  | Bool b -> sptf "%b" b
  | Int n -> sptf "%d" n
  | Char c -> sptf "%c" c
  | Constr name -> name
  | Array_empty -> sptf "[||]"
and sprint_binop lvl = function
  | Add -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Eq -> "="
  | Neq -> "<>"
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="
  | Or -> "||"
  | And -> "&&"
  | Lor -> "lor"
  | Land -> "land"
and sprint_unop lvl = function
  | Not -> "not"
  | UMinus -> "-"  
and sprint_ty lvl = function
| Sum (names) -> String.concat " | " names
| Ident_ty (name) -> name
| Star_ty (lty) -> sptf "(%s)" (mapcat " * " (sprint_ty lvl) lty)
| Arrow_ty (t1,t2) -> sptf "(%s -> %s)" (sprint_ty lvl t1) (sprint_ty lvl t2)



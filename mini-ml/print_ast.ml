open Ast

let sptf = Printf.sprintf

let next n = n + 2

let indent_string (level:int) : string =
  String.make level ' '

let mapcat c f l = String.concat c (List.map f l)

let rec sprint_prog ms = List.map (sprint_module 0) ms
and sprint_module lvl {mod_name;decls} =
 sptf "%smodule %s = struct\n%s ;;\n%send" (indent_string lvl) mod_name 
    (mapcat " ;;\n\n" (sprint_decl (next lvl)) decls) (indent_string lvl) 
and sprint_decl lvl = function
| DefVar (name,e) -> sptf "%slet %s = %s" (indent_string lvl) name (sprint_exp (next lvl) e)
| DefFun (name,args,e) -> let s = sptf "%slet %s %s = " (indent_string lvl)  name (String.concat " " args) in
                          s ^ "\n" ^ (indent_string (next lvl)) ^ (sprint_exp (next lvl) e)
| Exp (e) ->  sptf "let () = %s" (sprint_exp lvl e)
and sprint_exp lvl = function
| Constant(c) -> sprint_constant lvl c
| Ident name -> name
| Let(name,e1,e2) -> let w = sptf "(let %s = " name in
                     let z = String.length w in
                        sptf "%s%s in\n%s%s)" w
                        (sprint_exp (lvl + z) e1)
                        (indent_string (next lvl))
                        (sprint_exp (next lvl) e2)
| BinOp(op,e1,e2) -> let s = "(" ^ sprint_exp lvl e1 in
                     let lvl' = match String.rindex_opt s '\n' with
                             | None -> lvl + String.length s
                             | Some i -> String.length s - i in
                     let ops = " " ^ sprint_binop lvl' op ^ " " in
                     let opz = String.length ops in
                     s ^ ops ^ (sprint_exp (lvl'+opz) e2) ^ ")"
| UnOp(op,e1) -> let s = "(" ^ sprint_unop lvl op ^ " " in
                 let z = String.length s in
                 sptf "%s%s)" s (sprint_exp (lvl+z+1) e1)
| App(e,args) -> sptf "(%s %s)"
               (sprint_exp lvl e)
               (mapcat " " (sprint_exp ((next lvl)+1)) args) 
| If(e1,e2,e3) -> sptf "(if %s\n%sthen %s \n%selse %s)"
                     (sprint_exp lvl e1)
                     (indent_string (lvl+1)) (sprint_exp (lvl+1) e2)
                     (indent_string (lvl+1)) (sprint_exp (lvl+1) e3)
(* | Match of (exp * (patern * exp) list)*)
(* | Op of (op * exp list)*)
| Ref(e) -> sptf "(ref %s)" (sprint_exp 0 e)
(*| Bloc_alloc of (exp)*)

| Ref_access(e1) -> sptf "(! %s)" (sprint_exp 0 e1)
| Ref_assign(e1,e2) -> sptf "(%s := %s)" (sprint_exp 0 e1) (sprint_exp 0 e2)
| Array_create(es) -> "[|" ^ mapcat "; " (sprint_exp 0) es ^ "|]"
| Array_assign(e1,e2,e3) -> sptf "((%s).(%s) <- %s)" 
                     (sprint_exp lvl e1)
                    (sprint_exp (next lvl) e2)
                    (sprint_exp (next lvl) e3)
| Array_access(e1,e2) -> sptf "%s.(%s)" (sprint_exp 0 e1) (sprint_exp 0 e2)
| String s -> sptf "\"%s\"" s
| Seq(e1,e2) -> sptf "(%s;\n%s%s)" (sprint_exp lvl e1) (indent_string lvl) (sprint_exp (lvl) e2)
| While(e1,e2) -> sptf "while %s do\n%s\n%sdone"
               (sprint_exp lvl e1) (sprint_exp (lvl+1) e2) (indent_string lvl)
| For(x,e1,e2,e3) -> sptf "for %s = %s to %s do\n%s\n%sdone" x
               (sprint_exp 0 e1) (sprint_exp lvl e2) (sprint_exp (next lvl) e3) 
               (indent_string lvl)
| Assert(e) -> sptf "(assert %s)" (sprint_exp lvl e)
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
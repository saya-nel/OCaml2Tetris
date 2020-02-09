open Utils

type pos = Parseutils.pos

type name = string

type prog = decl list
and decl = Decl of (name * name list * expr * pos) 
         | RecDecl of (name * name list * expr * pos)
         | Type of (name * ty * pos)
and ty = Sum of (sum_type)
and sum_type = (constructor list)
and constructor = name
and expr = 
  | Constant of (constant * pos)
  | Ident of (name * pos)
  | Let of (name * expr * expr * pos)
  | App of (expr * expr list * pos)
  | Seq of (expr list * pos)
  | BinOp of (name * expr * expr * pos)
  | UnOp of (name * expr * pos)
  | If of (expr * expr * expr * pos)
  | Match of (expr * match_case list * pos)
  | While of (expr * expr * pos)
  | For of (name * expr * expr * pos)
  | Array_create of (expr list * pos)
  | Array_get of (expr * expr * pos)
  | Array_assign of (expr * expr * expr * pos)
  | Ref of (expr * pos)
  | Access of (expr * pos)
  | Assign of (expr * expr * pos)
and constant =
  | Unit
  | Int of (int)
  | Bool of (bool)
  | String of (string)
  | Constructor of (string)
and match_case =
  | Case of (constant * expr * pos)
  | Otherwise of (expr * pos)

let indent_factor = 2

let indent_string (level:int) : string =
  String.make (level * indent_factor) ' '



(** impression de l'AST en syntaxe Caml ***********)

(* [string_of_prog p] imprime le programme p *)
let rec string_of_prog (p : prog) : string = 
  (string_of_defuns 0 p)
  
and string_of_defuns (lvl : int) (dfs : decl list) : string =
  String.concat "\n" (List.map (string_of_defun lvl) dfs)
  
and string_of_defun (lvl : int) (df : decl) : string = match df with
  | (Decl (f,args,e,_)) | (RecDecl (f,args,e,_)) ->
     sptf "%slet %s %s =\n%s\n"
       (indent_string lvl)
       (string_of_name f)
       (string_of_bindings args) 
       (string_of_expr (lvl+1) e)
  | Type(t,d,_) -> sptf "type %s = %s\n" t
                     (match d with Sum(l) -> (String.concat " | " l))

and string_of_bindings l = 
  String.concat " " (List.map string_of_name l)

and string_of_name n = n
                     
and string_of_constant lvl = function
  | Unit -> sptf "%s()" (indent_string lvl)
  | Int(n) -> sptf "%s%d" (indent_string lvl) n
  | Bool(b) -> sptf "%s%s" (indent_string lvl) (if b then "true" else "false")
  | String(s) -> sptf "%s\"%s\"" (indent_string lvl) s
  | Constructor(s) -> sptf "%s%s" (indent_string lvl) s
                    
and string_of_expr (lvl : int) (e : expr) : string =
  match e with
  | Constant(c,_) -> string_of_constant lvl c
  | Ident (x,_) -> sptf "%s%s"
                     (indent_string lvl)
                     (string_of_name x)
  | Let (x,e1,e2,_) -> sptf "%slet %s = %s in\n%s" 
                         (indent_string lvl) (string_of_name x)  
                         (string_of_expr 0 e1)
                         (string_of_expr (lvl+1) e2)
  | App(f,args,_) -> sptf "%s((%s) %s)"
                       (indent_string lvl) 
                       (string_of_expr 0 f)
                       (String.concat " " (List.map (string_of_expr 0) args))
  | Seq(l,_) -> sptf "%sbegin\n%s\n%send"
                  (indent_string lvl)
                  (String.concat ";\n" (List.map (string_of_expr (lvl+1)) l))
                  (indent_string lvl)
  | BinOp(op,e1,e2,_) -> sptf "%s((%s)%s(%s))"
                           (indent_string lvl)
                           (string_of_expr 0 e1) 
                           (string_of_name op)
                           (string_of_expr 0 e2)
  |UnOp(op,e,_) -> sptf "%s(%s(%s))"
                     (indent_string lvl)
                     (string_of_name op)
                     (string_of_expr 0 e)
  | If(e1,e2,e3,_) -> let indentation = (indent_string lvl) in 
                      sptf "%sif %s\n%sthen %s\n%selse %s"
                        indentation
                        (string_of_expr 0 e1)
                        indentation
                        (string_of_expr 0 e2)
                        indentation
                        (string_of_expr 0 e3)
  | Match(e1,cases,_) ->
     let indentation = (indent_string lvl) in 
     sptf "%smatch %s with\n%s"
       indentation
       (string_of_expr 0 e1)
       (String.concat "\n" (List.map 
                              (function 
                               | Case (c,e,_) ->
                                  sptf "%s| %s -> %s"
                                    indentation
                                    (string_of_constant 0 c) (string_of_expr 0 e)
                               | Otherwise (e,_) ->
                                  sptf "%s| _ -> %s"
                                    indentation
                                    (string_of_expr 0 e))
                              cases)) 
  | While(e1,e2,_) -> let indentation = (indent_string lvl) in 
                      sptf "%swhile %s do\n%s\n%sdone"
                        indentation
                        (string_of_expr 0 e1)
                        (string_of_expr (lvl+1) e2)
                        indentation
  | For(x,e1,e2,_) -> let indentation = (indent_string lvl) in 
                      sptf "%sfor %s in %s do\n%s\n%sdone"
                        indentation
                        (string_of_name x)
                        (string_of_expr 0 e1)
                        (string_of_expr (lvl+1) e2)
                        indentation 
  | Array_create(l,_) -> sptf "%s[|%s|]"
                           (indent_string lvl)
                           (String.concat "; " (List.map (string_of_expr 0) l))
  | Array_get (e1,e2,_) -> sptf "%s(%s).(%s)"
                             (indent_string lvl)
                             (string_of_expr 0 e1)
                             (string_of_expr 0 e2)
  | Array_assign (e1,e2,e3,_) -> sptf "%s(%s).(%s) <- %s"
                                   (indent_string lvl) 
                                   (string_of_expr 0 e1)
                                   (string_of_expr 0 e2)
                                   (string_of_expr 0 e3) 
  | Ref (e1,_) -> sptf "%s(ref %s)"
                    (indent_string lvl)
                    (string_of_expr 0 e1)
  | Access (e1,_) -> sptf "%s(! %s)"
                       (indent_string lvl)
                       (string_of_expr 0 e1)
  | Assign (x,e1,_) -> sptf "%s(%s := %s)"
                         (indent_string lvl)
                         (string_of_expr 0 x)
                         (string_of_expr 0 e1)

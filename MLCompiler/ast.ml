open Utils

type pos = Parseutils.pos

type name = string

type prog = decl list
and decl = Decl of (name * name list * expr * pos) 
         | RecDecl of (name * name list * expr * pos)
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
and match_case = Case of (constant * expr * pos) | Otherwise of (expr * pos)

let indent_factor = 2

let indent_string (level:int) : string =
  String.make (level * indent_factor) ' '

let rec string_of_prog l = 
	(string_of_defuns 0 l)
and string_of_defuns lvl dfs =
	String.concat "\n" (List.map (string_of_defun lvl) dfs)
and string_of_defun lvl = function 
| (Decl (f,args,e,_)) | (RecDecl (f,args,e,_)) ->
	sptf "%slet %s %s =\n%s\n" (indent_string lvl) (string_of_name f) (string_of_bindings args) 
 (string_of_expr (lvl+1) e)
and string_of_bindings l = 
	String.concat " " (List.map string_of_name l)
and string_of_name n = n
and string_of_constant lvl = function
| Unit -> sptf "%s()" (indent_string lvl)
| Int(n) -> sptf "%s%d" (indent_string lvl) n
| Bool(b) -> sptf "%s%s" (indent_string lvl) (if b then "true" else "false")
| String(s) -> sptf "%s\"%s\"" (indent_string lvl) s
and string_of_expr lvl = function
| Constant(c,_) -> string_of_constant lvl c
| Ident (x,_) -> sptf "%s%s" (indent_string lvl) (string_of_name x)
| Let (x,e1,e2,_) -> sptf "%slet %s = %s in\n%s" 
                             (indent_string lvl) (string_of_name x)  
                             (string_of_expr 0 e1)
                             (string_of_expr (lvl+1) e2)
| App(f,args,_) -> sptf "%s((%s) %s)" (indent_string lvl) 
               (string_of_expr 0 f) (String.concat " " (List.map (string_of_expr 0) args))
| Seq(l,_) -> (String.concat ";\n" (List.map (string_of_expr lvl) l))
| BinOp(op,e1,e2,_) -> sptf "%s((%s)%s(%s))" (indent_string lvl)  (string_of_expr 0 e1) 
                    (string_of_name op) (string_of_expr 0 e2)
|UnOp(op,e,_) -> sptf "%s(%s(%s))" (indent_string lvl) (string_of_name op) (string_of_expr 0 e)
| If(e1,e2,e3,_) -> let k = (indent_string lvl) in 
                    sptf "%sif %s\n%sthen %s\n%selse %s" k (string_of_expr 0 e1)
                    k (string_of_expr 0 e2) k (string_of_expr 0 e3)
| Match(e1,cases,_) -> let k = (indent_string lvl) in 
                       sptf "%smatch %s with\n%s" k (string_of_expr 0 e1)
                       (String.concat "\n" (List.map 
                         (function 
                           | Case (c,e,_) -> sptf "%s| %s -> %s" k (string_of_constant 0 c) (string_of_expr 0 e)
                           |Otherwise (e,_) -> sptf "%s| _ -> %s" k (string_of_expr 0 e)) cases))

| While(e1,e2,_) -> let k = (indent_string lvl) in 
                    sptf "%swhile %s do\n%s\n%sdone" k (string_of_expr 0 e1)
                    (string_of_expr (lvl+1) e2) k 
| For(x,e1,e2,_) -> let k = (indent_string lvl) in 
                    sptf "%sfor %s in %s do\n%s\n%sdone" k (string_of_name x) (string_of_expr 0 e1)
                    (string_of_expr (lvl+1) e2) k 
| Array_create(l,_) -> sptf "%s[|%s|]" (indent_string lvl) (String.concat "; " (List.map (string_of_expr 0) l))
| Array_get (e1,e2,_) -> sptf "%s(%s).(%s)" (indent_string lvl) (string_of_expr 0 e1) (string_of_expr 0 e2)
| Array_assign (e1,e2,e3,_) -> sptf "%s(%s).(%s) <- %s" (indent_string lvl) 
                              (string_of_expr 0 e1) (string_of_expr 0 e2) (string_of_expr 0 e3) 
| Ref (e1,_) -> sptf "%s(ref %s)" (indent_string lvl) (string_of_expr 0 e1)
| Access (e1,_) -> sptf "%s(! %s)" (indent_string lvl) (string_of_expr 0 e1)
| Assign (x,e1,_) -> sptf "%s(%s := %s)" (indent_string lvl) (string_of_expr 0 x) (string_of_expr 0 e1)
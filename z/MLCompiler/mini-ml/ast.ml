open Utils

type prog = decl list
and decl = name * exp
and exp = 
  | Val of value
  | Var of name
  | Lam of name * exp
  | App of exp * exp
  | If of exp * exp * exp
  | While of exp * exp
  | Let of (name * exp) * exp
  | Rec of (name * exp) * exp

let rec string_of_ast ast =
  prtf "let rec %s" (mapcat "\nand " string_of_decl ast)
and string_of_decl (name,e) =
  prtf "%s = %s" name (string_of_exp e)
and string_of_exp e =
  let rec aux lvl = function
    | Val n -> string_of_int n
    | Var v -> v
    | Lam (name,e) -> prtf "(fun %s -> %s)" name (aux (lvl+1) e)
    | App (e1,e2) -> prtf "(%s %s)" (aux lvl e1) (aux 0 e2)
    | If (e1,e2,e3) -> prtf "if %s then %s else %s"
                         (aux lvl e1) (aux 0 e2) (aux 0 e3)
    | While (e1,e2) -> prtf "while %s do %s done" (aux 0 e1) (aux 0 e2)
    | Let ((name,e1),e2) -> prtf "let %s = %s in %s"
                              name (aux lvl e1) (aux 0 e2)
    | Rec ((name,e1),e2) -> prtf "let rec %s = %s in %s"
                              name (aux lvl e1) (aux 0 e2) in aux 0 e
                                                                

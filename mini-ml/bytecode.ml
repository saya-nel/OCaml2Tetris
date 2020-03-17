
type bcmodule = { mod_name : Ast.name ; bc_decls:instr list ;init: Ast.name list}
and instrs = instr list          
and instr =
  | Comment of string
  | Push of segment
  | Pop of segment
  | Label of label
  | IfGoto of label
  | Goto of label
  | Return
  | Function of fun_name * int (* nb locales *)
  | Call of fun_name * int (* arité *)
  | Op of vm_operator
and segment =
  | Anywhere
  | Argument of int
  | Constant of int
  | Static of int
  | Local of int
  | That of int
  | Temp of int
  | Pointer of int
             
and label = string
and fun_name = string

and vm_operator = Add | Sub | Mult | Div | Eq | Gt | Lt | And | Or | Not | Access | Assign | Alloc
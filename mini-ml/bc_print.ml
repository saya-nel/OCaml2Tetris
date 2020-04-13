open Bc

let sptf = Printf.sprintf

let prefix = "ML_"

let indent = "    "

let rec string_of_instrs b =
  String.concat "\n" (List.map string_of_instr b)      
and string_of_instr = function
  | Comment s ->
     indent ^ "// " ^ s
  | Push pt ->
     indent ^ "push " ^ string_of_segment pt
  | Pop pt ->
     indent ^ "pop " ^ string_of_segment pt
  | Label k ->
     sptf "label %s" k
  | IfGoto k ->
     indent ^ sptf "if-goto %s" k
  | Goto k ->
     indent ^ sptf "goto %s" k
  | Return ->
     indent ^ "return"
  | Function (f,n) ->
     sptf "// val \"%s\"\n\
           function %s %d" f (prefix ^ f) n
  | Call (f,n) -> indent ^ sptf "call %s %d" (prefix ^ f) n
  | BinOp s -> string_of_binop s
  | UnOp s -> string_of_unop s
  | True ->
     sptf "%spush constant 0\n%snot" indent indent
  | False -> indent ^ "push constant 0"
and string_of_segment = function
  | Anywhere ->
     string_of_segment (Temp 0)
  | Argument n -> 
    sptf "argument %d" n
  | Constant n -> (if n < 0 then sptf (* push *) "constant 0\n    push constant %d\n    sub" (- n) else
     sptf "constant %d" n)
  | Static n ->
     sptf "static %d" n
  | Local n ->
     sptf "local %d" n
  | That n ->
     sptf "that %d" n
  | Temp n ->
     sptf "temp %d" n
  | Pointer n ->
     sptf "pointer %d" n
and string_of_binop = function
  | Add ->
     indent ^ "add"
  | Sub ->
     indent ^ "sub"
  | Eq ->
     indent ^ "eq"
  | Gt ->
     indent ^ "gt"
  | Lt ->
     indent ^ "lt"
  | And ->
     indent ^  "and"
  | Or ->
     indent ^  "or"
  | Mult -> "call Math.multiply 2"
  | Div -> "call Math.divide 2"
  | Assign -> "call Memory.poke 2"
and string_of_unop = function
  | Not ->
     indent ^  "not"

  | Access -> "call Memory.peek 1"
  | Alloc -> "call Memory.alloc 1"

               (*
     string_of_instrs [Call ("Math.multiply",2)]
  | Div ->
     string_of_instrs [Call ("Math.divide",2)]
  | Access ->
     string_of_instrs [Call ("Memory.peek",1)]
  | Assign ->
     string_of_instrs [Call ("Memory.poke",2)]
  | Alloc ->
     string_of_instrs [Call ("Memory.alloc",1)] *)

open Bytecode

let sptf = Printf.sprintf

let indent = "    "

let rec string_of_instrs b =
  String.concat "\n" (List.map string_of_instr b)      
and string_of_instr = function
  | Comment s -> indent ^ "// " ^ s
  | Push pt -> indent ^ "push " ^ string_of_segment pt
  | Pop pt -> indent ^ "pop " ^ string_of_segment pt
  | Label k -> sptf "label %s" k
  | IfGoto k -> indent ^ sptf "if-goto %s" k
  | Goto k -> indent ^ sptf "goto %s" k
  | Return -> indent ^ "return"
  | Function (f,n) -> sptf "///////////////////////////////////////////////////////////////\n\
                           function %s %d" f n
  | Call (f,n) -> indent ^ sptf "call %s %d" f n
  | Op s -> string_of_op s
and string_of_segment = function
  | Anywhere -> string_of_segment (Temp 0)
  | Argument n -> sptf "argument %d" n
  | Constant n -> sptf "constant %d" n
  | Static n -> sptf "static %d" n
  | Local n -> sptf "local %d" n
  | That n -> sptf "that %d" n
  | Temp n -> sptf "temp %d" n
  | Pointer n -> sptf "pointer %d" n
and string_of_op = function
| Add -> indent ^ "add"
| Sub -> indent ^ "sub"
| Eq -> indent ^ "eq"
| Gt -> indent ^ "gt"
| Lt -> indent ^ "lt"
| And -> indent ^  "and"
| Or -> indent ^  "or"
| Not -> indent ^  "not"
| Mult -> string_of_instrs [Call ("Math.multiply",2)]
| Div -> string_of_instrs [Call ("Math.divide",2)]
| Access -> string_of_instrs [Call ("Memory.peek",1)]
| Assign -> string_of_instrs [Call ("Memory.poke",2)]
| Alloc -> string_of_instrs [Call ("Memory.alloc",1)]
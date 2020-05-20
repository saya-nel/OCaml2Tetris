
(* alloc définitions *)

let heap_size = ref 1000

let from_space = ref (Array.make !heap_size 0)
let to_space = ref (Array.make !heap_size 0)
let heap_top = ref 0

let global_size = 10
let global = Array.make (global_size) 0

(* registres de interp *)
let stack_size = 1024
let sp = ref 0
let stack = Array.make stack_size (Mlvalues.val_long 0)

let acc = ref (Mlvalues.val_long 0)
let env = ref (Mlvalues.val_long 0)

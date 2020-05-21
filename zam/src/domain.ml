
(* alloc définitions *)

let heap_size = ref 100

let from_space = ref (Array.make !heap_size (Mlvalues.val_long 0))
let to_space = ref (Array.make !heap_size (Mlvalues.val_long 0))
let heap_top = ref 0

let global_size = 10
let global = Array.make (global_size) (Mlvalues.val_long 0)

(* registres de interp *)
let stack_size = 102
let sp = ref 0
let stack = Array.make stack_size (Mlvalues.val_long 0)

let acc = ref (Mlvalues.val_long 0)
let env = ref (Mlvalues.val_long 0)

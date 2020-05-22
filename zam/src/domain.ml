
(* alloc définitions *)

let heap_size = ref 1000

let data_size = 100
let data_top = ref 0
let data = Array.make data_size (Mlvalues.val_long 0)

let global_start = data_size
let global_size = 100
let global_top = ref 0
let global = Array.make global_size (Mlvalues.val_long 0)


let heap_start = global_start+global_size
let from_space = ref (Array.make !heap_size (Mlvalues.val_long 0))
let to_space = ref (Array.make !heap_size (Mlvalues.val_long 0))
let heap_top = ref heap_start

(* registres de interp *)
let stack_size = 2048
let sp = ref 0
let stack = Array.make stack_size (Mlvalues.val_long 0)

let acc = ref (Mlvalues.val_long 0)
let env = ref (Mlvalues.val_long 0)

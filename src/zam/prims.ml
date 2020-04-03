


let negint v = Mlvalues.val_long (- Mlvalues.long_val v)
let addint v1 v2 = Mlvalues.val_long (Mlvalues.long_val v1 + Mlvalues.long_val v2)
let subint v1 v2 = Mlvalues.val_long (Mlvalues.long_val v1 - Mlvalues.long_val v2)
let mulint v1 v2 = Mlvalues.val_long (Mlvalues.long_val v1 * Mlvalues.long_val v2)
let divint v1 v2 = Mlvalues.val_long (Mlvalues.long_val v1 / Mlvalues.long_val v2)
let modint v1 v2 = failwith "todo"
let andint v1 v2 = Mlvalues.val_long (Mlvalues.long_val v1 && Mlvalues.long_val v2)
let orint v1 v2 = Mlvalues.val_long (Mlvalues.long_val v1 || Mlvalues.long_val v2)
let xorint v1 v2 = failwith "todo"
let lslint v1 v2 = failwith "todo"
let lsrint v1 v2 = failwith "todo"
let asrint v1 v2 = failwith "todo"
let eq v1 v2 = Mlvalues.val_long (if Mlvalues.long_val v1 = Mlvalues.long_val v2 then 0 else 1)
let neq v1 v2 = Mlvalues.val_long (if Mlvalues.long_val v1 <> Mlvalues.long_val v2 then 0 else 1)
let ltint v1 v2 = Mlvalues.val_long (Mlvalues.long_val v1 < Mlvalues.long_val v2)
let leint v1 v2 = Mlvalues.val_long (Mlvalues.long_val v1 <= Mlvalues.long_val v2)
let gtint v1 v2 = Mlvalues.val_long (Mlvalues.long_val v1 > Mlvalues.long_val v2)
let geint v1 v2 = Mlvalues.val_long (Mlvalues.long_val v1 >= Mlvalues.long_val v2)
let isint v = Mlvalues.val_long (if Mlvalues.is_imm v then 1 else 0) 

let compare_imm v1 v2 = 
	let n1 = Mlvalues.long_val v1 in
	let n2 = Mlvalues.long_val v2 in
	if n1 < n2 then -1 else if n1 > n2 then 1 else 0

let ultint v1 v2 = failwith "todo"
let ugeint v1 v2 = failwith "todo"

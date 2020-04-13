
let negint (v : value) : value = 
  Mlvalues.val_long (- Mlvalues.long_val v)

let sign_extension n =
	if n > 0x3FFF (* ie. n entier 15 bits négatif *)
	then - (n land 0x3FFF) 
    else n

let sign_contraction n = 
	if n < 0 (* ie. n entier 16 bits négatif *)
	then (- n) lor 0x4000 (* force à 1 le bit de signe de l'entier 15 bits *)
    else n

let addint (v1 : value) (v2 : value) : value = 
	Mlvalues.val_long @@ sign_contraction @@
	(sign_extension (Mlvalues.long_val v1) + sign_extension (Mlvalues.long_val v2))

let subint v1 v2 = 
	Mlvalues.val_long @@ sign_contraction @@
	(sign_extension (Mlvalues.long_val v1) - sign_extension (Mlvalues.long_val v2))

let mulint v1 v2 = 
	Mlvalues.val_long @@ sign_contraction @@
	(sign_extension (Mlvalues.long_val v1) * sign_extension (Mlvalues.long_val v2))

let divint v1 v2 =  (* a revoir, marche uniquement avec des positfs, d'après la doc nand2tetris *)
	Mlvalues.val_long @@ sign_contraction @@
	(sign_extension (Mlvalues.long_val v1) / sign_extension (Mlvalues.long_val v2))

let modint v1 v2 = 
	failwith "todo"

let andint v1 v2 = 
	Mlvalues.val_long 
	(if Mlvalues.long_val v1 <> 0 && Mlvalues.long_val v2 <> 0 then 1 else 0)

let orint v1 v2 = 
  Mlvalues.val_long
  (if Mlvalues.long_val v1 <> 0 then 1 else 
   if Mlvalues.long_val v2 <> 0 then 1 else 0)

let xorint v1 v2 = failwith "todo"
let lslint v1 v2 = failwith "todo"
let lsrint v1 v2 = failwith "todo"
let asrint v1 v2 = failwith "todo"

let eq (v1 : value) (v2 : value) : value = 
	Mlvalues.val_long (
	  if Mlvalues.long_val v1 = Mlvalues.long_val v2 then 0 else 1)

let neq (v1 : value) (v2 : value) : value = 
	Mlvalues.val_long (if Mlvalues.long_val v1 <> Mlvalues.long_val v2 then 0 else 1)

let ltint v1 v2 = 
	Mlvalues.val_long (if Mlvalues.long_val v1 < Mlvalues.long_val v2 then 1 else 0)

let leint v1 v2 = 
	Mlvalues.val_long (if Mlvalues.long_val v1 <= Mlvalues.long_val v2 then 1 else 0)

let gtint v1 v2 = 
	Mlvalues.val_long (if Mlvalues.long_val v1 > Mlvalues.long_val v2 then 1 else 0)

let geint v1 v2 = 
	Mlvalues.val_long (if Mlvalues.long_val v1 >= Mlvalues.long_val v2 then 1 else 0)
let isint v = 
	Mlvalues.val_long (if Mlvalues.is_ptr v then 0 else 1) 

let bnot v = 
	Mlvalues.val_long (if Mlvalues.long_val v = 0 then 1 else 0)

let compare_imm v1 v2 = 
	let n1 = Mlvalues.long_val v1 in
	let n2 = Mlvalues.long_val v2 in
	if n1 < n2 then -1 else if n1 > n2 then 1 else 0

let ultint v1 v2 = failwith "todo"
let ugeint v1 v2 = failwith "todo"



let _ = assert (sign_contraction 42 = 42);
        assert (sign_contraction (- 42) = 16426)

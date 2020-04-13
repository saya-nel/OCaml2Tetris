
(* hypothèse : les valeurs entières sont encodées par leurs inverses (voir mlvalue) *)

let negint (v : value) : value = -v
	(*  val_long(-long_val(v)) *)
	(*  ~> -(-(-v))            *)
	(*  ~> -v                  *)


let addint (v1 : value) (v2 : value) : value = v1 + v2 
	(*  val_long(long_val(v1) + long_val(v2)) *)
	(*  ~> -((-v1) + (-v2))                   *)
	(*  ~> v1 + v2                            *)
	

let subint v1 v2 = v1 - v2
	(*  val_long(long_val(v1) + long_val(v2)) *)
	(*  ~> -((-v1) - (-v2))                   *)
	(*  ~> -((-v1) + v2)                      *)
	(*  ~> v1 - v2)                           *)
	
let mulint v1 v2 = - (v1 * v2)
	(*  val_long(long_val(v1) + long_val(v2)) *)
	(*  ~> -((-v1) * (-v2))                   *)
	(*  ~> -(v1 * v2)                      *)
	

let divint v1 v2 = - (v1 / v2)
  (* à revoir, marche uniquement avec des positfs, d'après la doc nand2tetris *)
	 

let modint v1 v2 = 
	failwith "todo"

let andint v1 v2 = 
	if v1 <> 0 && v2 <> 0 then -1 else 0

let orint v1 v2 = 
	if v1 <> 0 || v2 <> 0 then -1 else 0

let xorint v1 v2 = 
	if (v1 <> 0 && v2 = 0) || (v1 <> 0 && v2 = 0) then -1 else 0

let rec shift_left n dep = 
	if dep = 0 then n
    else shift_left (n + n) (dep-1)

let lslint v1 v2 = 
    - shift_left (-v1) (-v2)
	(*  val_long(shift_left (long_val v1) (long_val v2)) *)
	(*  - (shift_left (-v1) (-v2))                       *)


let lsrint v1 v2 = failwith "todo"
let asrint v1 v2 = failwith "todo"

let eq (v1 : value) (v2 : value) : value = (* égalité physique *)
	if v1 = v2 then -1 else 0

let neq (v1 : value) (v2 : value) : value = 
	if v1 <> v2 then -1 else 0

let ltint v1 v2 = 
	if v1 > v2 then -1 else 0
	(*  long_val(v1) < long_val(v2) *)
	(*  ~> (-v1) < (-v2))           *)
	(*  ~>   v1 > v2                *)

let leint v1 v2 = 
	if v1 >= v2 then -1 else 0

let gtint v1 v2 = 
	if v1 < v2 then -1 else 0

let geint v1 v2 = 
	if v1 >= v2 then -1 else 0

let isint v = 
	if Mlvalues.is_ptr v then (-1) else 0

let bnot v = 
	if v = 0 then (-1) else 0

let compare_imm v1 v2 =
	(*  val_long(compare (long_val(v1)) (long_val(v2))) *)
	(*  -(compare (-v1) (-v2))                          *)	
	(*   (compare v1 v2)                                *)
	if v1 < v2 then -1 else if v1 > v2 then 1 else 0

let ultint v1 v2 = failwith "todo"
let ugeint v1 v2 = failwith "todo"

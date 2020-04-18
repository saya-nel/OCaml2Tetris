(************** module prims.ml ****************)
(*** hypothèses :                            ***)
(***  - `(long_val n) = (val_long n)`.       ***)
(***  toute valeur >= (-16384) est un entier ***)
(***********************************************)

let isint = function
| Mlvalues.I _ -> Mlvalues.val_long 1
| _ -> Mlvalues.val_long 0

(*** opérations arithmétiques ***)

let negint (v : Mlvalues.value) : Mlvalues.value = 
	match v with 
	| Mlvalues.I n -> Mlvalues.I (-n) 
	| _ -> assert false

let addint (v1 : Mlvalues.value) (v2 : Mlvalues.value) : Mlvalues.value = 
	match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (n1+n2)
	| _ -> assert false

let subint v1 v2 = 
     match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (n1-n2)
	| _ -> assert false

let mulint v1 v2 =
	match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (n1*n2)
	| _ -> assert false

(*** division d'après la doc OCaml : (-x) / y = x / (-y) = -(x / y). ***)
(*** (/) 17 2       ~>  8  *)
(*** (/) 17 (-2)    ~> -8  *)
(*** (/) (-17) 2    ~> -8  *)
(*** (/) (-17) (-2) ~>  8  *)

let rec div_aux v1 v2 acc = 
  if v1 < v2 then acc
  else div_aux (v1 - v2) v2 (acc + 1)

let div v1 v2 = div_aux v1 v2 0

let rout_div v1 v2 =
	if v2 = 0 then failwith "divint" else
	if v1 >= 0 then (if v2 >= 0 then div v1 v2 else - (div v1 (- v2)))
	else (if v2 >= 0 then - (div (- v1) v2) else (div (- v1) (- v2)))

let divint v1 v2= 	
    match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (rout_div n1 n2)
	| _ -> assert false

(*** modulo d'après la doc OCaml : `((mod) x y) < 0` si et seulement si `x < 0` ***)
(*** (mod) 11 3        ~>  2 *)
(*** (mod) 11 (-3)     ~>  2 *)
(*** (mod) (-11) 3     ~> -2 *)
(*** (mod) (-11) (-3)  ~> -2 *)

let rec modulo v1 v2 =
  if v1 < v2 then v1
  else modulo (v1 - v2) v2

let rec rout_mod v1 v2 = (* dans un premier temps, on suppose que v2 >= 0 ***)
  if v2 = 0 then failwith "modint" else
  if v1 < 0 then - (modulo (- v1) (abs v2)) else (modulo v1 (abs v2))

let modint v1 v2 =  
   match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (rout_mod n1 n2)
	| _ -> assert false


(*** opérations logiques ***)

let andint v1 v2 = 
	match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (if n1 <> 0 && n2 <> 0 then 1 else 0)
	| _ -> assert false
	

let orint v1 v2 = 
	match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (if n1 <> 0 || n2 <> 0 then 1 else 0)
	| _ -> assert false
	

let xorint v1 v2 = 
	match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (if (n1 <> 0 && n2 = 0) || (n1 <> 0 && n2 = 0) then 1 else 0)
	| _ -> assert false
	

let bnot v =
	match v with 
	| Mlvalues.I n -> Mlvalues.I (if n = 0 then 1 else 0) 
	| _ -> assert false 
	

(*** opérations de décalage ***)
(*** dans un premier temps, on suppose que le déplacement est toujours >= 0 ***)
(*** dans un premier temps, on ne fait pas de distinction entre lsr et asr  ***)
let rec lslint_aux n dep =
	if dep = 0 then n
    else lslint_aux (n + n) (dep - 1)
  

let rec lslint v dep =
	match v,dep with 
	| Mlvalues.I n,Mlvalues.I d -> Mlvalues.I (lslint_aux n d)
	| _ -> assert false 
  

let rec lsrint_aux n dep =
	if dep = 0 then n
    else lsrint_aux (n / 2) (dep - 1)
  

let rec lsrint v dep =
match v,dep with 
	| Mlvalues.I n,Mlvalues.I d -> Mlvalues.I (lsrint_aux n d)
	| _ -> assert false 

let asrint v1 v2 = lsrint v1 v2 

(*** opérations de comparaison ***)

(*** égalité physique ***)
let eq v1 v2 = 
	match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (if n1 = n2 then 1 else 0)
	| _ -> assert false


(*** différence physique ***)
let neq v1 v2 = 
		match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (if n1 <> n2 then 1 else 0)
	| _ -> assert false
	

let ltint v1 v2 = 
			match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (	if n1 < n2 then 1 else 0)
	| _ -> assert false


let leint v1 v2 = 
				match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (if n1 <= n2 then 1 else 0)
	| _ -> assert false
	

let gtint v1 v2 = 
					match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (if n1 < n2 then 1 else 0)
	| _ -> assert false
	
	

let geint v1 v2 = 
					match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  Mlvalues.I (if n1 >= n2 then 1 else 0)
	| _ -> assert false
	

	

let compare_imm v1 v2 : int =
	match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  if n1 < n2 then -1 else if n1 > n2 then 1 else 0
	| _ -> assert false
	

  

(*** comparaison (<) non signée               ***)
(*** v1 < 0 && v2 >= 0 => (ultint v1 v2) ~> 0 ***)
let ultint v1 v2 =
   match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  (if n1 < 0 then (if n2 < 0 then gtint v1 v2 else Mlvalues.I (0))
	else if n2 < 0 then Mlvalues.I (0) else ltint v1 v2)
	| _ -> assert false
	

(*** comparaison (>=) non signée              ***)
(*** v1 < 0 && v2 >= 0 => (ugeint v1 v2) ~> 1 ***)
let ugeint v1 v2 =
			match v1,v2 with
	| Mlvalues.I n1,  Mlvalues.I n2 ->  (if n1 < 0 then (if n2 < 0 then leint v1 v2 else Mlvalues.I (1))  
	else if n2 < 0 then Mlvalues.I (1) else geint v1 v2)
	| _ -> assert false
	


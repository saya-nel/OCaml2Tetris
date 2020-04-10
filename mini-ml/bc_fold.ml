(* attention, la propagation des constantes devrait respecter l'architecture (addition 16 bit etc.) *)

open Bc

let rec rewrite bcm = 
  {bcm with bc_body=rw (bcm.bc_body)}
and rw = function
| [] -> []
| Push(Constant n) :: Push(Constant m) :: BinOp(Add) :: bc -> 
  rw @@ Push (Constant ((n + m) mod 0x8FFF)) :: bc
| Push(Constant n) :: Push(Constant m) :: BinOp(Sub) :: bc -> 
  rw @@ Push (Constant ((n - m) mod 0x8FFF)) :: bc
| Push(Constant n) :: Push(Constant m) :: BinOp(Mult) :: bc -> 
  rw @@ Push (Constant ((n * m) mod 0x8FFF)) :: bc
| Push(Constant n) :: Push(Constant m) :: BinOp(Div) :: bc -> 
  rw @@ Push (Constant ((n / m) mod 0x8FFF)) :: bc
| Push(Constant n) :: Push(Constant m) :: BinOp(Eq) :: bc -> 
  rw @@ (if n = m then True else False) :: bc
| Push(Constant n) :: Push(Constant m) :: BinOp(Gt) :: bc -> 
  rw @@ (if n > m then True else False) :: bc
| Push(Constant n) :: Push(Constant m) :: BinOp(Lt) :: bc -> 
  rw @@ (if n < m then True else False) :: bc
| False :: False :: BinOp(And) :: bc
| False :: True :: BinOp(And) :: bc
| True :: False :: BinOp(And) :: bc -> rw (False :: bc)
| True :: True :: BinOp(And) :: bc -> rw (True :: bc)
| False :: False :: BinOp(Or) :: bc -> rw (False :: bc)
| False :: True :: BinOp(Or) :: bc
| True :: False :: BinOp(Or) :: bc 
| True :: True :: BinOp(Or) :: bc -> rw (True :: bc)
| s::bc -> s::(rw bc)
(* | Goto l1 :: Goto l2 :: bc ->  *)
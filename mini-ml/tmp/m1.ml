(* let rec 
g x = 0 and f x = g x 
*)
(*
let f () =
  let x1 = 54 in
  let x2 = -200 in
  let xu = 54 > (-200) in
  while true do () done ;;

let _ =f ()*)

(*
let n = 2
let x = assert (n < 2) *)
(*
let f x = ref x 
let g x = x := 2

let _ = print_int 5
let _ = print_char 'c'
let h = print_string "g"
let _ = print_char 'd'
*)
(*
let _ = 
  let i = ref 0 in
  while (!i) < 1500 do
    let x = "hello! " in print_int i; incr i
  done
*)
let sz = 3
let a = Array.make (sz + 2) 0

let _ = print_int (Array.length a)
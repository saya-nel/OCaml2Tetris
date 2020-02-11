
external alloc : (int -> unit) = "Memory.alloc" ;;

let main () =
	let n = 5 in
	alloc n;;
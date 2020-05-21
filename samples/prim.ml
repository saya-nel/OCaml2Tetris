external print_int : int -> unit = "caml_print_int" ;;
external print_newline : unit -> unit = "caml_print_newline" ;;
external array_make : int -> 'a -> 'a array = "caml_array_make" ;;
external array_length : 'a array -> int = "caml_array_length" ;;
external array_sub : 'a array -> int -> int -> 'a array  = "caml_array_sub" ;;

external array_get : 'a array -> int -> 'a = "caml_array_get" ;;
external array_set : 'a array -> int -> 'a -> unit  = "caml_array_set" ;;


let a = array_make 10 42 in
  let a' = array_sub a 3 4 in
  array_set a' 2 17;
  print_int (array_length a);
  print_newline ();
  print_int (array_get a' 2);
  print_newline ();
  print_int (array_length a')

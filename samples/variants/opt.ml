type 'a opt = S of 'a | N ;;

let f x  = S x ;;

match f 42 with
| N -> ()
| S x -> N2t.print_int x ;;

N2t.print_newline () ;;
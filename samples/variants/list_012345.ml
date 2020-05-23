let l = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: [] ;;

match l with
| [] -> ()
| h0::h1::h2::h3::t -> N2t.print_int h0 ;
                       N2t.print_int h1 ; 
                       N2t.print_int h2 ; 
                       N2t.print_int h3 ;;
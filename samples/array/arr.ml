let a = [|1;2;3;4|];;

N2t.array_set a 1 42;;

N2t.print_int (N2t.array_get a 1);;
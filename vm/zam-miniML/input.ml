let () = Data.push_global ((let x1 = Data.alloc 0 2 in
Data.set_data x1 0 (Mlvalues.val_long 1);Data.set_data x1 1 ((let x2 = Data.alloc 0 2 in
Data.set_data x2 0 (Mlvalues.val_long 2);Data.set_data x2 1 ((let x3 = Data.alloc 0 2 in
Data.set_data x3 0 (Mlvalues.val_long 3);Data.set_data x3 1 ((let x4 = Data.alloc 0 2 in
Data.set_data x4 0 (Mlvalues.val_long 4);Data.set_data x4 1 ((let x5 = Data.alloc 0 2 in
Data.set_data x5 0 (Mlvalues.val_long 5);Data.set_data x5 1 (Mlvalues.val_long 0); Mlvalues.val_ptr x5)); Mlvalues.val_ptr x4)); Mlvalues.val_ptr x3)); Mlvalues.val_ptr x2)); Mlvalues.val_ptr x1));
  Data.push_global ((let x1 = Data.alloc 0 2 in
Data.set_data x1 0 (Mlvalues.val_long 6);Data.set_data x1 1 ((let x2 = Data.alloc 0 2 in
Data.set_data x2 0 (Mlvalues.val_long 7);Data.set_data x2 1 ((let x3 = Data.alloc 0 2 in
Data.set_data x3 0 (Mlvalues.val_long 8);Data.set_data x3 1 ((let x4 = Data.alloc 0 2 in
Data.set_data x4 0 (Mlvalues.val_long 9);Data.set_data x4 1 ((let x5 = Data.alloc 0 2 in
Data.set_data x5 0 (Mlvalues.val_long 10);Data.set_data x5 1 (Mlvalues.val_long 0); Mlvalues.val_ptr x5)); Mlvalues.val_ptr x4)); Mlvalues.val_ptr x3)); Mlvalues.val_ptr x2)); Mlvalues.val_ptr x1));
  Data.push_global (Mlvalues.val_long 0);
  Data.push_global (Mlvalues.val_long 0)

let code = [|84; 45; 41; 42; 1; 0; 86; 19; 1; 11; 68; 50; 34; 11; 67; 64; 0; 40; 2; 1; 40; 2; 41; 42; 1; 1; 86; 38; 1; 67; 11; 33; 12; 68; 12; 50; 38; 5; 40; 2; 0; 93; Call.n2t_print_int; 40; 1; 53; 0; 54; 1; 9; 44; 1; 0; 3; 44; 1; 0; 23; 2; 14; 13; 34; 9; 43; 0; 40; 12; 34; 0; 12; 14; 16; 62; 0; 4; 19; 4; 57; 2; 53; 2; 63; 0; 57; 3|]

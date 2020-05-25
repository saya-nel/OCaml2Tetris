let () = Data.push_global (Data.add_string "m1.a");
  Data.push_global (Data.add_string "m1.b");
  Data.push_global (Data.add_string "dans m1");
  Data.push_global (Mlvalues.val_long 0);
  Data.push_global (Data.add_string "m2.a");
  Data.push_global (Data.add_string "m2.b");
  Data.push_global (Data.add_string "m2.c");
  Data.push_global (Mlvalues.val_long 0);
  Data.push_global (Data.add_string "m3.a");
  Data.push_global (Data.add_string "m3.b");
  Data.push_global (Data.add_string "m3.c");
  Data.push_global (Data.add_string "m3.d");
  Data.push_global ((let x1 = Data.alloc 0 2 in
Data.set_data x1 0 (Mlvalues.val_long 1);Data.set_data x1 1 ((let x2 = Data.alloc 0 2 in
Data.set_data x2 0 (Mlvalues.val_long 2);Data.set_data x2 1 ((let x3 = Data.alloc 0 2 in
Data.set_data x3 0 (Mlvalues.val_long 3);Data.set_data x3 1 ((let x4 = Data.alloc 0 2 in
Data.set_data x4 0 (Mlvalues.val_long 4);Data.set_data x4 1 (Mlvalues.val_long 0); Mlvalues.val_ptr x4)); Mlvalues.val_ptr x3)); Mlvalues.val_ptr x2)); Mlvalues.val_ptr x1));
  Data.push_global (Mlvalues.val_long 0);
  Data.push_global (Data.add_string "m4.a");
  Data.push_global (Data.add_string "m4.b");
  Data.push_global (Data.add_string "m4.c");
  Data.push_global (Data.add_string "m4.d");
  Data.push_global (Mlvalues.val_long 0);
  Data.push_global (Mlvalues.val_long 0)

let code = [|53; 0; 54; 1; 54; 2; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 0; 12; 64; 0; 19; 2; 57; 3; 53; 4; 54; 5; 54; 6; 12; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 103; 42; 93; Call.n2t_print_int; 55; 3; 1; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 1; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 55; 3; 0; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 0; 12; 14; 65; 0; 19; 3; 57; 7; 53; 8; 54; 9; 54; 10; 54; 11; 54; 12; 56; 7; 2; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 103; 17; 93; Call.n2t_print_int; 2; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 0; 12; 14; 16; 18; 8; 62; 0; 5; 19; 5; 57; 13; 53; 14; 54; 15; 54; 16; 54; 17; 56; 13; 4; 56; 13; 2; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 103; 17; 93; Call.n2t_print_int; 2; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 103; 56; 10; 93; Call.n2t_print_int; 55; 13; 4; 10; 86; 151; 0; 67; 93; Call.n2t_print_int; 19; 1; 0; 12; 14; 16; 18; 8; 18; 10; 62; 0; 6; 19; 6; 57; 18; 53; 18; 54; 13; 54; 7; 54; 3; 62; 0; 4; 57; 19|]

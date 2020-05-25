let () = Data.push_global (Mlvalues.val_long 0);
  Data.push_global (Mlvalues.val_long 0)

let code = [|84; 13; 0; 127; 1; 26; 37; 2; 0; 127; 42; 40; 1; 43; 0; 8; 10; 43; 1; 2; 108; 4; 11; 33; 93; Call.n2t_print_int; 19; 2; 58; 57; 0; 53; 0; 63; 0; 57; 1|]

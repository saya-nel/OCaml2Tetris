let () = Data.push_global (Data.add_string "toto");
  Data.push_global (Data.add_string "leizuhlziuhlzeiu");
  Data.push_global (Data.add_string "titif");
  Data.push_global (0);
  Data.push_global (Data.add_string "foo");
  Data.push_global (Data.add_string "gg");
  Data.push_global (Data.add_string "huhu");
  Data.push_global (0);
  Data.push_global (0)

let code = [|53; 0; 54; 1; 54; 2; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 0; 12; 64; 0; 19; 2; 57; 3; 53; 4; 54; 5; 54; 6; 12; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 103; 42; 93; Call.n2t_print_int; 55; 3; 1; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 0; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 55; 3; 0; 93; Call.n2t_print_string; 99; 93; Call.n2t_print_newline; 0; 12; 14; 65; 0; 19; 3; 57; 8|]

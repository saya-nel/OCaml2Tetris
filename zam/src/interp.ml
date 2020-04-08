let stack_size = 1024

let pc = ref 0

let sp = ref 0
let stack = Array.make stack_size (Mlvalues.val_long 0)

let acc = ref (Mlvalues.val_long 0)
let env = ref (Mlvalues.val_long 0)

(* *********************************** *)

let pop_stack () =
  let v = stack.(!sp) in decr sp; v

let push_stack v =
  stack.(!sp) <- v; incr sp

let take_argument code =
  incr pc;
  code.(!pc)

let debug_print_block block =
  print_string "block : ";
  print_string "size : ";
  print_int (Mlvalues.size (Mlvalues.ptr_val block));
  print_string ", content : ";
  for i = 0 to Mlvalues.size (Mlvalues.ptr_val block) - 1 do
    print_string "<";
    print_int (Mlvalues.get_field block i);
    print_string ">";
    print_string " | "
  done;
  print_newline ()

let debug_print_state () = 
  print_string " pc: "; 
  print_int (!pc);
  print_string " acc: "; 
  if Mlvalues.is_ptr (!acc) then 
    debug_print_block (!acc)
  else 
    print_int (Mlvalues.long_val (!acc));
  print_string " sp: "; 
  print_int (!sp);
  print_newline ()

let debug_print_stack () =
  print_string "stack :";
  print_newline ();
  for i = 0 to !sp - 1 do  
    print_int (Mlvalues.long_val stack.(i));
    print_string " | "
  done;
  print_newline ()


let interp code =
  let extra_args = ref 0 in
  let trap_sp = ref 0 in

  (* env := make_empty_env (); *)
  sp := 0;
  print_int (!acc);
  while !pc < Array.length code do
    incr pc where () = 
    debug_print_state ();
    match code.(!pc) with
    | 0 (* ACC0 *) -> acc := stack.((!sp)-1)
    | 1 (* ACC1 *) -> acc := stack.((!sp)-2)
    | 2 (* ACC2 *) -> acc := stack.((!sp)-3)
    | 3 (* ACC3 *) -> acc := stack.((!sp)-4)
    | 4 (* ACC4 *) -> acc := stack.((!sp)-5)
    | 5 (* ACC5 *) -> acc := stack.((!sp)-6)
    | 6 (* ACC6 *) -> acc := stack.((!sp)-7)
    | 7 (* ACC7 *) -> acc := stack.((!sp)-8)
    | 8 (* ACC *) -> let n = take_argument code in
                     assert (n < Array.length stack && n >= 0);
                     acc := stack.((!sp)-1-n)
    | 9 (* PUSH *) -> push_stack (!acc)
    | 10 (* PUSHACC0 *) -> push_stack (!acc)
    | 11 (* PUSHACC1 *) -> push_stack (!acc); acc := stack.((!sp)-2)
    | 12 (* PUSHACC2 *) -> push_stack (!acc); acc := stack.((!sp)-3)
    | 13 (* PUSHACC3 *) -> push_stack (!acc); acc := stack.((!sp)-4)
    | 14 (* PUSHACC4 *) -> push_stack (!acc); acc := stack.((!sp)-5)
    | 15 (* PUSHACC5 *) -> push_stack (!acc); acc := stack.((!sp)-6)
    | 16 (* PUSHACC6 *) -> push_stack (!acc); acc := stack.((!sp)-7)
    | 17 (* PUSHACC7 *) -> push_stack (!acc); acc := stack.((!sp)-8)
    | 18 (* PUSHACC *) -> let n = take_argument code in
                          push_stack (!acc); 
                          acc := stack.((!sp)-1-n)
    | 19 (* POP *) -> let n = take_argument code in 
                      assert (n < Array.length stack && n >= 0);
                      sp := !sp - n
    | 20 (* ASSIGN *) -> let n = take_argument code in 
                         assert (n < Array.length stack && n >= 0);
                         (stack.((!sp)-1-n) <- !acc; acc := 0)
    | 21 (* ENVACC1 *) -> acc := Mlvalues.get_field (!env) 1
    | 22 (* ENVACC2 *) -> acc := Mlvalues.get_field (!env) 2
    | 23 (* ENVACC3 *) -> acc := Mlvalues.get_field (!env) 3
    | 24 (* ENVACC4 *) -> acc := Mlvalues.get_field (!env) 4
    | 25 (* ENVACC *) -> let n = take_argument code in acc := Mlvalues.get_field (!env) n
    | 26 (* PUSHENVACC1 *) -> push_stack (!acc); acc := Mlvalues.get_field (!env) 1
    | 27 (* PUSHENVACC2 *) -> push_stack (!acc); acc := Mlvalues.get_field (!env) 2
    | 28 (* PUSHENVACC3 *) -> push_stack (!acc); acc := Mlvalues.get_field (!env) 3
    | 29 (* PUSHENVACC4 *) -> push_stack (!acc); acc := Mlvalues.get_field (!env) 4
    | 30 (* PUSHENVACC *) -> let n = take_argument code in(* Equivalent to PUSH then ENVACC *)
                             push_stack (!acc);
                             acc := Mlvalues.get_field (!env) n
    | 31 (* PUSH-RETADDR *) -> let ofs = take_argument code in
                               failwith "todo"
    | 32 (* APPLY *) -> let args = take_argument code in
                        extra_args := (args) - 1;
                        pc := Mlvalues.addr_closure (!acc); (* todo ? *)
                        env := Mlvalues.env_closure (!acc);
                        failwith "todo"
    | 62 (* MAKEBLOCK *) -> let tag = take_argument code in
                            let sz = take_argument code in     (* attention à l'ordre des arguments (tag et pc) dans la pile *)
                            let blk = Mlvalues.new_block tag sz in
                            Mlvalues.set_field blk 0 (!acc);
                            for i = 1 to sz - 1 do 
                              Mlvalues.set_field blk i (pop_stack ())
                            done;
                            acc := blk
    | 63 (* MAKEBLOCK1 *) -> let tag = take_argument code in
                             let blk = Mlvalues.new_block tag 1 in
                             Mlvalues.set_field blk 0 (!acc);
                             acc := blk
    | 64 (* MAKEBLOCK2 *) -> let tag = take_argument code in
                             let blk = Mlvalues.new_block tag 2 in
                             Mlvalues.set_field blk 0 (!acc);
                             Mlvalues.set_field blk 1 (pop_stack ())
                             acc := blk
    | 65 (* MAKEBLOCK4 *) -> let tag = take_argument code in
                             let blk = Mlvalues.new_block tag 2 in
                             Mlvalues.set_field blk 0 (!acc);
                             Mlvalues.set_field blk 1 (pop_stack ())
                             Mlvalues.set_field blk 2 (pop_stack ())
                             acc := blk

    (* 66 MAKEFLOATBLOCK *)

    | 67 (* GETFIELD0 *) -> acc := Mlvalues.get_field (!acc) 0
    | 68 (* GETFIELD1 *) -> acc := Mlvalues.get_field (!acc) 1
    | 69 (* GETFIELD2 *) -> acc := Mlvalues.get_field (!acc) 2 
    | 70 (* GETFIELD3 *) -> acc := Mlvalues.get_field (!acc) 4
    | 71 (* GETFIELD *) -> let n = code.(!pc) in 
                           acc := Mlvalues.get_field (!acc) n
    
    (* GETFLOATFIELD (opInput.code: 72) *)
    
    | 73 (* SETFIELD0 *) -> Mlvalues.set_field (!acc) 0 (pop_stack ()) 
    | 74 (* SETFIELD1 *) -> Mlvalues.set_field (!acc) 1 (pop_stack ()) 
    | 75 (* SETFIELD2 *) -> Mlvalues.set_field (!acc) 2 (pop_stack ())    
    | 76 (* SETFIELD3 *) -> Mlvalues.set_field (!acc) 3 (pop_stack ())
    | 77 (* SETFIELD *) -> let n = take_argument code in 
                           Mlvalues.set_field (!acc) n (pop_stack ())

    (* 78 *)
    | 79 (* VECTLENGTH *) -> acc := Mlvalues.val_long (Mlvalues.size acc)
    | 84 (* BRANCH *) -> let n = take_argument code in 
                         assert (n < Array.length code);
                         pc := (!pc) + n
    | 85 (* BRANCHIF *) -> let n = take_argument code in 
                           assert (n < Array.length code);
                           pc := if Mlvalues.long_val (!acc) = 1 then (!pc) + n else (!pc) + 1
    | 86 (* BRANCHIFNOT *) -> let n = take_argument code in 
                              assert (n < Array.length code);
                              pc := if Mlvalues.long_val (!acc) = 0 then (!pc) + n else (!pc) + 1
    (* 87 SWITCH *)
    | 88 (* BOOLNOT *) -> acc := Prims.bnot (!acc)
    (* | 92 à 98 -> interop avec C *)
    | 99  (* CONST0 *) -> acc := Mlvalues.val_long 0
    | 100 (* CONST1 *) -> acc := Mlvalues.val_long 1
    | 101 (* CONST2 *) -> acc := Mlvalues.val_long 2
    | 102 (* CONST3 *) -> acc := Mlvalues.val_long 3
    | 103 (* CONSTINT *) -> let n = take_argument code in 
                            acc := Mlvalues.val_long n
    | 109 (* NEGINT *) -> acc := Prims.negint acc
    | 110 (* ADDINT *) -> acc := Prims.addint (pop_stack ()) (!acc)
    | 111 (* SUBINT *) -> acc := Prims.subint (pop_stack ()) (!acc)
    | 112 (* MULINT *) -> acc := Prims.mulint (pop_stack ()) (!acc)
    | 113 (* DIVINT *) -> acc := Prims.divint (pop_stack ()) (!acc)
    | 114 (* MODINT *) -> acc := Prims.modint (pop_stack ()) (!acc)
    | 115 (* ANDINT *) -> acc := Prims.andint (pop_stack ()) (!acc)
    | 116 (* ORINT  *) -> acc := Prims.orint  (pop_stack ()) (!acc)
    | 117 (* XORINT *) -> acc := Prims.xorint (pop_stack ()) (!acc)
    | 118 (* LSLINT *) -> acc := Prims.lslint (pop_stack ()) (!acc)
    | 119 (* LSRINT *) -> acc := Prims.lsrint (pop_stack ()) (!acc)
    | 120 (* ASRINT *) -> acc := Prims.asrint (pop_stack ()) (!acc)
    | 121 (* EQ     *) -> acc := Prims.eq     (pop_stack ()) (!acc)
    | 122 (* NEQ    *) -> acc := Prims.neq    (pop_stack ()) (!acc)
    | 123 (* LTINT  *) -> acc := Prims.ltint  (pop_stack ()) (!acc)
    | 124 (* LEINT  *) -> acc := Prims.leint  (pop_stack ()) (!acc)
    | 125 (* GTINT  *) -> acc := Prims.gtint  (pop_stack ()) (!acc)
    | 126 (* GEINT  *) -> acc := Prims.geint  (pop_stack ()) (!acc)
    | 127 (* OFFSETINT *) -> let ofs = take_argument code in 
                             acc := Prims.addint ofs (!acc)
    | 128 (* OFFSETREF *) -> failwith "todo"
    | 129 (* ISINT *) -> acc := Prims.isint (!acc) 
    | 130 (* GETMETHOD *) -> failwith "todo"
    | 131 (* BEQ *) ->
      let v = take_argument code in
      let ofs = take_argument code in
      if  Prims.compare_imm v (!acc) = 0 then pc := (!pc) + ofs - 1
    | 132 (* BNEQ *) -> 
      let v = take_argument code in
      let ofs = take_argument code in
      if Prims.compare_imm v (!acc) <> 0 then pc := (!pc) + ofs - 1
    | 133 (* BLTINT *) ->
      let v = take_argument code in
      let ofs = take_argument code in
      if Prims.compare_imm v (!acc) < 0 then pc := (!pc) + ofs - 1
    | 134 (* BLEINT *) ->
      let v = take_argument code in
      let ofs = take_argument code in
      if Prims.compare_imm v (!acc) <= 0 then pc := (!pc) + ofs - 1
    | 135 (* BGTINT *) ->
      let v = take_argument code in
      let ofs = take_argument code in
      if Prims.compare_imm v (!acc) > 0 then pc := (!pc) + ofs - 1
    | 136 (* BGEINT *) ->
      let v = take_argument code in
      let ofs = take_argument code in
      if Prims.compare_imm v (!acc) >= 0 then pc := (!pc) + ofs - 1
    | 137 (* ULTINT *) -> acc := Prims.ultint (pop_stack ()) (!acc)
    | 138 (* UGEINT *) -> acc := Prims.ugeint (pop_stack ()) (!acc)
    | 143 (* STOP *) -> pc := Array.length code
  done;
  print_string "fin programme :";
  print_newline ();
  debug_print_state ();
  debug_print_stack ();
  print_newline ()



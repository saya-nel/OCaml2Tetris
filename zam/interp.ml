let stack_size = 1024

let pc = ref 0

let sp = ref 0
let stack = Array.make stack_size (Mlvalues.val_long 0)

let acc = ref (Mlvalues.val_long 0)
let env = ref (Mlvalues.val_long 0)

(* *********************************** *)

let pop_stack () =
  decr sp;
  stack.(!sp+1)

let debug_print_state () = 
  print_string " pc: "; 
  print_int (!pc);
  print_string " acc: "; 
  print_int (Mlvalues.long_val (!acc));
  print_string " sp: "; 
  print_int (!sp);
  print_newline ()

let interp () =
  let extra_args = ref 0 in
  let trap_sp = ref 0 in

  acc := 0;
  (* env := make_empty_env (); *)
  sp := 0;
  print_int (!acc);
  while !pc < Array.length Input.code do
    debug_print_state ();
    match Input.code.(!pc) with
    | 0 (* ACC0 *) -> acc := stack.(0); incr pc
    | 1 (* ACC1 *) -> acc := stack.(1); incr pc
    | 2 (* ACC2 *) -> acc := stack.(2); incr pc
    | 3 (* ACC3 *) -> acc := stack.(3); incr pc    
    | 4 (* ACC4 *) -> acc := stack.(4); incr pc
    | 5 (* ACC5 *) -> acc := stack.(5); incr pc
    | 6 (* ACC6 *) -> acc := stack.(6); incr pc
    | 7 (* ACC7 *) -> acc := stack.(7); incr pc   
    | 8 (* ACC *) -> 
       incr pc;
       let n = Input.code.(!pc) in
       if n >= Array.length stack || n < 0 then failwith "stack pleine"
       else acc := stack.(n);
       incr pc
    | 9 (* Push *) -> 
      incr sp; stack.(!sp) <- !acc; incr pc
    | 10 (* PUSHACC0 (~ PUSH) *) -> 
      incr sp; stack.(!sp) <- !acc; incr pc
    | 11 (* PUSHACC1 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(1); incr pc
    | 12 (* PUSHACC2 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(2); incr pc
    | 13 (* PUSHACC3 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(3); incr pc
    | 14 (* PUSHACC4 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(4); incr pc
    | 15 (* PUSHACC5 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(5); incr pc
    | 16 (* PUSHACC6 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(6); incr pc
    | 17 (* PUSHACC7 *) -> 
      incr sp; stack.(!sp) <- !acc; acc := stack.(7); incr pc
    | 18 (* PUSHACC *) -> 
      incr pc; 
      let n = Input.code.(!pc) in
      incr sp; stack.(!sp) <- !acc; acc := stack.(n); incr pc
    | 19 (* POP *) -> 
      incr pc;
      let n = Input.code.(!pc) in 
      if n >= Array.length stack || n < 0 then failwith "Stack pleine "
      else sp := !sp - n;
      incr pc
    | 20 (* ASSIGN *) -> 
      incr pc;
      let n = Input.code.(!pc) in 
      if n >= Array.length stack || n < 0 then failwith "stack pleine"
      else begin stack.(n) <- !acc;
                  acc := 0
            end;
       incr pc 
    | 21 (* ENVACC1 *) -> acc := Mlvalues.get_field (!env) 1; incr pc
    | 22 (* ENVACC2 *) -> acc := Mlvalues.get_field (!env) 2; incr pc
    | 23 (* ENVACC3 *) -> acc := Mlvalues.get_field (!env) 3; incr pc
    | 24 (* ENVACC4 *) -> acc := Mlvalues.get_field (!env) 4; incr pc
    | 25 (* ENVACC *) -> 
      incr pc;
      let n = Input.code.(!pc) in 
      acc := Mlvalues.get_field (!env) n;
      incr pc
    | 26 (* PUSHENVACC1 *) ->
      incr sp; stack.(!sp) <- !acc; acc := Mlvalues.get_field (!env) 1; incr pc
    | 27 (* PUSHENVACC2 *) ->
      incr sp; stack.(!sp) <- !acc; acc := Mlvalues.get_field (!env) 2; incr pc
    | 28 (* PUSHENVACC3 *) ->
      incr sp; stack.(!sp) <- !acc; acc := Mlvalues.get_field (!env) 3; incr pc
    | 29 (* PUSHENVACC4 *) ->
      incr sp; stack.(!sp) <- !acc; acc := Mlvalues.get_field (!env) 4; incr pc
    | 30 (* PUSHENVACC *) -> (* Equivalent to PUSH then ENVACC *)
      incr pc;
      let n = Input.code.(!pc) in
      incr sp; stack.(!sp) <- !acc; 
      acc := Mlvalues.get_field (!env) n;
      incr pc
    | 31 (* PUSH-RETADDR *) ->
      incr pc;
      let ofs = Input.code.(!pc) in
      failwith "todo"
    | 32 (* APPLY *) -> 
      incr pc;
      let args = Input.code.(!pc) in
      extra_args := (!args) - 1;
      pc := Mlvalues.addr_closure (acc); (* todo ? *)
      env := Mlvalues.env_closure (acc)
    failwith "todo"
    | 62 (* MAKEBLOCK *) -> 
      incr pc;
      let tag = Input.code.(!pc) in
      incr pc;
      let sz = Input.code.(!pc) in     (* attention à l'ordre des arguments (tag et pc) dans la pile *)
      let blk = Mlvalues.new_block tag sz in
      Mlvalues.set_field blk 0 (!acc);
      for i = 1 to sz - 1 do 
        Mlvalues.set_field blk i (pop_stack ())
      done;
      acc := blk;
      incr pc

  (********* MAKEBLOCK 1 2 3 ****************)


    | 67 (* GETFIELD0 *) -> acc := Mlvalues.get_field (!acc) 0; incr pc
    | 68 (* GETFIELD1 *) -> acc := Mlvalues.get_field (!acc) 1; incr pc
    | 69 (* GETFIELD2 *) -> acc := Mlvalues.get_field (!acc) 2; incr pc 
    | 70 (* GETFIELD3 *) -> acc := Mlvalues.get_field (!acc) 4; incr pc
    | 71 (* GETFIELD *) ->
      incr pc;
      let n = Input.code.(!pc) in 
      acc := Mlvalues.get_field (!acc) n;
      incr pc
    
    (* GETFLOATFIELD (opInput.code: 72) *)
    
    | 73 (* SETFIELD0 *) ->
      Mlvalues.set_field (!acc) 0 stack.(!sp);
      decr sp;
      incr pc 
    | 74 (* SETFIELD1 *) ->
      Mlvalues.set_field (!acc) 1 stack.(!sp);
      decr sp;
      incr pc 
    | 75 (* SETFIELD2 *) ->
      Mlvalues.set_field (!acc) 2 stack.(!sp);
      decr sp;
      incr pc    
    | 76 (* SETFIELD3 *) ->
      Mlvalues.set_field (!acc) 3 stack.(!sp);
      decr sp;
      incr pc
    | 77 (* SETFIELD *) ->
      incr pc;
      let n = Input.code.(!pc) in 
      Mlvalues.set_field (!acc) n stack.(!sp);
      decr sp;
      incr pc


    | 79 (* VECTLENGTH *) -> 
      acc := Mlvalues.val_long (Mlvalues.size acc);
      incr pc
    | 84 (* BRANCH *) -> 
       incr pc;
       let n = Input.code.(!pc) in 
       if n >= Array.length Input.code then failwith "Instr array out of bounds"
       else pc := !pc + n
    | 85 (* BRANCHIF *) -> 
       incr pc;
       let n = Input.code.(!pc) in 
       if n >= Array.length Input.code then failwith "Instr array out of bounds"
       else pc := if Mlvalues.long_val (!acc) = 1 then (!pc) + n else !pc + 1
    | 85 (* BRANCHIFNOT *) -> 
       incr pc;
       let n = Input.code.(!pc) in 
       if n >= Array.length Input.code then failwith "Instr array out of bounds"
       else pc := if Mlvalues.long_val (!acc) = 0 then (!pc) + n else !pc + 1
    
    (* | 92 à 98 -> interop avec C *)
    | 99 (* Const0 *) -> 
       acc := Mlvalues.val_long 0;
       incr pc
    | 100 (* Const1 *) -> 
       acc := Mlvalues.val_long 1;
       incr pc
    | 101 (* Const2 *) -> 
       acc := Mlvalues.val_long 2;
       incr pc
    | 102 (* Const3 *) -> 
       acc := Mlvalues.val_long 3;
       incr pc
    | 103 (* ConstInt *) -> 
       incr pc;
       let n = Input.code.(!pc) in
       acc := Mlvalues.val_long n;
       incr pc
    | 109 (*NEGINT*) -> 
       acc := Prims.negint acc;
       incr pc
    | 110 (* ADDINT *) ->  
       acc := Prims.addint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 111 (* SUBINT *) ->  
       acc := Prims.subint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 112 (* MULINT *) -> 
       acc := Prims.mulint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 113 (* DIVINT *) -> 
       acc := Prims.divint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 114 (* MODINT *) -> 
       acc := Prims.modint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 115 (* ANDINT *) -> 
       acc := Prims.andint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 116 (* ORINT *) -> 
       acc := Prims.orint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 117 (* XORINT *) -> 
       acc := Prims.xorint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 118 (* LSLINT *) -> 
       acc := Prims.lslint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 119 (* LSRINT *) -> 
       acc := Prims.lsrint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 120 (* ASRINT *) -> 
       acc := Prims.asrint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 121 (* EQ *) ->
       acc := Prims.eq stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 122 (* NEQ *) ->
       acc := Prims.neq stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 123 (* LTINT *) -> 
       acc := Prims.ltint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 124 (* LEINT *) -> 
       acc := Prims.leint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 125 (* GTINT *) ->
       acc := Prims.gtint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 126 (* GEINT *) -> 
       acc := Prims.geint stack.(!sp) (!acc);
       decr sp;
       incr pc
    | 127 (* OFFSETINT *) -> 
      incr pc; 
      let ofs = Input.code.(!pc) in
      acc := Prims.addint ofs (!acc);
      incr pc
    | 128 (* OFFSETREF *) -> failwith "todo"
    | 129 (* ISINT *) -> acc := Prims.isint (!acc) 
    | 130 (* GETMETHOD *) -> failwith "todo"
    | 131 (* BEQ *) -> 
      incr pc; 
      let v = Input.code.(!pc) in
      incr pc;
      let ofs = Input.code.(!pc) in
      pc := (!pc) + (if Prims.compare_imm v (!acc) = 0 then ofs else 1)
    | 132 (* BNEQ *) ->
      incr pc; 
      let v = Input.code.(!pc) in
      incr pc;
      let ofs = Input.code.(!pc) in
      pc := (!pc) + (if Prims.compare_imm v (!acc) <> 0 then ofs else 1)
    | 133 (* BLTINT *) ->
      incr pc; 
      let v = Input.code.(!pc) in
      incr pc;
      let ofs = Input.code.(!pc) in
      pc := (!pc) + (if Prims.compare_imm v (!acc) < 0 then ofs else 1)
    | 134 (* BLEINT *) ->
      incr pc; 
      let v = Input.code.(!pc) in
      incr pc;
      let ofs = Input.code.(!pc) in
      pc := (!pc) + (if Prims.compare_imm v (!acc) <= 0 then ofs else 1)
    | 135 (* BGTINT *) ->
      incr pc; 
      let v = Input.code.(!pc) in
      incr pc;
      let ofs = Input.code.(!pc) in
      pc := (!pc) + (if Prims.compare_imm v (!acc) > 0 then ofs else 1)
    | 136 (* BGEINT *) ->
      incr pc; 
      let v = Input.code.(!pc) in
      incr pc;
      let ofs = Input.code.(!pc) in
      pc := (!pc) + (if Prims.compare_imm v (!acc) >= 0 then ofs else 1)
    | 137 (* ULTINT *) ->
      acc := Prims.ultint stack.(!sp) (!acc);
      decr sp;
      incr pc
    | 138 (* UGEINT *) ->
      acc := Prims.ugeint stack.(!sp) (!acc);
      decr sp;
      incr pc
    | 143 (* STOP *) -> pc := Array.length Input.code
  done

let () =
  interp ()


let debug = true

let stack_size = 1024

let pc = ref 0

let sp = ref 0
let extra_args = ref 0 
let stack = Array.make stack_size (Mlvalues.val_long 0)

let acc = ref (Mlvalues.val_long 0)
let env = ref (Mlvalues.val_long 0)

let global = ref (Mlvalues.make_block 0 40)

let trap_sp = ref 0

let pop_stack () =
  let v = stack.(!sp - 1) in 
  decr sp;
  v

let push_stack v =
  stack.(!sp) <- v; incr sp

let take_argument code =
  incr pc;
  code.(!pc)

let rec debug_print_block block =
  if debug then 
    begin
      print_string "(block, size : ";
      print_int (Mlvalues.size (Mlvalues.ptr_val block));
      print_string ", tag : ";
      print_int (Mlvalues.long_val (Mlvalues.tag (Mlvalues.ptr_val block)));
      print_string ") ";
      for i = 0 to Mlvalues.size (Mlvalues.ptr_val block) - 1 do
        print_string "<";
        if Mlvalues.is_ptr (Mlvalues.get_field block i) then
          debug_print_block (Mlvalues.get_field block i)
        else
          print_int (Mlvalues.long_val (Mlvalues.get_field block i));
        print_string ">";
        print_string " | "
      done;
      print_newline ()
    end

let debug_print_state () = 
  if debug then 
    begin
      print_newline ();
      print_string " pc: "; 
      print_int (!pc);
      print_string ", acc: "; 
      if Mlvalues.is_ptr (!acc) then 
        debug_print_block (!acc)
      else 
        print_int (Mlvalues.long_val (!acc));
      print_string ", env: ";
      if Mlvalues.is_ptr (!env) then
        debug_print_block (!env)
      else 
        print_int (Mlvalues.long_val (!env));
      print_string ", sp: "; 
      print_int (!sp);
      print_string ", extra args: ";
      print_int (!extra_args);
      print_newline ()
    end
(* print_string " global: ";
   if Mlvalues.is_ptr (!global) then
   debug_print_block (!global);
   print_newline (); *)

let debug_print_stack () =
  if debug then 
    begin
      print_newline ();
      print_string "stack :";
      print_newline ();
      for i = 0 to !sp - 1 do  
        print_int (Mlvalues.long_val stack.(i));
        print_string " | "
      done;
      print_newline ()
    end

let interp code =
  sp := 0;
  while !pc < Array.length code do
    debug_print_state ();
    begin
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
        (stack.((!sp)-1-n) <- !acc; acc := (Mlvalues.val_long 0))
      | 21 (* ENVACC1 *) -> acc := Mlvalues.get_field (!env) 1
      | 22 (* ENVACC2 *) -> acc := Mlvalues.get_field (!env) 2
      | 23 (* ENVACC3 *) -> acc := Mlvalues.get_field (!env) 3
      | 24 (* ENVACC4 *) -> acc := Mlvalues.get_field (!env) 4
      | 25 (* ENVACC *) -> let n = take_argument code in acc := Mlvalues.get_field (!env) n
      | 26 (* PUSHENVACC1 *) -> push_stack (!acc); acc := Mlvalues.get_field (!env) 1
      | 27 (* PUSHENVACC2 *) -> push_stack (!acc); acc := Mlvalues.get_field (!env) 2
      | 28 (* PUSHENVACC3 *) -> push_stack (!acc); acc := Mlvalues.get_field (!env) 3
      | 29 (* PUSHENVACC4 *) -> push_stack (!acc); acc := Mlvalues.get_field (!env) 4
      | 30 (* PUSHENVACC *) -> let n = take_argument code in (* Equivalent to PUSH then ENVACC *)
        push_stack (!acc);
        acc := Mlvalues.get_field (!env) n
      | 31 (* PUSH-RETADDR *) -> let ofs = take_argument code in
        push_stack (Mlvalues.val_long (!extra_args));
        push_stack (!env);
        push_stack (Mlvalues.val_long ofs)
      | 32 (* APPLY *) -> let args = take_argument code in 
        extra_args := args - 1;
        pc := (Mlvalues.long_val (Mlvalues.addr_closure (!acc))) - 1; (* -1 pour enlever incrémentation d'apres *)
        env := Mlvalues.env_closure (!acc)
      | 33 (* APPLY1 *) -> let arg = pop_stack () in
        push_stack (Mlvalues.val_long (!extra_args));
        push_stack (!env);
        push_stack (Mlvalues.val_long ((!pc) + 1));
        push_stack arg;
        pc := ((Mlvalues.long_val (Mlvalues.addr_closure (!acc))) - 1);
        env := Mlvalues.env_closure (!acc);
        extra_args := 0
      | 34 (* APPLY2 *) -> let arg1 = pop_stack () in
        let arg2 = pop_stack () in
        push_stack (Mlvalues.val_long (!extra_args));
        push_stack (!env);
        push_stack (Mlvalues.val_long ((!pc) + 1));
        push_stack arg2;
        push_stack arg1;
        pc := ((Mlvalues.long_val (Mlvalues.addr_closure (!acc))) - 1);
        env := Mlvalues.env_closure (!acc);
        extra_args := 1
      | 35 (* APPLY3 *) -> let arg1 = pop_stack () in
        let arg2 = pop_stack () in
        let arg3 = pop_stack () in
        push_stack (Mlvalues.val_long (!extra_args));
        push_stack (!env);
        push_stack (Mlvalues.val_long ((!pc) + 1));
        push_stack arg3;
        push_stack arg2;
        push_stack arg1;
        pc := ((Mlvalues.long_val (Mlvalues.addr_closure (!acc))) - 1);
        env := Mlvalues.env_closure (!acc);
        extra_args := 2
      | 36 (* APPTERM *) -> let n = take_argument code in
        let s = take_argument code in
        for i = 0 to n - 1 do
          stack.((!sp) - s + i) <- stack.((!sp) - n + i)
        done;
        sp := (!sp) - (s - n);             
        pc := (Mlvalues.long_val (Mlvalues.addr_closure (!acc))) - 1;
        env := Mlvalues.env_closure (!acc);
        extra_args := (!extra_args) + n - 1
      | 37 (* APPTERM1 *) -> let s = take_argument code in 
        stack.((!sp) - s + 0) <- stack.((!sp) - 1 + 0);
        sp := (!sp) - (s - 1);             
        pc := (Mlvalues.long_val (Mlvalues.addr_closure (!acc))) - 1
      | 38 (* APPTERM2 *) -> let s = take_argument code in 
        stack.((!sp) - s + 0) <- stack.((!sp) - 2 + 0);
        stack.((!sp) - s + 1) <- stack.((!sp) - 2 + 1);
        sp := (!sp) - (s - 1);             
        pc := (Mlvalues.long_val (Mlvalues.addr_closure (!acc))) - 1;
        incr extra_args
      | 39 (* APPTERM3 *) -> let s = take_argument code in 
        stack.((!sp) - s + 0) <- stack.((!sp) - 3 + 0);
        stack.((!sp) - s + 1) <- stack.((!sp) - 3 + 1);
        stack.((!sp) - s + 1) <- stack.((!sp) - 3 + 2);
        sp := (!sp) - (s - 1);          
        pc := (Mlvalues.long_val (Mlvalues.addr_closure (!acc))) - 1;
        extra_args := (!extra_args) + 2
      | 40 (* RETURN *) -> let n = take_argument code in
        sp := (!sp) - n; 
        if (!extra_args) <= 0 
        then 
          begin
            pc := (Mlvalues.long_val (pop_stack ())) - 1;
            env := pop_stack ();
            extra_args := Mlvalues.long_val (pop_stack ())
          end
        else 
          begin
            decr extra_args;
            pc := (Mlvalues.long_val (Mlvalues.addr_closure (!acc)) - 1);
            env := Mlvalues.env_closure (!acc)
          end
      | 41 (* RESTART *) ->
        let size = Mlvalues.size (Mlvalues.ptr_val (!env)) in
        for i = 1 to size - 2 do 
          push_stack (Mlvalues.get_field (!env) (size-i))
        done;
        env := Mlvalues.get_field (!env) 1;
        extra_args := !extra_args + size - 2
      | 42 (* GRAB *) -> let n = take_argument code in
        if !extra_args >= n 
        then extra_args := !extra_args - n
        else 
          begin 
            let closure_env = Mlvalues.make_env (!extra_args + 3) in 
            Mlvalues.set_field closure_env 0 (Mlvalues.val_long (!pc - 4)); (* -4 car avec take_argument on a incr pc *)
            Mlvalues.set_field closure_env 1 !env;
            for i = 2 to !extra_args + 1 do 
              Mlvalues.set_field closure_env i (pop_stack ())
            done;
            acc := Mlvalues.make_closure (!pc - 4) closure_env; (* -4 car avec take_argument on a incr pc *)
            pc := Mlvalues.long_val (pop_stack ()) - 1; (* -1 pour eviter prochaine incr *)
            env := pop_stack ();
            extra_args := Mlvalues.long_val (pop_stack ()) 
          end
      | 43 (* CLOSURE *) -> let n = take_argument code in
        if n > 0 then push_stack (!acc);
        let addr = take_argument code in
        let closure_env = Mlvalues.make_env (n + 1) in
        Mlvalues.set_field closure_env 0 (Mlvalues.val_long (addr)); 
        for i = 1 to n do (* c'est bien n et pas n-1 *)
          Mlvalues.set_field closure_env i (pop_stack ()) 
        done;
        acc := Mlvalues.make_closure addr closure_env
      | 44 (* CLOSUREREC *) -> 
        (* source : https://github.com/stevenvar/OMicroB/blob/master/src/byterun/vm/interp.c#L768 *)
        let f = take_argument code in
        let v = take_argument code in
        let o = take_argument code in
        if v > 0 then push_stack !acc;
        let closure_size = (2 * f) - 1 + v in
        let closure = Mlvalues.make_block Mlvalues.closure_tag closure_size in
        acc := closure;
        Mlvalues.set_field !acc 0 (Mlvalues.val_codeptr o);
        for i = 1 to f-1 do 
          Mlvalues.set_field !acc (2 * i - 1) (Mlvalues.make_block Mlvalues.infix_tag (2 * i));
          Mlvalues.set_field !acc (2 * i - 1) (Mlvalues.val_long ((take_argument code) - i - 2))
        done;
        for i = 1 to v-1 do 
          Mlvalues.set_field !acc (i + 2 * f - 1) (pop_stack ()) 
        done;
        push_stack !acc;
        for i = 1 to f-1 do 
          push_stack (Mlvalues.get_field !acc (2 * i)) 
        done
      | 45 (* OFFSETCLOSUREM2 *) -> 
        acc := Mlvalues.val_long (Mlvalues.long_val !env - 2)
      | 46 (* OFFSETCLOSURE0 *) -> 
        acc := !env
      | 47 (* OFFSETCLOSURE2 *) -> 
        acc := Mlvalues.val_long (Mlvalues.long_val !env + 2)
      | 48 (* OFFSETCLOSURE *) -> 
        let n = take_argument code in
        acc := Mlvalues.val_long (Mlvalues.long_val !env + n)
      | 49 (* PUSHOFFSETCLOSUREM2 *) -> 
        push_stack !acc;
        acc := Mlvalues.val_long (Mlvalues.long_val !env - 2)
      | 50 (* PUSHOFFSETCLOSURE0 *) -> 
        push_stack !acc;
        acc := !env
      | 51 (* PUSHOFFSETCLOSURE2 *) ->
        push_stack !acc;
        acc := Mlvalues.val_long (Mlvalues.long_val !env + 2)
      | 52 (* PUSHOFFSETCLOSURE *) -> 
        let n = take_argument code in
        push_stack !acc;
        acc := Mlvalues.val_long (Mlvalues.long_val !env + n)
      | 53 (* GETGLOBAL *) -> let n = take_argument code in
        acc := (Mlvalues.get_field (!global) n)
      | 54 (* PUSHGETGLOBAL *) -> push_stack (!acc);
        let n = take_argument code in
        acc := (Mlvalues.get_field (!global) n)
      | 55 (* GETGLOBALFIELD *) -> let n = take_argument code in
        let p = take_argument code in
        let g = Mlvalues.get_field (!global) n in
        acc := Mlvalues.get_field g p

      | 56 (* PUSHGETGLOBALFIELD *) -> push_stack (!acc);
        let n = take_argument code in
        let p = take_argument code in
        let g = Mlvalues.get_field (!global) n in
        acc := Mlvalues.get_field g p


      | 57 (* SETGLOBAL *) -> let n = take_argument code in
        Mlvalues.set_field (!global) n (!acc);
        acc := Mlvalues.unit

      | 58 (* ATOM0 *) -> acc := Mlvalues.make_block 0 0
      | 59 (* ATOM *) -> let tag = take_argument code in
        acc := Mlvalues.make_block tag 0
      | 60 (* PUSHATOM0 *) -> push_stack (!acc);
        acc := Mlvalues.make_block 0 0
      | 61 (* PUSHATOM *) -> push_stack (!acc);
        let tag = take_argument code in
        acc := Mlvalues.make_block tag 0
      | 62 (* MAKEBLOCK *) -> let sz = take_argument code in
        let tag = take_argument code in (* attention à l'ordre des arguments (tag et pc) dans le code *)
        let blk = Mlvalues.make_block tag sz in
        Mlvalues.set_field blk 0 (!acc);
        for i = 1 to sz - 1 do 
          Mlvalues.set_field blk i (pop_stack ())
        done;
        acc := blk
      | 63 (* MAKEBLOCK1 *) -> let tag = take_argument code in
        let blk = Mlvalues.make_block tag 1 in
        Mlvalues.set_field blk 0 (!acc);
        acc := blk
      | 64 (* MAKEBLOCK2 *) -> let tag = take_argument code in
        let blk = Mlvalues.make_block tag 2 in
        Mlvalues.set_field blk 0 (!acc);
        Mlvalues.set_field blk 1 (pop_stack ());
        acc := blk
      | 65 (* MAKEBLOCK3 *) -> let tag = take_argument code in
        let blk = Mlvalues.make_block tag 3 in
        Mlvalues.set_field blk 0 (!acc);
        Mlvalues.set_field blk 1 (pop_stack ());
        Mlvalues.set_field blk 2 (pop_stack ());
        acc := blk

      (* 66 MAKEFLOATBLOCK *)

      | 67 (* GETFIELD0 *) -> acc := Mlvalues.get_field (!acc) 0
      | 68 (* GETFIELD1 *) -> acc := Mlvalues.get_field (!acc) 1
      | 69 (* GETFIELD2 *) -> acc := Mlvalues.get_field (!acc) 2 
      | 70 (* GETFIELD3 *) -> acc := Mlvalues.get_field (!acc) 3
      | 71 (* GETFIELD *) -> let n = take_argument code in 
        acc := Mlvalues.get_field (!acc) n

      (* GETFLOATFIELD (opInput.code: 72) *)

      | 73 (* SETFIELD0 *) -> Mlvalues.set_field (!acc) 0 (pop_stack ()); acc := Mlvalues.unit 
      | 74 (* SETFIELD1 *) -> Mlvalues.set_field (!acc) 1 (pop_stack ()); acc := Mlvalues.unit 
      | 75 (* SETFIELD2 *) -> Mlvalues.set_field (!acc) 2 (pop_stack ()); acc := Mlvalues.unit  
      | 76 (* SETFIELD3 *) -> Mlvalues.set_field (!acc) 3 (pop_stack ()); acc := Mlvalues.unit 
      | 77 (* SETFIELD *) -> let n = take_argument code in 
        Mlvalues.set_field (!acc) n (pop_stack ());
        acc := Mlvalues.unit 

      (* 78 SETFLOATFIELD *)

      | 79 (* VECTLENGTH *) -> acc := Mlvalues.val_long (Mlvalues.size (Mlvalues.ptr_val (!acc)))

      | 80 (* GETVECTITEM *) -> let n = Mlvalues.long_val (pop_stack ()) in
        acc := Mlvalues.get_field (!acc) n
      | 81 (* SETVECTITEM *) -> assert ((!sp) > 1);
        let n = pop_stack () in
        let v = pop_stack () in
        Mlvalues.set_field (!acc) (Mlvalues.long_val n) v;
        acc := Mlvalues.unit

      | 82 (* GETSTRINGCHAR *) -> (* parfois appele GETBYTESCHAR *)
        assert (!sp > 0);
        (* let n = pop_stack () in *)
        () (* Mlvalues.get_bytes (!acc) (Mlvalues.long_val n) *) (* à revoir, résultat va dans l'acc ? *)
      | 83 (* SETBYTESCHAR *) -> assert ((!sp) > 1);
        let n = pop_stack () in
        let v = pop_stack () in
        Mlvalues.set_bytes (!acc) (Mlvalues.long_val n) v;
        acc := Mlvalues.unit
      | 84 (* BRANCH *) -> let n = take_argument code in 
        (* assert (n < Array.length code); *)
        pc := n - 1 (* (!pc) + n - 2 *) (* - 2 pour enlever incr d'apres + take_argument incr*)
      | 85 (* BRANCHIF *) -> let n = take_argument code in 
        (* assert (n < Array.length code); *)
        if Mlvalues.long_val (!acc) = 1 then pc := n - 1 (* (!pc) + n - 2 *) (* - 2 pour enlever incr d'apres + take_argument incr*) (* -3 ?? *)
      | 86 (* BRANCHIFNOT *) -> let n = take_argument code in 
        (* assert (n < Array.length code); *)
        if Mlvalues.long_val (!acc) = 0 then pc := n - 1 (* (!pc) + n - 3 *) (* - 2 pour enlever incr d'apres + take_argument incr*) (* -3 ?? *)

      (* SWITCH *)

      | 88 (* BOOLNOT *) -> acc := Prims.bnot (!acc)
      | 89 (* PUSHTRAP *) -> let ofs = take_argument code in
        push_stack (Mlvalues.val_long !extra_args);
        push_stack !env;
        push_stack (Mlvalues.val_long !trap_sp);
        push_stack (Mlvalues.val_long (!pc - 1 + ofs)); (* -1 car on a prit un argument *)
        trap_sp := !sp
      | 90 (* POPTRAP *) -> 
        let _ = pop_stack () in
        trap_sp := Mlvalues.long_val (pop_stack ());
        let _ = pop_stack () in
        let _ = pop_stack () in ()
      | 91 (* RAISE *) -> 
        if !trap_sp = 0 then begin 
          print_string "Exception, acc = ";
          print_int (Mlvalues.long_val !acc);
          print_newline () end 
        else begin
          sp := !trap_sp;
          pc := Mlvalues.long_val (pop_stack ());
          trap_sp := Mlvalues.long_val (pop_stack ());
          env := pop_stack ();
          extra_args := Mlvalues.long_val (pop_stack ())
        end
      | 92 (* CHECK-SIGNALS *) -> print_string "CHECK-SIGNALS"
      | 93 (* C-CALL1 *) ->

        (*** placer dans le fichier source (.ml):       ***)
        (*** external print_int : int -> unit = "fake0" ***)
        let p = take_argument code in
        push_stack (!env);
        (match p with
         | 0 -> print_int (Mlvalues.long_val !acc); push_stack Mlvalues.unit
         (* indice de la primitive -> code de la primitive *)
        );
        let _ = pop_stack () in ()
      | 94 (* C-CALL2 *) -> 
        let p = take_argument code in
        let x = pop_stack () in
        push_stack (!env);
        (* (match p with ...) *)
        for i = 0 to 2 do let _ = pop_stack () in () done
      | 95 (* C-CALL3 *) ->
        let p = take_argument code in
        let x1 = pop_stack () in
        let x2 = pop_stack () in   
        push_stack (!env);
        (* (match p with ...) *)      
        for i = 0 to 3 do let _ = pop_stack () in () done
      | 96 (* C-CALL4 *) -> 
        let p = take_argument code in
        let x1 = pop_stack () in
        let x2 = pop_stack () in 
        let x3 = pop_stack () in   
        push_stack (!env);
        (* (match p with ...) *)      
        for i = 0 to 4 do let _ = pop_stack () in () done
      | 97 (* C-CALL5 *) -> 
        let p = take_argument code in
        let x1 = pop_stack () in
        let x2 = pop_stack () in 
        let x3 = pop_stack () in   
        let x4 = pop_stack () in   
        push_stack (!env);
        (* (match p with ...) *)      
        for i = 0 to 4 do let _ = pop_stack () in () done
      | 98 (* C-CALLN *) -> (* TODO *) ()
      | 99  (* CONST0 *) -> acc := Mlvalues.val_long 0
      | 100 (* CONST1 *) -> acc := Mlvalues.val_long 1
      | 101 (* CONST2 *) -> acc := Mlvalues.val_long 2
      | 102 (* CONST3 *) -> acc := Mlvalues.val_long 3
      | 103 (* CONSTINT *) -> let n = take_argument code in 
        acc := Mlvalues.val_long n
      | 104 (* PUSHCONST0 *) -> push_stack (!acc);
        acc := (Mlvalues.val_long 0)
      | 105 (* PUSHCONST1 *) -> push_stack (!acc);
        acc := (Mlvalues.val_long 1)
      | 106 (* PUSHCONST2 *) -> push_stack (!acc);
        acc := (Mlvalues.val_long 2)
      | 107 (* PUSHCONST3 *) -> push_stack (!acc);
        acc := (Mlvalues.val_long 3)
      | 108 (* PUSHCONSTINT *) -> let n = take_argument code in
        push_stack (!acc);
        acc := (Mlvalues.val_long n)
      | 109 (* NEGINT *) -> acc := Prims.negint (!acc)
      | 110 (* ADDINT *) -> acc := Prims.addint (!acc) (pop_stack ()) 
      | 111 (* SUBINT *) -> acc := Prims.subint (!acc) (pop_stack ()) 
      | 112 (* MULINT *) -> acc := Prims.mulint (!acc) (pop_stack ()) 
      | 113 (* DIVINT *) -> acc := Prims.divint (!acc) (pop_stack ()) 
      | 114 (* MODINT *) -> acc := Prims.modint (!acc) (pop_stack ()) 
      | 115 (* ANDINT *) -> acc := Prims.andint (!acc) (pop_stack ()) 
      | 116 (* ORINT  *) -> acc := Prims.orint  (!acc) (pop_stack ()) 
      | 117 (* XORINT *) -> acc := Prims.xorint (!acc) (pop_stack ()) 
      | 118 (* LSLINT *) -> acc := Prims.lslint (!acc) (pop_stack ()) 
      | 119 (* LSRINT *) -> acc := Prims.lsrint (!acc) (pop_stack ()) 
      | 120 (* ASRINT *) -> acc := Prims.asrint (!acc) (pop_stack ()) 
      | 121 (* EQ     *) -> acc := Prims.eq     (!acc) (pop_stack ()) 
      | 122 (* NEQ    *) -> acc := Prims.neq    (!acc) (pop_stack ()) 
      | 123 (* LTINT  *) -> acc := Prims.ltint  (!acc) (pop_stack ()) 
      | 124 (* LEINT  *) -> acc := Prims.leint  (!acc) (pop_stack ()) 
      | 125 (* GTINT  *) -> acc := Prims.gtint  (!acc) (pop_stack ()) 
      | 126 (* GEINT  *) -> acc := Prims.geint  (!acc) (pop_stack ()) 
      | 127 (* OFFSETINT *) -> let ofs = take_argument code in 
        acc := Prims.addint (!acc) (Mlvalues.val_long ofs)
      | 128 (* OFFSETREF *) -> let ofs = take_argument code in
        let old = Mlvalues.get_field (!acc) 0 in
        Mlvalues.set_field (!acc) 0 (Prims.addint old (Mlvalues.val_long ofs));
        acc := Mlvalues.unit
      | 129 (* ISINT *) -> acc := Prims.isint (!acc) 
      | 130 (* GETMETHOD *) -> let x = pop_stack () in (* [[[[[[[[[[[[ à verifier ]]]]]]]]]]]] *)
        let y = Mlvalues.get_field x 0 in
        acc := Mlvalues.get_field y (Mlvalues.long_val (!acc))
      | 131 (* BEQ *) ->
        let v = take_argument code in
        let ofs = take_argument code in
        if  Prims.compare_imm (Mlvalues.val_long v) (!acc) = 0 then pc := ofs - 1
      | 132 (* BNEQ *) -> 
        let v = take_argument code in
        let ofs = take_argument code in
        if Prims.compare_imm (Mlvalues.val_long v) (!acc) <> 0 then pc := ofs - 1 (* pc := (!pc) + ofs - 1 *)
      | 133 (* BLTINT *) ->
        let v = take_argument code in
        let ofs = take_argument code in
        if Prims.compare_imm (Mlvalues.val_long v) (!acc) < 0 then pc := ofs - 1
      | 134 (* BLEINT *) ->
        let v = take_argument code in
        let ofs = take_argument code in
        if Prims.compare_imm (Mlvalues.val_long v) (!acc) <= 0 then pc := ofs - 1
      | 135 (* BGTINT *) ->
        let v = take_argument code in
        let ofs = take_argument code in
        if Prims.compare_imm (Mlvalues.val_long v) (!acc) > 0 then pc := ofs - 1
      | 136 (* BGEINT *) ->
        let v = take_argument code in
        let ofs = take_argument code in
        if Prims.compare_imm (Mlvalues.val_long v) (!acc) >= 0 then pc := ofs - 1
      | 137 (* ULTINT *) -> acc := Prims.ultint (!acc) (pop_stack ()) 
      | 138 (* UGEINT *) -> acc := Prims.ugeint (!acc) (pop_stack ())
      | 139 (* BULTINT *) -> let v = take_argument code in 
        let ofs = take_argument code in
        if Mlvalues.long_val (Prims.ultint (Mlvalues.val_long v) (!acc)) = 1 then pc := ofs - 1

      | 140 (* BUGEINT *) -> let v = take_argument code in 
        let ofs = take_argument code in
        if Mlvalues.long_val (Prims.ugeint (Mlvalues.val_long v) (!acc)) = 1 then pc := ofs - 1
      (* GETPUBMET *)
      (* GETDYNMET *)

      | 143 (* STOP *) -> pc := Array.length code

      (* EVENT *)
      (* BREAk *)

      | _ -> assert false
    end;
    incr pc
  done;
  print_newline ();
  print_string "fin programme :";
  print_newline ();
  debug_print_state ();
  debug_print_stack ();
  print_newline ()



let debug = true

let pc = ref 0

let extra_args = ref 0 

let trap_sp = ref 0

let pop_stack () =
  let v = Mlvalues.stack.(!Mlvalues.sp - 1) in 
  decr Mlvalues.sp;
  v

let push_stack v =
  Mlvalues.stack.(!Mlvalues.sp) <- v; incr Mlvalues.sp

let take_argument code =
  incr pc;
  code.(!pc)

let rec debug_print_block block =
  if debug then 
    begin
      print_string "(block[";
      print_int (Mlvalues.ptr_val block);
      print_string "], size : ";
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

let debug_print_arr arr arr_end name =
  if debug then 
    begin
      print_newline ();
      print_string name;
      print_string " :";
      print_newline ();
      for i = 0 to arr_end do  
        if Mlvalues.is_ptr arr.(i) then 
          debug_print_block (arr.(i))
        else begin
          print_int (Mlvalues.long_val arr.(i)) 
        end;
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
      print_string " --------------------------------------";
      print_newline ();
      print_string "acc: "; 
      if Mlvalues.is_ptr (!Mlvalues.acc) then 
        debug_print_block (!Mlvalues.acc)
      else 
        print_int (Mlvalues.long_val (!Mlvalues.acc));
      print_string ", env: ";
      if Mlvalues.is_ptr (!Mlvalues.env) then
        debug_print_block (!Mlvalues.env)
      else 
        print_int (Mlvalues.long_val (!Mlvalues.env));
      print_string ", Mlvalues.sp: "; 
      print_int (!Mlvalues.sp);
      print_string ", extra args: ";
      print_int (!extra_args);
      print_newline ();
      debug_print_arr Mlvalues.stack !Mlvalues.sp "stack";
      (* debug_print_arr !Mlvalues.from_space (!Mlvalues.heap_top - 1) "from_space" *)
    end
(* print_string " global: ";
   if Mlvalues.is_ptr (!global) then
   debug_print_block (!global);
   print_newline (); *)

let interp code =
  Mlvalues.sp := 0;
  while !pc < Array.length code do
    debug_print_state ();
    begin
      match code.(!pc) with
      | 0 (* ACC0 *) -> Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-1)
      | 1 (* ACC1 *) -> Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-2)
      | 2 (* ACC2 *) -> Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-3)
      | 3 (* ACC3 *) -> Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-4)
      | 4 (* ACC4 *) -> Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-5)
      | 5 (* ACC5 *) -> Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-6)
      | 6 (* ACC6 *) -> Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-7)
      | 7 (* ACC7 *) -> Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-8)
      | 8 (* ACC *) -> let n = take_argument code in
        assert (n < Array.length Mlvalues.stack && n >= 0);
        Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-1-n)
      | 9 (* PUSH *) -> push_stack (!Mlvalues.acc)
      | 10 (* PUSHACC0 *) -> push_stack (!Mlvalues.acc)
      | 11 (* PUSHACC1 *) -> push_stack (!Mlvalues.acc); Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-2)
      | 12 (* PUSHACC2 *) -> push_stack (!Mlvalues.acc); Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-3)
      | 13 (* PUSHACC3 *) -> push_stack (!Mlvalues.acc); Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-4)
      | 14 (* PUSHACC4 *) -> push_stack (!Mlvalues.acc); Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-5)
      | 15 (* PUSHACC5 *) -> push_stack (!Mlvalues.acc); Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-6)
      | 16 (* PUSHACC6 *) -> push_stack (!Mlvalues.acc); Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-7)
      | 17 (* PUSHACC7 *) -> push_stack (!Mlvalues.acc); Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-8)
      | 18 (* PUSHACC *) -> let n = take_argument code in
        push_stack (!Mlvalues.acc); 
        Mlvalues.acc := Mlvalues.stack.((!Mlvalues.sp)-1-n)
      | 19 (* POP *) -> let n = take_argument code in 
        assert (n < Array.length Mlvalues.stack && n >= 0);
        Mlvalues.sp := !Mlvalues.sp - n
      | 20 (* ASSIGN *) -> let n = take_argument code in 
        assert (n < Array.length Mlvalues.stack && n >= 0);
        (Mlvalues.stack.((!Mlvalues.sp)-1-n) <- !Mlvalues.acc; Mlvalues.acc := (Mlvalues.val_long 0))
      | 21 (* ENVACC1 *) -> Mlvalues.acc := Mlvalues.get_field (!Mlvalues.env) 1
      | 22 (* ENVACC2 *) -> Mlvalues.acc := Mlvalues.get_field (!Mlvalues.env) 2
      | 23 (* ENVACC3 *) -> Mlvalues.acc := Mlvalues.get_field (!Mlvalues.env) 3
      | 24 (* ENVACC4 *) -> Mlvalues.acc := Mlvalues.get_field (!Mlvalues.env) 4
      | 25 (* ENVACC *) -> let n = take_argument code in Mlvalues.acc := Mlvalues.get_field (!Mlvalues.env) n
      | 26 (* PUSHENVACC1 *) -> push_stack (!Mlvalues.acc); Mlvalues.acc := Mlvalues.get_field (!Mlvalues.env) 1
      | 27 (* PUSHENVACC2 *) -> push_stack (!Mlvalues.acc); Mlvalues.acc := Mlvalues.get_field (!Mlvalues.env) 2
      | 28 (* PUSHENVACC3 *) -> push_stack (!Mlvalues.acc); Mlvalues.acc := Mlvalues.get_field (!Mlvalues.env) 3
      | 29 (* PUSHENVACC4 *) -> push_stack (!Mlvalues.acc); Mlvalues.acc := Mlvalues.get_field (!Mlvalues.env) 4
      | 30 (* PUSHENVACC *) -> let n = take_argument code in (* Equivalent to PUSH then ENVACC *)
        push_stack (!Mlvalues.acc);
        Mlvalues.acc := Mlvalues.get_field (!Mlvalues.env) n
      | 31 (* PUSH-RETADDR *) -> let ofs = take_argument code in
        push_stack (Mlvalues.val_long (!extra_args));
        push_stack (!Mlvalues.env);
        push_stack (Mlvalues.val_long ofs)
      | 32 (* APPLY *) -> let args = take_argument code in 
        extra_args := args - 1;
        pc := (Mlvalues.ptr_val (Mlvalues.addr_closure (!Mlvalues.acc))) - 1; (* -1 pour enlever incrémentation d'apres *)
        Mlvalues.env := Mlvalues.env_closure (!Mlvalues.acc)
      | 33 (* APPLY1 *) -> let arg = pop_stack () in
        push_stack (Mlvalues.val_long (!extra_args));
        push_stack (!Mlvalues.env);
        push_stack (Mlvalues.val_long ((!pc) + 1));
        push_stack arg;
        pc := ((Mlvalues.long_val (Mlvalues.addr_closure (!Mlvalues.acc))) - 1);
        Mlvalues.env := Mlvalues.env_closure (!Mlvalues.acc);
        extra_args := 0
      | 34 (* APPLY2 *) -> let arg1 = pop_stack () in
        let arg2 = pop_stack () in
        push_stack (Mlvalues.val_long (!extra_args));
        push_stack (!Mlvalues.env);
        push_stack (Mlvalues.val_long ((!pc) + 1));
        push_stack arg2;
        push_stack arg1;
        pc := ((Mlvalues.long_val (Mlvalues.addr_closure (!Mlvalues.acc))) - 1);
        Mlvalues.env := Mlvalues.env_closure (!Mlvalues.acc);
        extra_args := 1
      | 35 (* APPLY3 *) -> let arg1 = pop_stack () in
        let arg2 = pop_stack () in
        let arg3 = pop_stack () in
        push_stack (Mlvalues.val_long (!extra_args));
        push_stack (!Mlvalues.env);
        push_stack (Mlvalues.val_long ((!pc) + 1));
        push_stack arg3;
        push_stack arg2;
        push_stack arg1;
        pc := ((Mlvalues.long_val (Mlvalues.addr_closure (!Mlvalues.acc))) - 1);
        Mlvalues.env := Mlvalues.env_closure (!Mlvalues.acc);
        extra_args := 2
      | 36 (* APPTERM *) -> let n = take_argument code in
        let s = take_argument code in
        for i = 0 to n - 1 do
          Mlvalues.stack.((!Mlvalues.sp) - s + i) <- Mlvalues.stack.((!Mlvalues.sp) - n + i)
        done;
        Mlvalues.sp := (!Mlvalues.sp) - (s - n);             
        pc := (Mlvalues.long_val (Mlvalues.addr_closure (!Mlvalues.acc))) - 1;
        Mlvalues.env := Mlvalues.env_closure (!Mlvalues.acc);
        extra_args := (!extra_args) + n - 1
      | 37 (* APPTERM1 *) -> let s = take_argument code in 
        Mlvalues.stack.((!Mlvalues.sp) - s + 0) <- Mlvalues.stack.((!Mlvalues.sp) - 1 + 0);
        Mlvalues.sp := (!Mlvalues.sp) - (s - 1);             
        pc := (Mlvalues.long_val (Mlvalues.addr_closure (!Mlvalues.acc))) - 1
      | 38 (* APPTERM2 *) -> let s = take_argument code in 
        Mlvalues.stack.((!Mlvalues.sp) - s + 0) <- Mlvalues.stack.((!Mlvalues.sp) - 2 + 0);
        Mlvalues.stack.((!Mlvalues.sp) - s + 1) <- Mlvalues.stack.((!Mlvalues.sp) - 2 + 1);
        Mlvalues.sp := (!Mlvalues.sp) - (s - 1);             
        pc := (Mlvalues.long_val (Mlvalues.addr_closure (!Mlvalues.acc))) - 1;
        incr extra_args
      | 39 (* APPTERM3 *) -> let s = take_argument code in 
        Mlvalues.stack.((!Mlvalues.sp) - s + 0) <- Mlvalues.stack.((!Mlvalues.sp) - 3 + 0);
        Mlvalues.stack.((!Mlvalues.sp) - s + 1) <- Mlvalues.stack.((!Mlvalues.sp) - 3 + 1);
        Mlvalues.stack.((!Mlvalues.sp) - s + 1) <- Mlvalues.stack.((!Mlvalues.sp) - 3 + 2);
        Mlvalues.sp := (!Mlvalues.sp) - (s - 1);          
        pc := (Mlvalues.long_val (Mlvalues.addr_closure (!Mlvalues.acc))) - 1;
        extra_args := (!extra_args) + 2
      | 40 (* RETURN *) -> let n = take_argument code in
        Mlvalues.sp := (!Mlvalues.sp) - n; 
        if (!extra_args) <= 0 
        then 
          begin
            pc := (Mlvalues.long_val (pop_stack ())) - 1;
            Mlvalues.env := pop_stack ();
            extra_args := Mlvalues.long_val (pop_stack ())
          end
        else 
          begin
            decr extra_args;
            pc := (Mlvalues.long_val (Mlvalues.addr_closure (!Mlvalues.acc)) - 1);
            Mlvalues.env := Mlvalues.env_closure (!Mlvalues.acc)
          end
      | 41 (* RESTART *) ->
        let size = Mlvalues.size (Mlvalues.ptr_val (!Mlvalues.env)) in
        for i = 1 to size - 2 do 
          push_stack (Mlvalues.get_field (!Mlvalues.env) (size-i))
        done;
        Mlvalues.env := Mlvalues.get_field (!Mlvalues.env) 1;
        extra_args := !extra_args + size - 2
      | 42 (* GRAB *) -> let n = take_argument code in
        if !extra_args >= n 
        then extra_args := !extra_args - n
        else 
          begin 
            Mlvalues.acc := Mlvalues.make_closure (!pc - 4) (!extra_args + 3);
            Mlvalues.set_field !Mlvalues.acc 1 !Mlvalues.env;
            for i = 2 to !extra_args do
              Mlvalues.set_field !Mlvalues.acc i (pop_stack ())
            done;
            pc := Mlvalues.long_val (pop_stack ()) - 1;
            Mlvalues.env := pop_stack();
            extra_args := Mlvalues.long_val (pop_stack ())
          end
      | 43 (* CLOSURE *) -> let n = take_argument code in
        if n > 0 then push_stack (!Mlvalues.acc);
        let addr = take_argument code in
        Mlvalues.acc := Mlvalues.make_closure addr (n + 1);
        for i = 1 to n - 1 do
          Mlvalues.set_field !Mlvalues.acc i (pop_stack ())
        done
      | 44 (* CLOSUREREC *) -> 
        (* source : https://github.com/stevenvar/OMicroB/blob/master/src/byterun/vm/interp.c#L768 *)
        let f = take_argument code in
        let v = take_argument code in
        let o = take_argument code in
        if v > 0 then push_stack !Mlvalues.acc;
        let closure_size = (2 * f) - 1 + v in
        Mlvalues.acc := Mlvalues.make_block Mlvalues.closure_tag closure_size;
        for i = 1 to f-1 do 
          Mlvalues.set_field !Mlvalues.acc (2 * i - 1) (Mlvalues.make_header Mlvalues.infix_tag (2 * i));
          Mlvalues.set_field !Mlvalues.acc (2 * i) (Mlvalues.val_long (take_argument code))
        done;
        for i = 0 to v-1 do 
          Mlvalues.set_field !Mlvalues.acc (i + 2 * f - 1) (pop_stack ())
        done;
        Mlvalues.set_field !Mlvalues.acc 0 o; (* (Mlvalues.val_ptr o) ?? *)
        push_stack !Mlvalues.acc;
        for i = 1 to f - 1 do
          push_stack (Mlvalues.get_field (!Mlvalues.acc) (2 * i))
        done
      | 45 (* OFFSETCLOSUREM2 *) ->            
        Mlvalues.acc := Mlvalues.val_ptr (Mlvalues.ptr_val !Mlvalues.env - 2)
      | 46 (* OFFSETCLOSURE0 *) -> 
        Mlvalues.acc := Mlvalues.val_ptr (Mlvalues.ptr_val !Mlvalues.env) (* à simplifier *)
      | 47 (* OFFSETCLOSURE2 *) -> 
        Mlvalues.acc := Mlvalues.val_ptr (Mlvalues.ptr_val !Mlvalues.env + 2)
      | 48 (* OFFSETCLOSURE *) -> 
        let n = take_argument code in
        Mlvalues.acc := Mlvalues.val_ptr (Mlvalues.ptr_val !Mlvalues.env + n)
      | 49 (* PUSHOFFSETCLOSUREM2 *) -> 
        push_stack !Mlvalues.acc;
        Mlvalues.acc := Mlvalues.val_ptr (Mlvalues.ptr_val !Mlvalues.env - 2)
      | 50 (* PUSHOFFSETCLOSURE0 *) -> 
        push_stack !Mlvalues.acc;
        Mlvalues.acc := Mlvalues.val_ptr (Mlvalues.ptr_val !Mlvalues.env) (* à simplifier *)
      | 51 (* PUSHOFFSETCLOSURE2 *) ->
        push_stack !Mlvalues.acc;
        Mlvalues.acc := Mlvalues.val_ptr (Mlvalues.ptr_val !Mlvalues.env + 2)
      | 52 (* PUSHOFFSETCLOSURE *) -> 
        let n = take_argument code in
        push_stack !Mlvalues.acc;
        Mlvalues.acc := Mlvalues.val_ptr (Mlvalues.ptr_val !Mlvalues.env + n)
      | 53 (* GETGLOBAL *) -> let n = take_argument code in
        Mlvalues.acc := (Mlvalues.get_global n)
      | 54 (* PUSHGETGLOBAL *) -> push_stack (!Mlvalues.acc);
        let n = take_argument code in
        Mlvalues.acc := (Mlvalues.get_global n)
      | 55 (* GETGLOBALFIELD *) -> let n = take_argument code in
        let p = take_argument code in
        let g = Mlvalues.get_global n in
        Mlvalues.acc := Mlvalues.get_field g p

      | 56 (* PUSHGETGLOBALFIELD *) -> push_stack (!Mlvalues.acc);
        let n = take_argument code in
        let p = take_argument code in
        let g = Mlvalues.get_global n in
        Mlvalues.acc := Mlvalues.get_field g p


      | 57 (* SETGLOBAL *) -> let n = take_argument code in
        Mlvalues.set_global n (!Mlvalues.acc);
        Mlvalues.acc := Mlvalues.unit

      | 58 (* ATOM0 *) -> Mlvalues.acc := Mlvalues.make_block 0 0
      | 59 (* ATOM *) -> let tag = take_argument code in
        Mlvalues.acc := Mlvalues.make_block tag 0
      | 60 (* PUSHATOM0 *) -> push_stack (!Mlvalues.acc);
        Mlvalues.acc := Mlvalues.make_block 0 0
      | 61 (* PUSHATOM *) -> push_stack (!Mlvalues.acc);
        let tag = take_argument code in
        Mlvalues.acc := Mlvalues.make_block tag 0
      | 62 (* MAKEBLOCK *) -> let sz = take_argument code in
        let tag = take_argument code in (* attention à l'ordre des arguments (tag et pc) dans le code *)
        let blk = Mlvalues.make_block tag sz in
        Mlvalues.set_field blk 0 (!Mlvalues.acc);
        for i = 1 to sz - 1 do 
          Mlvalues.set_field blk i (pop_stack ())
        done;
        Mlvalues.acc := blk
      | 63 (* MAKEBLOCK1 *) -> let tag = take_argument code in
        let blk = Mlvalues.make_block tag 1 in
        Mlvalues.set_field blk 0 (!Mlvalues.acc);
        Mlvalues.acc := blk
      | 64 (* MAKEBLOCK2 *) -> let tag = take_argument code in
        let blk = Mlvalues.make_block tag 2 in
        Mlvalues.set_field blk 0 (!Mlvalues.acc);
        Mlvalues.set_field blk 1 (pop_stack ());
        Mlvalues.acc := blk
      | 65 (* MAKEBLOCK3 *) -> let tag = take_argument code in
        let blk = Mlvalues.make_block tag 3 in
        Mlvalues.set_field blk 0 (!Mlvalues.acc);
        Mlvalues.set_field blk 1 (pop_stack ());
        Mlvalues.set_field blk 2 (pop_stack ());
        Mlvalues.acc := blk

      (* 66 MAKEFLOATBLOCK *)

      | 67 (* GETFIELD0 *) -> Mlvalues.acc := Mlvalues.get_field (!Mlvalues.acc) 0
      | 68 (* GETFIELD1 *) -> Mlvalues.acc := Mlvalues.get_field (!Mlvalues.acc) 1
      | 69 (* GETFIELD2 *) -> Mlvalues.acc := Mlvalues.get_field (!Mlvalues.acc) 2 
      | 70 (* GETFIELD3 *) -> Mlvalues.acc := Mlvalues.get_field (!Mlvalues.acc) 3
      | 71 (* GETFIELD *) -> let n = take_argument code in 
        Mlvalues.acc := Mlvalues.get_field (!Mlvalues.acc) n

      (* GETFLOATFIELD (opInput.code: 72) *)

      | 73 (* SETFIELD0 *) -> Mlvalues.set_field (!Mlvalues.acc) 0 (pop_stack ()); Mlvalues.acc := Mlvalues.unit 
      | 74 (* SETFIELD1 *) -> Mlvalues.set_field (!Mlvalues.acc) 1 (pop_stack ()); Mlvalues.acc := Mlvalues.unit 
      | 75 (* SETFIELD2 *) -> Mlvalues.set_field (!Mlvalues.acc) 2 (pop_stack ()); Mlvalues.acc := Mlvalues.unit  
      | 76 (* SETFIELD3 *) -> Mlvalues.set_field (!Mlvalues.acc) 3 (pop_stack ()); Mlvalues.acc := Mlvalues.unit 
      | 77 (* SETFIELD *) -> let n = take_argument code in 
        Mlvalues.set_field (!Mlvalues.acc) n (pop_stack ());
        Mlvalues.acc := Mlvalues.unit 

      (* 78 SETFLOATFIELD *)

      | 79 (* VECTLENGTH *) -> Mlvalues.acc := Mlvalues.val_long (Mlvalues.size (Mlvalues.ptr_val (!Mlvalues.acc)))

      | 80 (* GETVECTITEM *) -> let n = Mlvalues.long_val (pop_stack ()) in
        Mlvalues.acc := Mlvalues.get_field (!Mlvalues.acc) n
      | 81 (* SETVECTITEM *) -> assert ((!Mlvalues.sp) > 1);
        let n = pop_stack () in
        let v = pop_stack () in
        Mlvalues.set_field (!Mlvalues.acc) (Mlvalues.long_val n) v;
        Mlvalues.acc := Mlvalues.unit

      | 82 (* GETSTRINGCHAR *) -> (* parfois appele GETBYTESCHAR *)
        assert (!Mlvalues.sp > 0);
        (* let n = pop_stack () in *)
        () (* Mlvalues.get_bytes (!Mlvalues.acc) (Mlvalues.long_val n) *) (* à revoir, résultat va dans l'acc ? *)
      | 83 (* SETBYTESCHAR *) -> assert ((!Mlvalues.sp) > 1);
        let n = pop_stack () in
        let v = pop_stack () in
        Mlvalues.set_bytes (!Mlvalues.acc) (Mlvalues.long_val n) v;
        Mlvalues.acc := Mlvalues.unit
      | 84 (* BRANCH *) -> let n = take_argument code in 
        (* assert (n < Array.length code); *)
        pc := n - 1 (* (!pc) + n - 2 *) (* - 2 pour enlever incr d'apres + take_argument incr*)
      | 85 (* BRANCHIF *) -> let n = take_argument code in 
        (* assert (n < Array.length code); *)
        if Mlvalues.long_val (!Mlvalues.acc) = 1 then pc := n - 1 (* (!pc) + n - 2 *) (* - 2 pour enlever incr d'apres + take_argument incr*) (* -3 ?? *)
      | 86 (* BRANCHIFNOT *) -> let n = take_argument code in 
        (* assert (n < Array.length code); *)
        if Mlvalues.long_val (!Mlvalues.acc) = 0 then pc := n - 1 (* (!pc) + n - 3 *) (* - 2 pour enlever incr d'apres + take_argument incr*) (* -3 ?? *)

      (* SWITCH *)

      | 88 (* BOOLNOT *) -> Mlvalues.acc := Prims.bnot (!Mlvalues.acc)
      | 89 (* PUSHTRAP *) -> let ofs = take_argument code in
        push_stack (Mlvalues.val_long !extra_args);
        push_stack !Mlvalues.env;
        push_stack (Mlvalues.val_long !trap_sp);
        push_stack (Mlvalues.val_long (!pc - 1 + ofs)); (* -1 car on a prit un argument *)
        trap_sp := !Mlvalues.sp
      | 90 (* POPTRAP *) -> 
        let _ = pop_stack () in
        trap_sp := Mlvalues.long_val (pop_stack ());
        let _ = pop_stack () in
        let _ = pop_stack () in ()
      | 91 (* RAISE *) -> 
        if !trap_sp = 0 then begin 
          print_string "Exception, acc = ";
          print_int (Mlvalues.long_val !Mlvalues.acc);
          print_newline () end 
        else begin
          Mlvalues.sp := !trap_sp;
          pc := Mlvalues.long_val (pop_stack ());
          trap_sp := Mlvalues.long_val (pop_stack ());
          Mlvalues.env := pop_stack ();
          extra_args := Mlvalues.long_val (pop_stack ())
        end
      | 92 (* CHECK-SIGNALS *) -> ()
      | 93 (* C-CALL1 *) ->

        (*** placer dans le fichier source (.ml):       ***)
        (*** external print_int : int -> unit = "fake0" ***)
        let p = take_argument code in
        push_stack (!Mlvalues.env);
        (match p with
         | 0 -> print_int (Mlvalues.long_val !Mlvalues.acc); push_stack Mlvalues.unit
         (* indice de la primitive -> code de la primitive *)
        );
        let _ = pop_stack () in ()
      | 94 (* C-CALL2 *) -> 
        let p = take_argument code in
        print_string "primitive numéro " ; print_int p; print_newline () ;
        let x = pop_stack () in
        push_stack (!Mlvalues.env);
        (* (match p with ...) *)
        (match p with
         | _ -> (* Array.make *)
           let n = Mlvalues.long_val !Mlvalues.acc in
           let a = Mlvalues.make_block 0 n in (* tag 0 *)
           for i = 0 to n - 1 do
             Mlvalues.set_field a i x 
           done;
           pop_stack ();
           pop_stack();
           push_stack a
        )

      | 95 (* C-CALL3 *) ->
        let p = take_argument code in
        let x1 = pop_stack () in
        let x2 = pop_stack () in   
        push_stack (!Mlvalues.env);
        (* (match p with ...) *)      
        for i = 0 to 3 do let _ = pop_stack () in () done
      | 96 (* C-CALL4 *) -> 
        let p = take_argument code in
        let x1 = pop_stack () in
        let x2 = pop_stack () in 
        let x3 = pop_stack () in   
        push_stack (!Mlvalues.env);
        (* (match p with ...) *)      
        for i = 0 to 4 do let _ = pop_stack () in () done
      | 97 (* C-CALL5 *) -> 
        let p = take_argument code in
        let x1 = pop_stack () in
        let x2 = pop_stack () in 
        let x3 = pop_stack () in   
        let x4 = pop_stack () in   
        push_stack (!Mlvalues.env);
        (* (match p with ...) *)      
        for i = 0 to 4 do let _ = pop_stack () in () done
      | 98 (* C-CALLN *) -> (* TODO *) ()
      | 99  (* CONST0 *) -> Mlvalues.acc := Mlvalues.val_long 0
      | 100 (* CONST1 *) -> Mlvalues.acc := Mlvalues.val_long 1
      | 101 (* CONST2 *) -> Mlvalues.acc := Mlvalues.val_long 2
      | 102 (* CONST3 *) -> Mlvalues.acc := Mlvalues.val_long 3
      | 103 (* CONSTINT *) -> 
        let n = take_argument code in 
        Mlvalues.acc := Mlvalues.val_long n
      | 104 (* PUSHCONST0 *) -> push_stack (!Mlvalues.acc);
        Mlvalues.acc := (Mlvalues.val_long 0)
      | 105 (* PUSHCONST1 *) -> push_stack (!Mlvalues.acc);
        Mlvalues.acc := (Mlvalues.val_long 1)
      | 106 (* PUSHCONST2 *) -> push_stack (!Mlvalues.acc);
        Mlvalues.acc := (Mlvalues.val_long 2)
      | 107 (* PUSHCONST3 *) -> push_stack (!Mlvalues.acc);
        Mlvalues.acc := (Mlvalues.val_long 3)
      | 108 (* PUSHCONSTINT *) -> let n = take_argument code in
        push_stack (!Mlvalues.acc);
        Mlvalues.acc := (Mlvalues.val_long n)
      | 109 (* NEGINT *) -> Mlvalues.acc := Prims.negint (!Mlvalues.acc)
      | 110 (* ADDINT *) -> Mlvalues.acc := Prims.addint (!Mlvalues.acc) (pop_stack ()) 
      | 111 (* SUBINT *) -> Mlvalues.acc := Prims.subint (!Mlvalues.acc) (pop_stack ()) 
      | 112 (* MULINT *) -> Mlvalues.acc := Prims.mulint (!Mlvalues.acc) (pop_stack ()) 
      | 113 (* DIVINT *) -> Mlvalues.acc := Prims.divint (!Mlvalues.acc) (pop_stack ()) 
      | 114 (* MODINT *) -> Mlvalues.acc := Prims.modint (!Mlvalues.acc) (pop_stack ()) 
      | 115 (* ANDINT *) -> Mlvalues.acc := Prims.andint (!Mlvalues.acc) (pop_stack ()) 
      | 116 (* ORINT  *) -> Mlvalues.acc := Prims.orint  (!Mlvalues.acc) (pop_stack ()) 
      | 117 (* XORINT *) -> Mlvalues.acc := Prims.xorint (!Mlvalues.acc) (pop_stack ()) 
      | 118 (* LSLINT *) -> Mlvalues.acc := Prims.lslint (!Mlvalues.acc) (pop_stack ()) 
      | 119 (* LSRINT *) -> Mlvalues.acc := Prims.lsrint (!Mlvalues.acc) (pop_stack ()) 
      | 120 (* ASRINT *) -> Mlvalues.acc := Prims.asrint (!Mlvalues.acc) (pop_stack ()) 
      | 121 (* EQ     *) -> Mlvalues.acc := Prims.eq     (!Mlvalues.acc) (pop_stack ()) 
      | 122 (* NEQ    *) -> Mlvalues.acc := Prims.neq    (!Mlvalues.acc) (pop_stack ()) 
      | 123 (* LTINT  *) -> Mlvalues.acc := Prims.ltint  (!Mlvalues.acc) (pop_stack ()) 
      | 124 (* LEINT  *) -> Mlvalues.acc := Prims.leint  (!Mlvalues.acc) (pop_stack ()) 
      | 125 (* GTINT  *) -> Mlvalues.acc := Prims.gtint  (!Mlvalues.acc) (pop_stack ()) 
      | 126 (* GEINT  *) -> Mlvalues.acc := Prims.geint  (!Mlvalues.acc) (pop_stack ()) 
      | 127 (* OFFSETINT *) -> let ofs = take_argument code in 
        Mlvalues.acc := Prims.addint (!Mlvalues.acc) (Mlvalues.val_long ofs)
      | 128 (* OFFSETREF *) -> let ofs = take_argument code in
        let old = Mlvalues.get_field (!Mlvalues.acc) 0 in
        Mlvalues.set_field (!Mlvalues.acc) 0 (Prims.addint old (Mlvalues.val_long ofs));
        Mlvalues.acc := Mlvalues.unit
      | 129 (* ISINT *) -> Mlvalues.acc := Prims.isint (!Mlvalues.acc) 
      | 130 (* GETMETHOD *) -> let x = pop_stack () in (* [[[[[[[[[[[[ à verifier ]]]]]]]]]]]] *)
        let y = Mlvalues.get_field x 0 in
        Mlvalues.acc := Mlvalues.get_field y (Mlvalues.long_val (!Mlvalues.acc))
      | 131 (* BEQ *) ->
        let v = take_argument code in
        let ofs = take_argument code in
        if  Prims.compare_imm (Mlvalues.val_long v) (!Mlvalues.acc) = 0 then pc := ofs - 1
      | 132 (* BNEQ *) -> 
        let v = take_argument code in
        let ofs = take_argument code in
        if Prims.compare_imm (Mlvalues.val_long v) (!Mlvalues.acc) <> 0 then pc := ofs - 1 (* pc := (!pc) + ofs - 1 *)
      | 133 (* BLTINT *) ->
        let v = take_argument code in
        let ofs = take_argument code in
        if Prims.compare_imm (Mlvalues.val_long v) (!Mlvalues.acc) < 0 then pc := ofs - 1
      | 134 (* BLEINT *) ->
        let v = take_argument code in
        let ofs = take_argument code in
        if Prims.compare_imm (Mlvalues.val_long v) (!Mlvalues.acc) <= 0 then pc := ofs - 1
      | 135 (* BGTINT *) ->
        let v = take_argument code in
        let ofs = take_argument code in
        if Prims.compare_imm (Mlvalues.val_long v) (!Mlvalues.acc) > 0 then pc := ofs - 1
      | 136 (* BGEINT *) ->
        let v = take_argument code in
        let ofs = take_argument code in
        if Prims.compare_imm (Mlvalues.val_long v) (!Mlvalues.acc) >= 0 then pc := ofs - 1
      | 137 (* ULTINT *) -> Mlvalues.acc := Prims.ultint (!Mlvalues.acc) (pop_stack ()) 
      | 138 (* UGEINT *) -> Mlvalues.acc := Prims.ugeint (!Mlvalues.acc) (pop_stack ())
      | 139 (* BULTINT *) -> let v = take_argument code in 
        let ofs = take_argument code in
        if Mlvalues.long_val (Prims.ultint (Mlvalues.val_long v) (!Mlvalues.acc)) = 1 then pc := ofs - 1

      | 140 (* BUGEINT *) -> let v = take_argument code in 
        let ofs = take_argument code in
        if Mlvalues.long_val (Prims.ugeint (Mlvalues.val_long v) (!Mlvalues.acc)) = 1 then pc := ofs - 1
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
  (* debug_print_stack (); *)
  print_newline ()



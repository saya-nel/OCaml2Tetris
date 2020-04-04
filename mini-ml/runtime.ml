let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let mv f dst =
  let s = load_file f in 
  let oc = open_out dst in
  Printf.fprintf oc "%s" s;
  close_out oc

let link_test_file dir = 
  let oc = open_out (Filename.concat dir ("Main.tst")) in
   Printf.fprintf oc "%s\n" "load, output-file Main.out, output-list RAM[12]%D1.6.1;
  repeat 2500000 { vmstep; }";
  close_out oc

let link_runtime dir = 
  mv "stdlib/ML_internal.vm" (Filename.concat dir "ML_internal.vm");
  mv "stdlib/ML_array.vm" (Filename.concat dir "ML_array.vm");
  mv "stdlib/ML_pervasives.vm" (Filename.concat dir "ML_pervasives.vm");
  mv "stdlib/ML_string.vm" (Filename.concat dir "ML_string.vm")

 
  let init dir = 
    link_test_file dir;
    link_runtime dir


 
 module PrimTypes = struct
  open Types

  let v () =
    Tvar (V.create ())

  let ty_internal_pair =
    let a,b = v (),v () in 
    Tarrow(a,Tarrow(b,Tproduct(a,b)))

  let ty_internal_cons = 
    let a = v () in 
    Tarrow(a,Tarrow(Tlist a,Tlist a))

  let ty_internal_fst = 
    let a,b = v (),v () in 
    Tarrow(Tproduct(a,b),a)

  let ty_internal_snd = 
   let a,b = v (), v() in 
   Tarrow(Tproduct(a,b),a)

  let ty_exit = 
    Tarrow(Tint,v())

  let ty_failwith =
   Tarrow(Tstring,v ())

  let ty_ref = 
    let a = v () in 
    Tarrow(a,Tref a)

  let ty_ref_contents = 
    let a = v () in 
    Tarrow(Tref a,a)

  let ty_ref_set_contents = 
    let a = v () in 
    Tarrow(Tref a,(Tarrow(a,Tunit)))

  let ty_incr = 
    Tarrow(Tref (v ()),Tunit)

  let ty_decr = 
    Tarrow(Tref (v ()),Tunit)

  let ty_fst = 
    ty_internal_fst

  let ty_snd = 
    ty_internal_snd

  let ty_hd = 
    let a = v () in 
      Tarrow(Tlist a,a)

  let ty_tl = 
    let a = v () in 
      Tarrow(Tlist a,Tlist a)

  let ty_print_char =
     Tarrow(Tchar,Tunit)

  let ty_print_string = 
    Tarrow(Tstring,Tunit)

  let ty_print_int =
     Tarrow(Tint,Tunit)

  let ty_print_newline =
     Tarrow(Tunit,Tunit)

  let ty_array_length = 
    Tarrow(Tarray (v ()),Tint)

  let ty_array_set = 
    let a = v () in
    Tarrow(Tarray a,Tarrow(Tint,Tarrow(a,Tunit)))

  let ty_array_get = 
    let a = v () in
    Tarrow(Tarray a,Tarrow(Tint,a))

  let ty_array_make =
    let a = v () in
    Tarrow(Tint,Tarrow(a,Tarray a))

  let ty_array_create_uninitialized =
    Tarrow(Tint,Tarray (v ()))

  let ty_string_length =
    Tarrow(Tstring,Tint)

  let ty_string_get =
    Tarrow(Tstring,Tarrow(Tint,Tchar))

  let ty_string_set =
    Tarrow(Tstring,Tarrow(Tint,Tarrow(Tchar,Tunit)))
end

let primitives =
  let open PrimTypes in
  let ml_internal = 
  [("__internal.pair", "ML_internal.make_pair",          ty_internal_pair);
   ("__internal.cons", "ML_internal.make_pair",          ty_internal_cons);
   ("__internal.fst",  "ML_internal.fst",                ty_internal_fst);
   ("__internal.snd",  "ML_internalasives.snd",          ty_internal_snd) ] in
  let openned_ml_pervasives =
  [("exit",             "ML_pervasives.exit",             ty_exit);
   ("failwith",         "ML_pervasives.failwith",         ty_failwith);
   ("ref",              "ML_pervasives.ref",              ty_ref);
   ("ref_contents",     "ML_pervasives.ref_contents",     ty_ref_contents);
   ("ref_set_contents", "ML_pervasives.ref_set_contents", ty_ref_set_contents);
   ("incr",             "ML_pervasives.incr",             ty_incr);
   ("decr",             "ML_pervasives.decr",             ty_decr);
   ("fst",              "ML_pervasives.fst",              ty_fst);
   ("snd",              "ML_pervasives.snd",              ty_snd);
   ("hd",               "ML_pervasives.hd",               ty_hd);
   ("tl",               "ML_pervasives.tl",               ty_tl);
   ("print_char",       "ML_pervasives.print_char",       ty_print_char);
  ("print_string",      "ML_pervasives.print_string",     ty_print_string); 
   ("print_int",        "ML_pervasives.print_int",        ty_print_int);
   ("print_newline",    "ML_pervasives.print_newline",    ty_print_newline) ] in
  let ml_pervasives = List.map (fun (f,c,ty) -> ("Pervasives." ^ f,c,ty)) openned_ml_pervasives in
  let ml_array =
   [("Array.length",    "ML_array.length",                 ty_array_length);
    ("Array.set",       "ML_array.set",                    ty_array_set);
    ("Array.get",       "ML_array.get",                    ty_array_get); 
    ("Array.make",      "ML_array.make",                   ty_array_make);
    ("Array.create_uninitialized","ML_array.create_uninitialized", ty_array_create_uninitialized); ] in
  let ml_string = 
    [("String.length",  "ML_string.length",                ty_string_length);
     ("String.get",     "ML_string.get",                   ty_string_get);
     ("String.make",    "ML_string.make",                  ty_string_set)] in
  ml_internal @ openned_ml_pervasives @ ml_pervasives @ ml_array @ ml_string
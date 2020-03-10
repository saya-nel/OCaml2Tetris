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
  mv "stdlib/ML_array.vm" (Filename.concat dir "ML_array.vm");
  mv "stdlib/ML_pervasives.vm" (Filename.concat dir "ML_pervasives.vm");
  mv "stdlib/ML_string.vm" (Filename.concat dir "ML_string.vm")


let init dir = 
  link_test_file dir;
  link_runtime dir

let primitives () =
  let openned_ml_pervasives =
  ["exit",("ML_pervasives.exit");
   "ref",("ML_pervasives.ref");
   "ref_contents",("ML_pervasives.ref_contents");
   "ref_set_contents",("ML_pervasives.ref_set_contents");
   "incr",("ML_pervasives.incr");
   "decr",("ML_pervasives.decr");
   "print_char",("ML_pervasives.print_char");
   "print_string",("ML_pervasives.print_string");
   "print_int", ("ML_pervasives.print_int") ] in
  let ml_pervasives = 
    ["Pervasives.exit",("ML_pervasives.exit");
     "Pervasives.ref",("ML_pervasives.ref");
     "Pervasives.ref_contents",("ML_pervasives.ref_contents");
     "Pervasives.ref_set_contents",("ML_pervasives.ref_set_contents");
     "Pervasives.incr",("ML_pervasives.incr");
     "Pervasives.decr",("ML_pervasives.decr");
     "Pervasives.print_char",("ML_pervasives.print_char");
     "Pervasives.print_string",("ML_pervasives.print_string");
     "Pervasives.print_int", ("ML_pervasives.print_int") ] in
  let ml_array =
   ["Array.set", ("ML_array.set");
   "Array.get", ("ML_array.get");
   "Array.make", ("ML_array.make");
   "Array.create_uninitialized",("ML_array.create_uninitialized") ] in
  let ml_string = 
    ["String.get", ("ML_string.get");
     "String.make", ("ML_string.make")] in
  openned_ml_pervasives @ ml_pervasives @ ml_array @ ml_string
     
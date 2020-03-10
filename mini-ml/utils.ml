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
  mv "stdlib/Array.vm" (Filename.concat dir "Array.vm");
  mv "stdlib/Pervasives.vm" (Filename.concat dir "Pervasives.vm");
  mv "stdlib/String.vm" (Filename.concat dir "String.vm")
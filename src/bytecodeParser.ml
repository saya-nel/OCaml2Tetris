open String
open Str

let () =
  try 
    while true do 
      let instr = string_after ( trim (read_line ())) 3 in
      print_endline instr
    done
  with End_of_file -> ()
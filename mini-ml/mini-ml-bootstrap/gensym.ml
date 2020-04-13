
let create n =
  ref n

let next prefix gen = 
  incr gen;
  (^) prefix (string_of_int (!gen))


let next_int gen =
	incr gen;
    (!gen)
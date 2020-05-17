let r = ref (7,8) in

let f x =
  for i = 0 to 100 do
    r := (i,i);
  done
in f
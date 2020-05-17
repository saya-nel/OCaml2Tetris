let rec f x =
  let y = (1,2) in
  if x ==  0 then 42
  else f (x-1)
in 
f 1000
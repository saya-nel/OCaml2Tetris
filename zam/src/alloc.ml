(* mlvalues makebloc / makeclosure *)

let make_block tag sz =
  let sz = if sz = 0 then 1 else sz in
  let a = Gc.alloc (sz + 1) in
  (!Domain.from_space).(a) <- Mlvalues.val_long (tag + 256 * sz);
  Mlvalues.val_ptr a

let make_closure pc size =
  let res = make_block Block.closure_tag size in
  Block.set_field res 0 (Mlvalues.val_long pc);
  res
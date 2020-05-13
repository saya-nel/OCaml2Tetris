let heap_size = 100

let heap = Array.make heap_size 0

let heap_top = ref 0

let alloc size = 
  let res = heap_top in
  heap_top := (!heap_top) + size;
  !res 
let mapcat sep f l = String.concat sep (List.map f l)

let sptf = Printf.sprintf
let put = sptf
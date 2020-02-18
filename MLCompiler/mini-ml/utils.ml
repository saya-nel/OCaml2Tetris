type name = string
type value = int

let prtf = Printf.sprintf


let mapcat s f l = String.concat s (List.map f l)
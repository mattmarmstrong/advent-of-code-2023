open Core 

let split ~char = Str.split (Str.regexp char) 

let get_split_element_at_index str ~char ~index = 
  let str = split ~char str in 
    Core.List.nth_exn str index

let unwrap opt = match opt with 
  | Some(value) -> value 
  | None -> failwith "Encountered None, Panicking!"

let filter_chars ~chars = String.filter ~f: (fun char -> not (String.mem chars char))

let rec greatest_common_div x y = 
  if y = 0 then x else greatest_common_div y (x mod y)

(* Shamelessly pulled from Stack Overflow *)
let least_common_multiple x y =
  match x, y with
  | 0, _ | _, 0 -> 0
  | x, y -> abs (x * y) / (greatest_common_div x y)

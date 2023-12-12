open Core 

let split ~char = Str.split (Str.regexp char) 

let get_split_element_at_index str ~char ~index = 
  let str = split ~char str in 
    Core.List.nth_exn str index

let unwrap opt = match opt with 
  | Some(value) -> value 
  | None -> failwith "Encountered None, Panicking!"

let filter_chars ~chars = String.filter ~f: (fun char -> not (String.mem chars char))

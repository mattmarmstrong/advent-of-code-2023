open Core 

let split ~char = Str.split (Str.regexp char) 

let get_split_element_at_index str ~char ~index = 
  let str = split ~char str in 
    List.nth_exn str index

open Core

let input_file = "./input.txt"

let filter_non_ints str = String.to_list str |> List.filter ~f:(Char.is_digit)

let rec get_first_and_last_element list =
  match list with 
    | [] -> failwith "Something went wrong!"
    | [x] -> (x, x)
    | [x; y] -> (x, y)
    | x :: _ :: cdr -> get_first_and_last_element (x :: cdr) 

let join_tuple_of_numeric_chars (x, y) = String.of_char_list [x; y] |> int_of_string

let transform_function input_line = 
  let filtered_line = filter_non_ints input_line in
    let (first, last) = get_first_and_last_element filtered_line in
      join_tuple_of_numeric_chars (first, last)    

let rec sum list =
  match list with 
  | [] -> 0
  | car::cdr -> car + sum cdr


let solve_part_1 input = 
  let input_lines = In_channel.read_lines input in 
    let transformed_list = List.map input_lines ~f: (transform_function) in 
      sum transformed_list 


let () =
  let calibration_value = solve_part_1 input_file in 
    printf "Solution: Part 1 = %d\n" calibration_value

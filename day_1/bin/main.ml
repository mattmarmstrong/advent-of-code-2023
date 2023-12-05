open Core

let input_file = "./input.txt"

(** START Part 1 *)

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

(** END Part 1 *)

(** START Part 2 *)

let word_to_digit_char_map word = 
  match word with
    | "one" -> "1"
    | "two" -> "2"
    | "three" -> "3"
    | "four" -> "4"
    | "five" -> "5"
    | "six" -> "6"
    | "seven"-> "7"
    | "eight"-> "8"
    | "nine" -> "9"
    | _ -> word

let string_regex = "[0-9]\\|one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine" 

let sum_2 = ref 0 

let rec solve_part_2 input_list =
    match input_list with 
    | [] -> ()
    | car :: cdr -> (
    let regex =  Str.regexp string_regex in 
    let _ = String.get car (Str.search_forward regex car 0) in 
        let first = Str.matched_string car in 
          let _ = String.get car (Str.search_backward regex car (String.length car)) in 
            let last = Str.matched_string car in 
            sum_2 := !sum_2 + int_of_string ((word_to_digit_char_map first) ^ (word_to_digit_char_map last));
            solve_part_2 cdr
  )

(** END Part 2 *)

let () =
  let calibration_value_pt1 = solve_part_1 input_file in 
  printf "Solution: Part 1 = %d\n" calibration_value_pt1;
  solve_part_2 (In_channel.read_lines input_file);
  printf "Solution: Part 1 = %d\n" !sum_2;

open Core

let input_file = "./input.txt"

type game = { winning_numbers: int list; your_numbers: int list; }

(* TODO: I really need to move these utility functions into their own library *)

let split ~char = Str.split (Str.regexp char)

let get_split_element_at_index str ~char ~index = 
  let str = split ~char str in 
    List.nth_exn str index

let string_of_opt string_opt =
  match string_opt with 
    | Some(str) -> str
    | None -> failwith "Parse error!"

let parse_card string = split ~char: " +" string |> List.map ~f: int_of_string

let parse_game game_string = 
  let split_cards_list = get_split_element_at_index game_string ~char: ": +" ~index: 1 
  |> split ~char: " | " in
    let winners = List.hd split_cards_list |> string_of_opt in 
      let yours = List.rev split_cards_list |> List.hd |> string_of_opt in
  { winning_numbers = parse_card winners; your_numbers = parse_card yours; }

let count_matches game =  
  let winning_numbers_set = Set.of_list (module Int) game.winning_numbers in 
    List.filter game.your_numbers ~f: (Set.mem winning_numbers_set) |> List.length

let solve_part_1 input = In_channel.read_lines input
  |> List.map ~f: parse_game
  |> List.map ~f: count_matches
  |> List.filter ~f: (fun score -> score > 0)
  |> List.sum (module Int) ~f: (fun score -> Int.pow 2 (score - 1))
  

let () = 
  let part_1_point_total = solve_part_1 input_file in 
    printf "Solution: Part 1 = %d\n" part_1_point_total

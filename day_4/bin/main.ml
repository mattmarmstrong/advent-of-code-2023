open Core
open Util

let input_file = "day_4/input.txt"

type game = { winning_numbers: int list; your_numbers: int list; }

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
  |> List.sum (module Int) ~f: (fun score -> Int.shift_left 1 (score - 1))

let array_get_opt array index = 
  if index < 0 || index > Array.length array then None else Some (Array.unsafe_get array index)

let solve_part_2 input = 
  let games_array = In_channel.read_lines input |> List.map ~f: parse_game |> Array.of_list in 
    let total_copies =   
      Memo.recursive ~hashable: Int.hashable (fun total_copies i ->
        match array_get_opt games_array i with
        | Some (game) ->
            let direct_matches = count_matches game in
              let indirect_matches =
                List.range ~start:`exclusive ~stop:`inclusive i (i + direct_matches)
                |> List.map ~f: total_copies
                |> List.sum (module Int) ~f:Fn.id
              in
              direct_matches + indirect_matches
        | None -> 0)
          in Array.mapi games_array ~f:(fun i _ -> 1 + total_copies i) |> Array.sum (module Int) ~f:Fn.id
  
let () = 
  let part_1_point_total = solve_part_1 input_file in 
  printf "Solution: Part 1 = %d\n" part_1_point_total;
  let part_2_point_total = solve_part_2 input_file in 
  printf "Solution: Part 2 = %d\n" part_2_point_total;

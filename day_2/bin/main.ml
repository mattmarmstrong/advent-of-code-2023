open Core

let input_file = "./input.txt"

type round_row = { red: int option; green: int option; blue: int option;}
type game = { id: int; rounds: round_row list}

let unwrap_opts option = 
  match option with 
    | Some(value) -> value
    | None -> 0  

let try_first_match ~regex string = 
  try
    let _ = Str.search_forward (regex) string 0 in
      Some (Str.matched_group 1 string)
  with _ -> None

let split ~char = Str.split (Str.regexp char) 

let get_split_element_at_index str ~char ~index = 
  let str = split ~char str in 
    List.nth_exn str index

let valid_round round = 
    match ((unwrap_opts round.red <= 12), (unwrap_opts round.green <= 13), (unwrap_opts round.blue <= 14)) with 
    | (true, true, true) -> true
    | (_, _, _) -> false 

let colour_regex colour = Str.regexp ("\\([0-9]+\\) " ^ colour)
let game_regex = Str.regexp "Game \\([0-9]+\\):"

let parse_round round_string = 
  let get_colour colour = 
    try_first_match ~regex: (colour_regex colour) round_string |> Option.map ~f: int_of_string in 
      { red = get_colour "red"; green = get_colour "green"; blue = get_colour "blue" }

let parse_game game_string =  
  let id = try_first_match ~regex: game_regex game_string |> Option.map ~f: int_of_string in 
    let rounds_list = get_split_element_at_index game_string ~char: ": " ~index: 1 |> split ~char: ";" |> List.map ~f: parse_round in
      {id = unwrap_opts id; rounds = rounds_list}

let solve_part_1 input = 
  In_channel.read_lines input
    |> List.map ~f: parse_game 
    |> List.filter ~f: (fun { rounds; _ } -> List.for_all rounds ~f: valid_round)
    |> List.sum (module Int) ~f: (fun { id; _ } -> id)

let max_of_int_opt_list int_opt_list = 
  List.filter_opt int_opt_list
  |> List.max_elt ~compare: Int.compare
  |> unwrap_opts 

let solve_part_2 input = 
  In_channel.read_lines input 
  |> List.map ~f: parse_game
  |> List.sum (module Int) ~f: (fun { rounds; _ }  -> 
    let reds_list, greens_list, blues_list = List.map rounds ~f: (fun { red; green; blue } -> (red, green, blue)) 
      |> List.unzip3 in 
      max_of_int_opt_list reds_list * max_of_int_opt_list greens_list * max_of_int_opt_list blues_list
  )

let () =  
  let game_ids_sum = solve_part_1 input_file in 
    printf "Solution: Part 1 = %d\n" game_ids_sum; 
  let minimum_pieces_power_sum = solve_part_2 input_file in 
    printf "Solution: Part 2 = %d\n" minimum_pieces_power_sum;

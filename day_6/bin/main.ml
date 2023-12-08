open Core
open Util

let input_file = "day_6/input.txt"

type race = { race_duration: int; distance_record: int; }

let parse input = 
  let race_duration_and_dists_list = In_channel.read_lines input 
  |> List.map ~f: (fun input_line -> get_split_element_at_index input_line ~char: ": +" ~index: 1)
  |> List.map ~f: (fun input_line -> split ~char: " +" input_line)
  |> List.map ~f: (fun input_line -> List.map ~f: int_of_string input_line) in 
    match race_duration_and_dists_list with 
    | [ times; dists; ] -> List.zip_exn times dists |> List.map ~f: (fun (time, dist) -> { race_duration = time; distance_record = dist; })
    | _ -> failwith "Parse error!"

let additional_part_2_parse races_list = 
    let accumulated_race_duration_str = List.fold_left ~init: "" ~f: (fun acc { race_duration; _ } -> acc ^ (string_of_int race_duration)) races_list in 
      let accumulated_distance_record_str = List.fold_left ~init: "" ~f: (fun acc { distance_record; _} -> acc ^ (string_of_int distance_record)) races_list in 
        {race_duration = int_of_string accumulated_race_duration_str; distance_record = int_of_string accumulated_distance_record_str}
 
let calculate_wins race = 
  List.range 0 race.race_duration 
  |> List.count ~f: (fun time_button_held -> race.distance_record <= (time_button_held * (race.race_duration - time_button_held)))

let solve_part_1 input = parse input |> List.map ~f: calculate_wins |> List.fold ~init: 1 ~f: (fun acc x -> acc * x)

let solve_part_2 input = parse input |> additional_part_2_parse |> calculate_wins

let () = 
  let part_1_solution = solve_part_1 input_file in 
  printf "Solution: Part 1 = %d\n" part_1_solution;
  let part_2_solution = solve_part_2 input_file in 
  printf "Solution: Part 2 = %d\n" part_2_solution;

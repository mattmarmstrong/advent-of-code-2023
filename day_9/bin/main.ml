open Core
open Util

let input_file = "day_9/input.txt"

let parse_row row = split ~char: " +" row |> List.map ~f: (int_of_string)

let rec list_pairs list = 
  match list with 
  | [] -> []
  | [ _ ] -> []
  | car :: cdr :: tl -> (car, cdr) :: list_pairs (cdr :: tl)  

let compute_diff pairs_list = 
  List.map ~f: (fun (a, b) -> (b - a)) pairs_list

let rec next_value row ~f = 
  let next_row = list_pairs row  |> compute_diff in 
    if List.for_all ~f: (Int.equal 0) row then 0 else f row (next_value next_row ~f)
  
let solve_part_1 input = In_channel.read_lines input 
  |> List.map ~f: parse_row
  |> List.sum (module Int) 
    ~f: (next_value ~f: (fun row next_value -> let last = List.last_exn row in last + next_value ))

let solve_part_2 input = In_channel.read_lines input
  |> List.map ~f: parse_row 
  |> List.sum (module Int)
    ~f: (next_value ~f: (fun row next_value -> let first = List.hd_exn row in first - next_value)) 


let () = 
  let solution_part_1 = solve_part_1 input_file in 
    printf "Solution: Part 1 = %d\n" solution_part_1;
  let solution_part_2 = solve_part_2 input_file in 
    printf "Solution: Part 2 = %d\n" solution_part_2;

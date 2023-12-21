open Core
open Util

let input_file = "day_10/input.txt"

let can_step_into mp (step: Coords.t)   = 
  let valid_steps = match mp with 
    | '|' -> [ Coords.make 0 1; Coords.make 0 (-1); ] 
    | '-' -> [ Coords.make 1 0; Coords.make (-1) 0; ]
    | '7' -> [ Coords.make 1 0; Coords.make 0 1; ] 
    | 'F' -> [ Coords.make (-1) 0; Coords.make 0 1; ]
    | 'L' -> [ Coords.make (-1) 0; Coords.make 0 (-1); ]
    | 'J' -> [ Coords.make 1 0; Coords.make 0 (-1); ]
    | _ -> [] 
  in List.mem valid_steps step ~equal: Coords.equal 

let move_through mp (step: Coords.t) = match mp with
  | '|' | '-' -> step
  | '7' -> Coords.inv step  
  | 'F' -> Coords.neg_inv step 
  | 'L' -> Coords.inv step 
  | 'J' -> Coords.neg_inv step 
  | '.' -> failwith "Reached ground!"
  | _ -> failwith "Input error!"

let max_dims map = (Array.length map, Array.length (map.(0)))

let find_start_coords map =
  let max_y, max_x = max_dims map in 
  let rec aux i j map = match map.(j).(i) with 
  | 'S' -> Coords.make i j 
  |  _ -> if i < (max_x - 1) then aux (i + 1) j map else if j < (max_y - 1) then aux 0 (j + 1) map else failwith "Start not found!" in 
  aux 0 0 map 

let get_adj_mp map coord = 
  let adj_coords = Coords.all_adj |> List.map ~f: (Coords.add coord) in 
  List.zip_exn (List.map ~f: (fun {x; y;} -> map.(y).(x)) adj_coords) Coords.all_adj
  |> List.filter ~f: (fun (mp, step) -> can_step_into mp step)
  
let parse_map input = 
  let map_list = In_channel.read_lines input |> List.map ~f: (String.to_list) in
  let map = Array.make_matrix ~dimx: (List.length (List.hd_exn map_list)) ~dimy: (List.length map_list) ' ' in 
  List.iteri ~f: (fun y list -> List.iteri ~f: (fun x ele -> map.(y).(x) <- ele ) list) map_list; map 

let walk_pipes map (start_coords: Coords.t) = 
  let coords_list = [ start_coords; ] in 
  let _, first_step = get_adj_mp map start_coords |> List.hd_exn in 
  let rec aux (acc: Coords.t) (next_step: Coords.t) steps coords_l =  
    let curr_coords = Coords.add acc next_step in 
    let curr_mp = map.(curr_coords.y).(curr_coords.x) in
    match curr_mp with 
    | 'S' -> (steps, coords_l)
    | _ -> aux curr_coords (move_through curr_mp next_step) (steps + 1) (curr_coords :: coords_l) in 
  aux start_coords first_step 1 coords_list
  
let horizontal_line (p: Coords.t) x = 
  let rec aux acc x' = match (x' < x) with 
    | false -> acc 
    | true -> let next_coord = Coords.make (x' + 1) p.y in aux (next_coord :: acc) (x' + 1)
  in aux [p] x

let is_inside_polygon map p max_x (polygon_coords: Coords.t list) = 
  if List.exists ~f: (fun p' -> Coords.equal p' p) polygon_coords then false else 
  let line = horizontal_line p max_x in 
  let rec aux acc (line': Coords.t list) = match line' with 
    | [] -> acc 
    | hd :: tl -> let mp = map.(hd.y).(hd.x) in 
    match List.exists ~f: (fun p -> Coords.equal p hd) polygon_coords with 
    | true -> if Char.equal mp '|' then aux (acc + 1) tl else aux acc tl 
    | false -> aux acc tl in 
    ((aux 0 line) % 2 = 1) 

let count_total_inside_pts map polygon_coords = 
  let max_y, max_x = max_dims map in 
  let rec aux acc y x = match (x < (max_x - 1)) with 
    | true -> if (is_inside_polygon map (Coords.make x y) max_x polygon_coords) then aux (acc + 1) y (x + 1) else aux acc y (x + 1)
    | false -> 
    match (y < (max_y - 1)) with
    | true -> aux acc (y + 1) 0 
    | false -> acc in 
  aux 0 0 0    

let solve_both_parts input = 
  let map = parse_map input in
  let start_coords = find_start_coords map in 
  let total_steps, polygon_coords = walk_pipes map start_coords in
  let part_1_solution = total_steps / 2 in 
  let part_2_solution = count_total_inside_pts map polygon_coords in 
  (part_1_solution, part_2_solution)

let () = 
  let solution_part_1, solution_part_2 = solve_both_parts input_file in
  printf "Solution: Part 1 = %d\n" solution_part_1;
  printf "Solution: Part 2 = %d\n" solution_part_2;

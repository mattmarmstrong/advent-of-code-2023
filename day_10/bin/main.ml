open Core

let input_file = "day_10/input.txt"

module Coords = struct 
  type t = { x: int; y: int; }

  let make x y = { x; y; }
  let all_adj = [make 1 0; make (-1) 0; make 0 1; make 0 (-1)]
  let equal p1 p2 = (p1.x = p2.x) && (p1.y = p2.y)
  let add p1 p2 = make (p1.x + p2.x) (p1.y + p2.y) 
  let inv p = make p.y p.x 
  let neg_inv p = make (-p.y) (-p.x) 
end 

let can_step_into mp (step: Coords.t)   = 
  let valid_steps = match mp with 
    | 'S' -> Coords.all_adj
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
  | 'S' -> Coords.make 0 0 
  | '.' -> failwith "Reached ground!"
  | _ -> failwith "Input error!"

let find_start_coords map = 
  let max_y = Array.length map in 
  let max_x = Array.length (map.(0)) in 
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
  let _, first_step = get_adj_mp map start_coords |> List.hd_exn in 
  let rec aux (acc: Coords.t) (next_step: Coords.t) acc_steps =  
    let curr_coords = Coords.add acc next_step in 
    let curr_mp = map.(curr_coords.y).(curr_coords.x) in
    match curr_mp with 
    | 'S' -> acc_steps 
    | _ -> aux curr_coords (move_through curr_mp next_step) (acc_steps + 1) in 
  aux start_coords first_step 1 

let solve_part_1 input = 
  let map = parse_map input in
  let start_coords = find_start_coords map in 
  ((walk_pipes map start_coords) / 2)
  
let () = 
  let solution_part_1 = solve_part_1 input_file in
  printf "Solution: Part 1 = %d\n" solution_part_1;

open Util

let input_file = "day_8/input.txt"

let parse_nodes_string node_network_string = 
  let cleaned_nodes_list = split ~char: ", +" node_network_string 
    |> List.map (filter_chars ~chars: "()") in 
      match cleaned_nodes_list with 
        | [ left_node; right_node; ] -> (left_node, right_node)
        | _ -> failwith "Parse error!"

let add_node_entry input_line node_tbl = 
    let split_list = split ~char: " = +" input_line in 
      let key = List.hd split_list  in 
        let value = (List.rev split_list |> List.hd |> parse_nodes_string) in 
          Hashtbl.add node_tbl key value

let get_next_key ~direction ~key ~node_tbl = 
  match direction with 
  | 'L' -> let left, _ = Hashtbl.find node_tbl key in left 
  | 'R' -> let _, right = Hashtbl.find node_tbl key in right 
  | _ -> failwith "Invalid direction!"

let rec find_destination_steps ~acc ~start ~cond ~node_tbl ~directions =
  let next, directions = Seq.uncons directions |> unwrap in 
    let next_node = get_next_key ~direction: next ~key: start ~node_tbl: node_tbl in 
    match (cond next_node) with
    | true -> acc + 1
    | false -> 
      find_destination_steps 
        ~acc: (acc + 1) 
        ~start: next_node 
        ~cond: cond 
        ~node_tbl: node_tbl
        ~directions: directions 
    

let parse_and_build_map input =   
  let node_tbl = Hashtbl.create 1024 in 
    let input_lines = Core.In_channel.read_lines input in 
      match input_lines with 
      | directions :: _ :: nodes_list -> 
        List.iter (fun l -> add_node_entry l node_tbl) nodes_list;
        let directions = String.to_seq directions |> Seq.cycle in 
          (node_tbl, directions)
      | _ -> failwith "Parse error!" 

let solve_part_1 input = 
  let node_tbl, directions = parse_and_build_map input in 
    find_destination_steps 
      ~acc: 0 
      ~start: "AAA"
      ~cond: (String.equal "ZZZ")  
      ~node_tbl: node_tbl
      ~directions: directions 

(* START PART 2 *)

let filter_keys node_tbl = 
  Hashtbl.to_seq_keys node_tbl |> List.of_seq |> List.filter (String.ends_with ~suffix: "A")

let solve_part_2 input = 
  let node_tbl, directions = parse_and_build_map input in 
    let keys_list = filter_keys node_tbl in 
      List.map (fun key ->  
        find_destination_steps 
        ~acc: 0 
        ~start: key 
        ~cond: (String.ends_with ~suffix: "Z") 
        ~node_tbl: node_tbl 
        ~directions: directions 
      ) keys_list |> Core.List.reduce_exn ~f: least_common_multiple 

let () = 
  let solution_part_1 = solve_part_1 input_file in 
    Core.printf "Solution: Part 1 = %d\n" solution_part_1;
  let solution_part_2 = solve_part_2 input_file in 
    Core.printf "Solution: Part 2 = %d\n" solution_part_2;

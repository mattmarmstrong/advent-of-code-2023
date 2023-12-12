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

let rec find_destination ~acc ~start ~dest ~cond ~node_tbl ~directions =
  let next, directions = Seq.uncons directions |> unwrap in 
    let next_node = get_next_key ~direction: next ~key: start ~node_tbl: node_tbl in 
    match (cond next_node dest) with
    | true -> acc + 1
    | false -> find_destination ~acc: (acc + 1) ~start: next_node ~dest: dest ~cond: cond ~directions: directions ~node_tbl: node_tbl
    
let solve_part_1 input = 
  let node_tbl = Hashtbl.create 1024 in 
    let input_lines = Core.In_channel.read_lines input in 
      match input_lines with 
      | directions :: _ :: nodes_list -> 
        List.iter (fun l -> add_node_entry l node_tbl) nodes_list;
        let directions = String.to_seq directions |> Seq.cycle in 
          find_destination ~acc: 0 ~start: "AAA" ~dest: "ZZZ" ~cond: String.equal ~directions: directions ~node_tbl: node_tbl
      | _ -> failwith "Parse error!" 

let () = 
  let solution_part_1 = solve_part_1 input_file in 
    Core.printf "Solution: Part 1 = %d\n" solution_part_1;

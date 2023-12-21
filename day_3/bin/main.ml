open Core
open Util 

let input_file = "day_3/input.txt"

let parse input = 
  let lines = In_channel.read_lines input |> List.map ~f: (String.to_list) in 
  let x, y = (List.length (List.hd_exn lines), List.length lines) in 
  let map = Array.make_matrix ~dimx: x ~dimy: y ' ' in 
  List.iteri ~f: (fun j l -> List.iteri ~f: (fun i ele -> map.(j).(i) <- ele) l) lines; map 











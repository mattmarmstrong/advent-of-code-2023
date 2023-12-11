open Core
open Util

let input_file = "day_7/input.txt"

module Card = struct
  type t =
    | Joker [@value 1]
    | Two
    | Three 
    | Four 
    | Five 
    | Six 
    | Seven 
    | Eight 
    | Nine
    | Ten 
    | Jack
    | Queen 
    | King
    | Ace 
  [@@deriving enum, eq, ord, show] 

  let is_joker = equal Joker

  let from_char ?(jokers=false) char = 
    match char with  
    | 'A' -> Ace 
    | 'K' -> King
    | 'Q' -> Queen 
    | 'J' -> if jokers then Joker else Jack  
    | 'T' -> Ten 
    | _ ->  of_enum (Char.to_int char - Char.to_int '0') |> unwrap
end

module Hand = struct 
  type t = 
    | HighCard of Card.t list 
    | Pair of Card.t list 
    | TwoPair of Card.t list
    | ThreeOfAKind of Card.t list
    | FullHouse of Card.t list
    | FourOfAKind of Card.t list
    | FiveOfAKind of Card.t list
  [@@deriving eq, ord, show]

  let parse hand_string = 
    String.to_list hand_string |> List.map ~f: (Card.from_char) 
  
  let parse_with_jokers hand_string = 
    String.to_list hand_string |> List.map ~f: (Card.from_char ~jokers: true) 
  

  let from_card_list hand =
    let num_jokers = List.count ~f: (Card.is_joker) hand in 
    let grouped_cards_list = Containers.List.group_by ~eq: Card.equal hand 
    |> List.map ~f: List.length 
    |> List.sort ~compare: Int.compare 
    in match grouped_cards_list with 
    | [ 5;] -> FiveOfAKind hand 
    | [ 1; 4;] ->  if num_jokers = 0 then FourOfAKind hand else FiveOfAKind hand 
    | [ 2; 3;] -> if num_jokers = 0 then FullHouse hand else FiveOfAKind hand 
    | [ 1; 1; 3;] -> if num_jokers = 0 then ThreeOfAKind hand else FourOfAKind hand  
    | [ 1; 2; 2;] -> if num_jokers = 0 then TwoPair hand else if num_jokers = 1 then FullHouse hand else FourOfAKind hand  
    | [ 1; 1; 1; 2] -> if num_jokers = 0 then Pair hand else ThreeOfAKind hand 
    | _ -> if num_jokers = 0 then HighCard hand else Pair hand  
end 


(* TODO: Lots of pointless duplication here *)
let parse input_line =  
  let hand, bid = Scanf.sscanf input_line "%s %d" (fun s d -> (s, d)) in 
    let parsed_hand = Hand.parse hand |> Hand.from_card_list in 
      (parsed_hand, bid)

let solve_part_1 input = In_channel.read_lines input 
  |> List.map ~f: parse 
  |> List.sort ~compare: (fun (h, _) (h', _) -> Hand.compare h h')
  |> Containers.List.foldi (fun acc i (_, b) -> acc + ((i + 1) * b)) 0 

let parse_part_2 input_line = 
  let hand, bid = Scanf.sscanf input_line "%s %d" (fun s d -> (s, d)) in 
    let parsed_hand = Hand.parse_with_jokers hand |> Hand.from_card_list in 
      (parsed_hand, bid)

let solve_part_2 input = In_channel.read_lines input 
  |> List.map ~f: parse_part_2 
  |> List.sort ~compare: (fun (h, _) (h', _) -> Hand.compare h h')
  |> Containers.List.foldi (fun acc i (_, b) -> acc + ((i + 1) * b)) 0 


let () = 
  let part_1_solution = solve_part_1 input_file in 
    printf "Solution: Part 1 = %d\n" part_1_solution;
  let part_2_solution = solve_part_2 input_file in 
    printf "Solution: Part 1 = %d\n" part_2_solution;

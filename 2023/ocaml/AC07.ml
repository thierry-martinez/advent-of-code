type kind =
  | Five
  | Four
  | Full
  | Three
  | Two
  | One
  | High

module CharMap = Map.Make (Char)

let parikh_of_str str =
  String.fold_left (fun map c ->
    CharMap.update c (fun count ->
      Some (succ (Option.value ~default:0 count))) map)
    CharMap.empty str

type part =
  Part1 | Part2

let counts_of_parikh_vector parikh_vector =
  List.rev (List.sort Int.compare
    (List.map snd (CharMap.bindings parikh_vector)))

let kind_of_str ~part str =
  let parikh_vector = parikh_of_str str in
  let counts =
    match part with
    | Part1 -> counts_of_parikh_vector parikh_vector
    | Part2 ->
       let jokers, parikh_vector =
         match CharMap.find_opt 'J' parikh_vector with
         | Some jokers -> jokers, CharMap.remove 'J' parikh_vector
         | None -> 0, parikh_vector in
       match counts_of_parikh_vector parikh_vector with
       | best :: others -> best + jokers :: others
       | [] ->  [jokers] in
  match counts with
  | [5] -> Five
  | [4; 1] -> Four
  | [3; 2] -> Full
  | [3; 1; 1] -> Three
  | [2; 2; 1] -> Two
  | [2; 1; 1; 1] -> One
  | [1; 1; 1; 1; 1] -> High
  | _ -> assert false  

let hand_bid_of_str str =
  Scanf.sscanf str "%s %d" (fun hand bid -> (hand, bid))

let strength_of_card ~part card =
  match card with
  | 'A' -> 12
  | 'K' -> 11
  | 'Q' -> 10
  | 'J' ->
     begin match part with
     | Part1 -> 9
     | Part2 -> -1
     end
  | 'T' -> 8
  | '9' -> 7
  | '8' -> 6
  | '7' -> 5
  | '6' -> 4
  | '5' -> 3
  | '4' -> 2
  | '3' -> 1
  | '2' -> 0
  | _ -> assert false

let strength_of_str ~part str =
  List.of_seq (Seq.map (strength_of_card ~part) (String.to_seq str))

let hand_bid_compare ~part (hand1, _bid1) (hand2, _bid2) =
  let kind1 = kind_of_str ~part hand1 in
  let kind2 = kind_of_str ~part hand2 in
  match compare kind1 kind2 with
  | 0 ->
     let strength1 = strength_of_str ~part hand1 in
     let strength2 = strength_of_str ~part hand2 in
     List.compare Int.compare strength1 strength2
  | result ->
     - result

let eval ~part input =
  let hand_bids = List.of_seq (Seq.map hand_bid_of_str input) in
  let sorted_hands = List.sort (hand_bid_compare ~part) hand_bids in
  let _rank, sum =
    List.fold_left
      (fun (rank, sum) (_hand, bid) -> rank + 1, sum + rank * bid)
      (1, 0) sorted_hands in
  sum

let () =
  let input = Seq.memoize (Common.input_lines ()) in
  Format.eprintf "Part 1: %d@." (eval ~part:Part1 input);
  Format.eprintf "Part 2: %d@." (eval ~part:Part2 input)

module StringMap = Map.Make (String)

let parse input =
  let instructions, input = Option.get (Seq.uncons input) in
  let _, input = Option.get (Seq.uncons input) in
  let map =
    Seq.fold_left (fun map line ->
        let (key, left, right) =
          Scanf.sscanf line "%s = (%s@, %s@)"
            (fun key left right -> (key, left, right)) in
        StringMap.add key (left, right) map)
      StringMap.empty input in
  (instructions, map)

let follow direction map place =
  let (left, right) = StringMap.find place map in
  let place =
    match direction with
    | 'L' -> left
    | 'R' -> right
    | _ -> assert false in
  place

let part1 instructions map =
  let rec find index place instructions =
    if place = "ZZZ" then
      index
    else
      let (direction, instructions) =
        Option.get (Seq.uncons instructions) in
      let place = follow direction map place in
      find (succ index) place instructions in
  find 0 "AAA" (Seq.cycle (String.to_seq instructions))

let journey_length instructions map initial_place =
  let rec find step_count place instructions =
    if place.[2] = 'Z' then
       step_count
    else
      let ((_index, direction), instructions) =
        Option.get (Seq.uncons instructions) in
      let place = follow direction map place in
      find (succ step_count) place instructions in
  find 0 initial_place (Seq.cycle (String.to_seqi instructions))

(* Not a general solution, but crafted for the kind of inputs AoC give... *)
let part2 instructions map =
  let places =
    List.of_seq
      (Seq.filter_map (fun (key, _) -> if key.[2] = 'A' then Some key else None)
         (StringMap.to_seq map)) in
  let lengths = List.map (journey_length instructions map) places in
  List.fold_left Common.lcm 1 lengths

let () =
  let instructions, map = parse (Common.input_lines ()) in
  Format.eprintf "Part 1: %d@." (part1 instructions map);
  Format.eprintf "Part 2: %d@." (part2 instructions map)

let parse_cube_count cube_count_str =
  Scanf.sscanf (String.trim cube_count_str) "%d %s" (fun count color -> (count, color))

let parse_set set_str =
  let cube_count_str = String.split_on_char ',' set_str in
  List.map parse_cube_count cube_count_str

let parse_line id line_str =
  let sets_str = String.split_on_char ';' line_str in
  id, List.map parse_set sets_str

let parse_game game_str =
  Scanf.sscanf game_str "Game %d: %s@\n" parse_line

module StringMap = Map.Make (String)

let is_cube_count_possible maximum (count, color) =
  match StringMap.find_opt color maximum with
  | None -> false
  | Some maximum_count ->
     count <= maximum_count

let is_set_possible maximum set =
  List.for_all (is_cube_count_possible maximum) set

let is_game_possible maximum game =
  List.for_all (is_set_possible maximum) game

let sum seq =
  Seq.fold_left ( + ) 0 seq

let sum_ids maximum games =
  sum (Seq.map (fun game_str ->
    let (id, game) = parse_game game_str in
    if is_game_possible maximum game then
      id
    else
      0) games)

let input_lines () =
  Seq.of_dispenser (fun () ->
    try Some (input_line stdin)
    with End_of_file -> None)

let part1 input =
  let maximum = StringMap.of_list [
      ("red", 12);
      ("green", 13);
      ("blue", 14);
    ] in
  Format.eprintf "%d@." (sum_ids maximum input)

let minimum_set_of_cube_count (count, color) =
  StringMap.singleton color count

let union_minimum_sets sets =
  List.fold_left
    (StringMap.union (fun _color count1 count2 -> Some (max count1 count2)))
    StringMap.empty sets

let minimum_set_of_cube_set set =
  union_minimum_sets (List.map minimum_set_of_cube_count set)

let minimum_set_of_game game =
  union_minimum_sets (List.map minimum_set_of_cube_set game)

let power_of_game (_id, game) =
  StringMap.fold (fun _color count accu -> accu * count)
    (minimum_set_of_game game) 1

let part2 input =
  sum (Seq.map (fun game_str -> power_of_game (parse_game game_str)) input)

let () =
  let input = Seq.memoize (Common.input_lines ()) in
  Format.eprintf "Part 1: %d@." (part1 input);
  Format.eprintf "Part 2: %d@." (part2 input)

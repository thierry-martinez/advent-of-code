let count_array l p =
  let counter = ref 0 in
  Array.init l (fun i ->
    if p i then
      incr counter;
    !counter)

let expand_of_grid grid =
  let empty_rows =
    count_array (Array.length grid)
      (fun i -> Array.for_all (fun cell -> cell = '.') grid.(i)) in
  let empty_columns =
    count_array (Array.length grid.(0)) (fun j ->
      Seq.for_all (fun i -> grid.(i).(j) = '.')
        (Seq.init (Array.length grid) Fun.id)) in
  empty_rows, empty_columns

let find_galaxies grid =
  snd (Array.fold_left (fun (y, list) line ->
    y + 1, snd (Array.fold_left (fun (x, list) cell ->
      x + 1,
      if cell = '#' then
        (x, y) :: list
      else
        list) (0, list) line)) (0, []) grid)

let[@tail_mod_cons] rec tails list =
  list :: strict_tails list
and[@tail_mod_cons] strict_tails list =
  match list with
  | [] -> []
  | _ :: tl -> tails tl

let pairs_with_head list =
  match list with
  | [] -> []
  | hd :: tl -> List.map (fun item -> (hd, item)) tl

let pairs list =
  List.concat_map pairs_with_head (tails list)

let distance (empty_rows, empty_columns) factor (x, y) (x', y') =
  let empty_rows = abs (empty_rows.(y) - empty_rows.(y')) in
  let empty_columns = abs (empty_columns.(x) - empty_columns.(x')) in
  abs (x - x') + abs (y - y') + (empty_rows + empty_columns) * factor

let distances expand factor galaxies =
  List.fold_left ( + ) 0
    (List.map (fun (g1, g2) -> distance expand factor g1 g2) (pairs galaxies))

let () =
  let input = Seq.memoize (Common.input_lines ()) in
  let grid =
    Array.of_seq (Seq.map (fun s -> Array.of_seq (String.to_seq s)) input) in
  let expand = expand_of_grid grid in
  let galaxies = find_galaxies grid in
  Format.eprintf "Part 1: %d@." (distances expand 1 galaxies);
  Format.eprintf "Part 2: %d@." (distances expand (1000000 - 1) galaxies)

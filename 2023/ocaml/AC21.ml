let add_offset (x, y) (dx, dy) =
  (x + dx, y + dy)

let size matrix =
  (Array.length matrix.(0), Array.length matrix)

let in_bounds (width, height) (x, y) =
  x >= 0 && y >= 0 && x < width && y < height

let get grid (x, y) =
  grid.(y).(x)

let set grid (x, y) value =
  grid.(y).(x) <- value

let parse_line line =
  Array.of_seq (String.to_seq line)

module Position = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    match Int.compare x1 x2 with
    | 0 -> Int.compare y1 y2
    | c -> c
end

module PositionSet = Set.Make (Position)

let get_grid_part1 grid position =
  in_bounds (size grid) position && get grid position

let positive_mod a b =
  if a < 0 then
    b + (a + 1) mod b - 1
  else
    a mod b

let get_grid_part2 grid (x, y) =
  let (width, height) = size grid in
  let position = (
      positive_mod x width,
      positive_mod y height
    ) in
  get grid position

let add_position get_grid position set offset =
  let position' = add_offset position offset in
  if get_grid position' then
    PositionSet.add position' set
  else
    set

let next_of_position get_grid position =
  List.fold_left (add_position get_grid position) PositionSet.empty
    [(-1, 0); (0, -1); (1, 0); (0, 1)]

let next_of_position_set get_grid position_set =
  List.fold_left PositionSet.union PositionSet.empty
    (List.map (next_of_position get_grid) (PositionSet.elements position_set))

let rec iterate_until p get_grid count position_set =
  if p count position_set then
    count, position_set
  else
    iterate_until p get_grid (succ count)
      (next_of_position_set get_grid position_set)

let iterate_next get_grid position_set count =
  snd (iterate_until (fun count' _position_set -> count = count') get_grid 0
    position_set)

let () =
  let char_grid = Array.of_seq (Seq.map parse_line (Common.input_lines ())) in
  let gardener =
    Option.get
      (Array.find_mapi (fun y line ->
        Option.map (fun x -> (x, y)) (Array.find_index ((=) 'S') line))
         char_grid) in
  set char_grid gardener '.';
  let grid = Array.map (Array.map (( = ) '.')) char_grid in
  let initial = (PositionSet.singleton gardener) in
  let result_part1 =
    PositionSet.cardinal (iterate_next (get_grid_part1 grid) initial 64) in
  Printf.eprintf "Part 1: %d\n" result_part1;
  let height = Array.length grid in
  let accu = ref [] in
  let p count set =
    if count mod height = height / 2 then
      begin
        accu := PositionSet.cardinal set :: !accu;
        List.length !accu = 3
      end
    else
      false in
  let _ = iterate_until p (get_grid_part2 grid) 0 initial in
  let a, b, c =
    match !accu with
    | [c; b; a] -> a, b, c
    | _ -> assert false in
  let n = 26501365 / height in
  let result_part2 =
    (n * n * (a + c - 2 * b) + n * (4 * b - 3 * a - c) + 2 * a) / 2 in
  Printf.eprintf "Part 2: %d\n" result_part2

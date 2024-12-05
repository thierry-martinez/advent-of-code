type direction = Left | Right | Up | Down

module Coords = struct
  type t = {
      x : int;
      y : int;
    }

  let zero = { x = 0; y = 0 }

  let det a b = a.x * b.y - b.x * a.y

  let ( + ) a b = { x = a.x + b.x; y = a.y + b.y }

  let ( * ) k a = { x = k * a.x; y = k * a.y }

  let of_direction d =
    match d with
    | Left -> { x = -1; y = 0 }
    | Right -> { x = 1; y = 0 }
    | Up -> { x = 0; y = -1 }
    | Down -> { x = 0; y = 1 }
end

type line = {
    direction : direction;
    count : int;
  }

let parse_part1_direction c =
  match c with
  | 'R' -> Right
  | 'D' -> Down
  | 'L' -> Left
  | 'U' -> Up
  | _ -> failwith "parse_part1_direction"

let parse_part2_direction c =
  match c with
  | '0' -> Right
  | '1' -> Down
  | '2' -> Left
  | '3' -> Up
  | _ -> failwith "parse_part2_direction"

let parse_line line =
  Scanf.sscanf line "%c %d (#%5x%c)" (fun d1 c1 c2 d2 -> 
    { direction = parse_part1_direction d1; count = c1 },
    { direction = parse_part2_direction d2; count = c2 })

let add_line_point (accu, last_point) line =
  let point = Coords.(last_point + line.count * of_direction line.direction) in
  (last_point :: accu, point)

let add_point_to_area (area, previous) point =
  area + Coords.det previous point, point

let area lines =
  let (points, _last_point) =
    List.fold_left add_line_point ([], Coords.zero) lines in
  let twice_area =
    abs (fst (List.fold_left add_point_to_area (0, Coords.zero) points)) in
  let area = twice_area / 2 in
  let perimeter =
    List.fold_left ( + ) 0 (List.map (fun line -> line.count) lines) in
  area + perimeter / 2 + 1

let () =
  let lines = List.of_seq (Seq.map parse_line (Common.input_lines ())) in
  let result_part1 = area (List.map fst lines) in
  Printf.eprintf "Part 1: %d\n" result_part1;
  let result_part2 = area (List.map snd lines) in
  Printf.eprintf "Part 2: %d\n" result_part2

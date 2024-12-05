let add_offset (x, y) (dx, dy) =
  (x + dx, y + dy)

module Visited = struct
  type t = {
      position : int * int;
      direction : int * int;
    }

  let equal = ( = )

  let hash = Hashtbl.hash

  let next position direction =
    { position = add_offset position direction; direction }

  let initial = { position = (0, 0); direction = (1, 0) }
end

module VisitedTable = Hashtbl.Make (Visited)

let parse_line line =
  Array.of_seq (String.to_seq line)

let rec count_energized_aux grid energized set visited =
  if VisitedTable.mem set visited then
    0
  else
    let (x, y) = visited.position in
    if x < 0 || y < 0 || x >= Array.length grid.(0) || y >= Array.length grid then
      0
    else
      begin
        VisitedTable.add set visited ();
        let counter =
          if energized.(y).(x) then
            0
          else
            begin
              energized.(y).(x) <- true;
              1
            end in
        let (dx, dy) = visited.direction in
        let next =
          match grid.(y).(x) with
          | '.'  ->
             count_energized_aux grid energized set
               (Visited.next visited.position visited.direction)
          | '/' ->
             count_energized_aux grid energized set
               (Visited.next visited.position (-dy, -dx))
          | '\\' ->
             count_energized_aux grid energized set
               (Visited.next visited.position (dy, dx))
          | '-' ->
             if dy = 0 then
               count_energized_aux grid energized set
                 (Visited.next visited.position visited.direction)
             else
               count_energized_aux grid energized set
                 (Visited.next visited.position (-1, 0)) +
               count_energized_aux grid energized set
                 (Visited.next visited.position (1, 0))
          | '|' ->
             if dx = 0 then
               count_energized_aux grid energized set
                 (Visited.next visited.position visited.direction)
             else
               count_energized_aux grid energized set
                 (Visited.next visited.position (0, -1)) +
               count_energized_aux grid energized set
                 (Visited.next visited.position (0, 1))
          | _ -> assert false in
        counter + next
      end

let count_energized grid initial =
  let energized = Array.map (Array.map (fun _ -> false)) grid in
  count_energized_aux grid energized (VisitedTable.create 16) initial

let maximize_energized grid =
  let maximum = ref 0 in
  for i = 0 to Array.length grid - 1 do
    maximum := max !maximum (max
        (count_energized grid
          { position = (0, i); direction = (1, 0) })
        (count_energized grid
          { position = (Array.length grid.(0) - 1, i); direction = (-1, 0) })
    )
  done;
  for i = 0 to Array.length grid.(0) - 1 do
    maximum := max !maximum (max
        (count_energized grid
          { position = (i, 0); direction = (0, 1) })
        (count_energized grid
          { position = (i, Array.length grid - 1); direction = (0, -1) })
    );
  done;
  !maximum

let () =
  let grid = Array.of_seq (Seq.map parse_line (Common.input_lines ())) in
  let result_part1 = count_energized grid Visited.initial in
  Format.eprintf "Part 1: %d@." result_part1;
  let result_part2 = maximize_energized grid in
  Format.eprintf "Part 2: %d@." result_part2

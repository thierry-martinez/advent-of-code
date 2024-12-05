let get grid transform_coords coords =
  let (i, j) = transform_coords coords in
  grid.(i).(j)

let set grid transform_coords coords value =
  let (i, j) = transform_coords coords in
  grid.(i).(j) <- value

let rec drop_rounded_rocks grid transform_coords column row count =
  if count > 0 then
    begin
      set grid transform_coords (row, column) 'O';
      drop_rounded_rocks grid transform_coords column (row + 1) (count - 1)
    end

let tilt_column grid transform_coords height column =
  let rounded_rock_count = ref 0 in
  for row = height - 1 downto 0 do
    match get grid transform_coords (row, column) with
    | 'O' ->
       incr rounded_rock_count;
       set grid transform_coords (row, column) '.'
    | '#' ->
       drop_rounded_rocks grid transform_coords column (row + 1)
         !rounded_rock_count;
       rounded_rock_count := 0
    | '.' -> ()
    | _ -> assert false
  done;
  drop_rounded_rocks grid transform_coords column 0 !rounded_rock_count

let tilt grid transform_coords height width =
  for column = 0 to width - 1 do
    tilt_column grid transform_coords height column
  done

let tilt_north grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  tilt grid Fun.id height width

let tilt_cycle grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  tilt grid Fun.id height width;
  tilt grid (fun (i, j) -> (j, i)) width height;
  tilt grid (fun (i, j) -> (height - i - 1, j)) height width;
  tilt grid (fun (i, j) -> (j, width - i - 1)) width height

let parse_line line =
  Array.of_seq (String.to_seq line)

let amount_of_load grid =
  let total = ref 0 in
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  for row = 0 to height - 1 do
    for column = 0 to width -1 do
      if grid.(row).(column) = 'O' then
        total := !total + (height - row)
    done
  done;
  !total

let copy_grid grid =
  Array.copy (Array.map Array.copy grid)

let measure_iterate_tilting_cycle grid =
  let table = Hashtbl.create 16 in
  let rec loop count =
    match Hashtbl.find_opt table grid with
    | Some index -> (index, count)
    | None ->
       Hashtbl.add table (copy_grid grid) count;
       tilt_cycle grid;
       loop (count + 1) in
  loop 0

let iterate_tilting_cycle grid count =
  let (initial, count') = measure_iterate_tilting_cycle grid in
  assert (count >= count');
  let left = (count - count') mod (count' - initial) in
  for _ = 1 to left do
    tilt_cycle grid;
  done

let () =
  let grid = Array.of_seq (Seq.map parse_line (Common.input_lines ())) in
  let result_part1 =
    let grid_part1 = copy_grid grid in
    tilt_north grid_part1;
    amount_of_load grid_part1 in
  Format.eprintf "Part 1: %d@." result_part1;
  let result_part2 =
    let grid_part2 = copy_grid grid in
    iterate_tilting_cycle grid_part2 1000000000;
    amount_of_load grid_part2 in
  Format.eprintf "Part 2: %d@." result_part2


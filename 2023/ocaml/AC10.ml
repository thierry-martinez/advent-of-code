let follow_pipe symbol (dx, dy) =
  match symbol, dx, dy with
  | '|', 0, _ -> Some (0, dy)
  | '-', _, 0 -> Some (dx, 0)
  | 'L', 0, 1 -> Some (1, 0)
  | 'L', -1, 0 -> Some (0, -1)
  | 'J', 0, 1 -> Some (-1, 0)
  | 'J', 1, 0 -> Some (0, -1)
  | '7', 1, 0 -> Some (0, 1)
  | '7', 0, -1 -> Some (-1, 0)
  | 'F', 0, -1 -> Some (1, 0)
  | 'F', -1, 0 -> Some (0, 1)
  | _ -> None

let push_pipe pipes (x, y) symbol =
  if symbol <> '-' then
    pipes.(y) <- (x, symbol) :: pipes.(y)

let rec follow_pipes grid pipes len (x, y) d =
  let (dx, dy) = d in
  let x = x + dx and y = y + dy in
  match grid.(y).(x) with
  | 'S' -> (d, len + 1)
  | symbol ->
     push_pipe pipes (x, y) symbol;
     follow_pipes grid pipes (succ len) (x, y) (Option.get (follow_pipe symbol d))

let infer_pipe (dx, dy) (dx', dy') =
  match dx, dy, dx', dy' with
  | 0, _, 0, _ -> '|'
  | _, 0, _, 0 -> '-'
  | 0, 1, 1, 0
  | -1, 0, 0, -1 -> 'L'
  | 0, 1, -1, 0
  | 1, 0, 0, -1 -> 'J'
  | 1, 0, 0, 1
  | 0, -1, -1, 0 -> '7'
  | 0, -1, 1, 0
  | -1, 0, 0, 1 -> 'F'
  | _ -> failwith "infer_pipe"

let measure_cycle grid pipes (x, y) =
  let (p, d, d') = Option.get (Seq.find_map (fun dy -> Seq.find_map (fun dx ->
      let x = x + dx and y = y + dy in
      if
        (dx, dy) <> (0, 0)
        && y >= 0 && y < Array.length grid && x >= 0 && x < Array.length grid.(y)
      then
        let symbol = grid.(y).(x) in
        Option.map (fun d ->
            push_pipe pipes (x, y) symbol;
            ((x, y), (dx, dy), d))
          (follow_pipe symbol (dx, dy))
      else
        None) (Seq.init 3 pred)) (Seq.init 3 pred)) in
  let (d'', len) = follow_pipes grid pipes 1 p d' in
  push_pipe pipes (x, y) (infer_pipe d'' d);
  len

type state =
  | Inside of int
  | Outside
  | Up
  | Down

let measure_inside pipes =
  let pipes = List.sort (fun (x, _) (x', _) -> Int.compare x x') pipes in
  let total, _state =
    List.fold_left (fun (total, state) (x, symbol) ->
      let total =
        match state with
        | Inside x0 -> total + x - x0 - 1
        | _ -> total in
      let state =
        match symbol, state with
        | '|', Inside _ -> Outside
        | '|', Outside -> Inside x
        | 'L', Outside -> Down
        | 'L', Inside _ -> Up
        | 'F', Outside -> Up
        | 'F', Inside _ -> Down
        | '7', Down -> Inside x 
        | '7', Up -> Outside
        | 'J', Down -> Outside
        | 'J', Up -> Inside x
        | _ -> failwith "measure_inside" in
      total, state) (0, Outside) pipes in
  total

let () =
  let input = Seq.memoize (Common.input_lines ()) in
  let grid =
    Array.of_seq (Seq.map (fun s -> Array.of_seq (String.to_seq s)) input) in
  let animal =
    Option.get
      (Array.find_mapi (fun y line ->
        Option.map (fun x -> (x, y)) (Array.find_index ((=) 'S') line)) grid) in
  let pipes = Array.make (Array.length grid) [] in
  let length = measure_cycle grid pipes animal in
  Format.eprintf "Part 1: %d\n" (length / 2);
  let inside_area =
    Array.fold_left ( + ) 0 (Array.map measure_inside pipes) in
  Format.eprintf "Part 2: %d\n" inside_area

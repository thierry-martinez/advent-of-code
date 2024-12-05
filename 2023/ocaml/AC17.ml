let compare_pair cmp_a cmp_b (a1, b1) (a2, b2) =
  match cmp_a a1 a2 with
  | 0 -> cmp_b b1 b2
  | cmp -> cmp

module Direction = struct
  type t = Horizontal | Vertical

  let compare a b =
    match a, b with
    | Horizontal, Horizontal -> 0
    | Horizontal, Vertical -> -1
    | Vertical, Horizontal -> 1
    | Vertical, Vertical -> 0

  let turn direction =
    match direction with
    | Horizontal -> Vertical
    | Vertical -> Horizontal
end

module Coords = struct
  type t = {
      x: int;
      y: int;
    }

  let compare a b =
    compare_pair Int.compare Int.compare (a.x, a.y) (b.x, b.y)

  let size matrix =
    { x = Array.length matrix.(0); y = Array.length matrix }

  let ( + ) a b =
    { x = a.x + b.x; y = a.y + b.y }

  let in_bounds { x = width; y = height } { x; y } =
    x >= 0 && y >= 0 && x < width && y < height

  let get matrix { x; y } =
    matrix.(y).(x)

  let of_direction (direction : Direction.t) =
    match direction with
    | Horizontal -> [{ x = -1; y = 0 }; { x = 1; y = 0 }]
    | Vertical -> [{ x = 0; y = -1 }; { x = 0; y = 1 }]
end

let parse_line line =
  Array.of_seq (Seq.map
    (fun c -> Option.get (Common.int_of_digit c)) (String.to_seq line))

module State = struct
  type t = {
      pos: Coords.t;
      direction: Direction.t;
    }

  let compare a b =
    compare_pair Coords.compare Direction.compare
      (a.pos, a.direction) (b.pos, b.direction)
end

module HeatlossState = struct
  type t = {
      heatloss: int;
      state: State.t;
    }

  let compare a b =
    compare_pair Int.compare State.compare
      (a.heatloss, a.state) (b.heatloss, b.state)
end

module StateMap = Map.Make (State)

module HeatlossStateMap = Map.Make (HeatlossState)

let rec find_path_loop min_turn max_forward grid tgt heap_map heatloss_map =
  let heatloss_state, () = HeatlossStateMap.min_binding heap_map in
  let heap_map = HeatlossStateMap.remove heatloss_state heap_map in
  if heatloss_state.state.pos = tgt then
    heatloss_state.heatloss
  else
    begin
      let heatloss = StateMap.find heatloss_state.state heatloss_map in
      if heatloss <> heatloss_state.heatloss then
        find_path_loop min_turn max_forward grid tgt heap_map heatloss_map
      else
        begin
          let direction = Direction.turn heatloss_state.state.direction in
          let offsets = Coords.of_direction direction in
          let heap_map, heatloss_map =
            List.fold_left (fun (heap_map, heatloss_map) offset ->
              let rec make_step step pos heatloss heap_map heatloss_map =
                if step > max_forward then
                  heap_map, heatloss_map
                else
                  begin
                    let pos = Coords.(pos + offset) in
                    if Coords.in_bounds (Coords.size grid) pos then
                      begin
                        let heatloss = heatloss + Coords.get grid pos in
                        let heap_map, heatloss_map =
                          if step >= min_turn then
                            begin
                              let state = State.{ pos; direction } in
                              match StateMap.find state heatloss_map with
                              | exception Not_found ->
                                 HeatlossStateMap.add { heatloss; state } ()
                                   heap_map,
                                 StateMap.add state heatloss heatloss_map
                              | heatloss' ->
                                 if heatloss' <= heatloss then
                                   heap_map, heatloss_map
                                 else
                                   HeatlossStateMap.add { heatloss; state } ()
                                     heap_map,
                                   StateMap.add state heatloss heatloss_map
                            end
                          else
                            heap_map, heatloss_map in
                        make_step (step + 1) pos heatloss heap_map heatloss_map
                      end
                    else
                      heap_map, heatloss_map
                  end in
              make_step 1 heatloss_state.state.pos heatloss heap_map
                heatloss_map)
              (heap_map, heatloss_map) offsets in
          find_path_loop min_turn max_forward grid tgt heap_map heatloss_map
        end
    end

let find_path ~min_turn ~max_forward grid from tgt =
  let heap_map = HeatlossStateMap.of_list [
    { heatloss = 0; state = { pos = from; direction = Horizontal }}, ();
    { heatloss = 0; state = { pos = from; direction = Vertical }}, ();
  ] in
  let heatloss_map = StateMap.of_list [
    { pos = from; direction = Horizontal }, 0;
    { pos = from; direction = Vertical }, 0;
  ] in
  find_path_loop min_turn max_forward grid tgt heap_map heatloss_map

let () =
  let grid = Array.of_seq (Seq.map parse_line (Common.input_lines ())) in
  let result_part1 =
    find_path grid { x = 0; y = 0 } Coords.(size grid + { x = -1; y = -1 })
      ~min_turn:0 ~max_forward:3 in
  Format.eprintf "Part 1: %d@." result_part1;
  let result_part2 =
    find_path grid { x = 0; y = 0 } Coords.(size grid + { x = -1; y = -1 })
      ~min_turn:4 ~max_forward:10 in
  Format.eprintf "Part 2: %d@." result_part2

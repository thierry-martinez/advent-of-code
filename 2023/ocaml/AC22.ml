type point = {
    x : int;
    y : int;
    z : int;
  }

type block = {
    id: int;
    inf: point;
    sup: point;
  }

let parse_line id line =
  Scanf.sscanf line "%d,%d,%d~%d,%d,%d" (fun x0 y0 z0 x1 y1 z1 ->
      { id; inf = { x = x0; y = y0; z = z0 }; sup = { x = x1; y = y1; z = z1 }})

let compare_block_z a b =
  Int.compare a.inf.z b.inf.z

let intersect_block_xy a b =
  a.inf.x <= b.sup.x && b.inf.x <= a.sup.x &&
  a.inf.y <= b.sup.y && b.inf.y <= a.sup.y

let rec filter_max accu p blocks =
  match blocks with
  | [] -> accu
  | (hd, _) :: tl ->
     let accu =
       if p hd then
         match accu with
         | [] -> [hd]
         | max :: _ ->
            if max.sup.z < hd.sup.z then
              [hd]
            else if max.sup.z = hd.sup.z then
              hd :: accu
            else
              accu
       else
         accu in
     filter_max accu p tl

let fall_block fallen block =
  let supports = filter_max [] (intersect_block_xy block) fallen in
  let z =
    match supports with
    | [] -> 1
    | hd :: _ -> hd.sup.z + 1 in
  let height = block.sup.z - block.inf.z in
  (
    { id = block.id; inf = { block.inf with z }; sup = { block.sup with z = z + height }},
    supports
  ) :: fallen

let fall_blocks blocks =
  let blocks = List.sort compare_block_z blocks in
  List.fold_left fall_block [] blocks

module IntMap = Map.Make (Int)

module IntSet = Set.Make (Int)

let rec fall_set support_map fallen_blocks =
  let (added, set) =
    IntMap.fold (fun block_id support (added, set) ->
        if not (IntSet.mem block_id set) && not (IntSet.is_empty support) && IntSet.subset support fallen_blocks then
          (true, IntSet.add block_id set)
        else
          (added, set)) support_map (false, fallen_blocks) in
  if added then
    fall_set support_map set
  else
    set

let count_fall support_map id =
  IntSet.cardinal (fall_set support_map (IntSet.singleton id)) - 1

let () =
  let blocks = List.mapi parse_line (List.of_seq (Common.input_lines ())) in
  let blocks = fall_blocks blocks in
  let singletons = List.sort_uniq compare (List.filter_map (fun (_, supports) ->
    match supports with [block] -> Some block.id | _ -> None
  ) blocks) in
  let result_part1 = List.length blocks - List.length singletons in
  Printf.eprintf "Part 1: %d\n" result_part1;
  let support_map = List.fold_left (fun map (block, supports) ->
      IntMap.add block.id (IntSet.of_list (List.map (fun block -> block.id) supports)) map)
    IntMap.empty blocks in
  let result_part2 =
    List.fold_left ( + ) 0 (List.map (count_fall support_map) singletons) in
  Printf.eprintf "Part 2: %d\n" result_part2
  

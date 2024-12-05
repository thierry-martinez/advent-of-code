let add_char_to_hash current_value char =
  (current_value + int_of_char char) * 17 mod 256

let hash s =
  Seq.fold_left add_char_to_hash 0 (String.to_seq s)

module StringSet = Set.Make (String)

module StringMap = Map.Make (String)

let strip_remove (removed, accu) step =
  if String.ends_with ~suffix:"-" step then
    let label = String.sub step 0 (String.length step - 1) in
    StringSet.add label removed, accu
  else
    let (label, focus) =
      Scanf.sscanf step "%s@=%d" (fun label focus -> (label, focus)) in
    let accu =
      if StringSet.mem label removed then
        accu
      else
        (label, focus) :: accu in
    removed, accu

let execute_set_step boxes focuses (label, focus) =
  if not (StringMap.mem label focuses) then
    begin
      let index = hash label in
      boxes.(index) <- label :: boxes.(index)
    end;
  StringMap.add label focus focuses

let add_len_focusing_power focuses (i, total) label =
  i + 1, total + i * StringMap.find label focuses

let add_box_focusing_power focuses (i, total) lens =
  let _, box_total =
    List.fold_left (add_len_focusing_power focuses) (1, 0) (List.rev lens) in
  i + 1, total + i * box_total

let () =
  let input = Common.input_lines () in
  let s = String.concat "" (List.of_seq input) in
  let steps = String.split_on_char ',' s in
  let hashes = List.map hash steps in
  let result_part1 = List.fold_left ( + ) 0 hashes in
  Format.eprintf "Part 1: %d@." result_part1;
  let _, set_steps =
    List.fold_left strip_remove (StringSet.empty, []) (List.rev steps) in
  let boxes = Array.make 256 [] in
  let focuses =
    List.fold_left (execute_set_step boxes) StringMap.empty set_steps in
  let _, result_part2 =
    Array.fold_left (add_box_focusing_power focuses) (1, 0) boxes in
  Format.eprintf "Part 2: %d@." result_part2

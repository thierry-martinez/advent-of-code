module IntSet = Set.Make (Int)

let add_number_if_winning winning_set count number =
  if IntSet.mem number winning_set then
    succ count
  else
    count

let list_of_string str =
  List.filter_map (fun s -> if s = "" then None else Some (int_of_string s)) (String.split_on_char ' ' str)

let winning_number_count_of_card_aux _id winning numbers =
  let winning_list = list_of_string winning in
  let numbers_list = list_of_string numbers in
  let winning_set = IntSet.of_list winning_list in
  List.fold_left (add_number_if_winning winning_set) 0 numbers_list

let winning_number_count_of_card card_str =
  Scanf.sscanf card_str "Card %d: %s@| %s@\n" winning_number_count_of_card_aux

let points_of_card card_str =
  let count = winning_number_count_of_card card_str in
  if count = 0 then
    0
  else
    1 lsl (count - 1)

let part1 input =
  Common.sum (Seq.map points_of_card input)

let[@tail_mod_cons] rec increment points value list =
  if points <= 0 then
    list
  else
    match list with
    | [] -> 1 + value :: increment (points - 1) value []
    | hd :: tl -> hd + value :: increment (points - 1) value tl

let copies_of_card (sum, next_copies) card_str =
  let copy_count, next_copies =
    match next_copies with
    | [] -> 1, []
    | hd :: tl -> hd, tl in
  let points = winning_number_count_of_card card_str in
  let sum = sum + copy_count in
  let next_copies = increment points copy_count next_copies in
  (sum, next_copies)

let part2 input =
  let (sum, next_copies) = Seq.fold_left copies_of_card (0, []) input in
  assert (next_copies = []);
  sum

let () =
  let input = Seq.memoize (Common.input_lines ()) in
  Format.eprintf "Part 1: %d@." (part1 input);
  Format.eprintf "Part 2: %d@." (part2 input)

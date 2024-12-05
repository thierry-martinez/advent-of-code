let find_numbers str =
  let rec find_next_digit accu i =
    if i < String.length str then
      match Common.int_of_digit str.[i] with
      | Some n -> add_number accu n i (i + 1)
      | None -> find_next_digit accu (i + 1)
    else
      accu
  and add_number accu number start i =
    match
      if i < String.length str then
        Common.int_of_digit str.[i]
      else
        None
    with
    | Some n -> add_number accu (number * 10 + n) start (i + 1)
    | None -> find_next_digit ((number, start, i) :: accu) (i + 1) in
  find_next_digit [] 0

let is_symbol c =
  c <> '.'

let rec string_has_symbol_between str start stop =
  start <= stop && (is_symbol str.[start] || string_has_symbol_between str (start + 1) stop)

let string_opt_has_symbol_between str_opt start stop =
  match str_opt with
  | None -> false
  | Some str ->
     string_has_symbol_between str (max start 0) (min stop (String.length str - 1))

let number_has_symbol line previous_line next_line (number, start, stop) =
  if
    (start > 0 && is_symbol line.[start - 1]) ||
    (stop < String.length line && is_symbol line.[stop]) ||
    string_opt_has_symbol_between previous_line (start - 1) stop ||
    string_opt_has_symbol_between next_line (start - 1) stop
  then
    Some number
  else
    None

let sum_line line previous_line next_line =
  let numbers = find_numbers line in
  List.fold_left ( + ) 0
    (List.filter_map (number_has_symbol line previous_line next_line) numbers)

let sum_adjacent f seq =
  let (sum, previous_line, line) =
    Seq.fold_left (fun (sum, previous_line, line) next_line ->
      let sum =
        match line with
        | None -> sum
        | Some line -> sum + f line previous_line (Some next_line) in
      (sum, line, Some next_line)) (0, None, None) seq in
  match line with
  | None -> sum
  | Some line -> sum + f line previous_line None

let _part1 () =
  let sum = sum_adjacent sum_line (Common.input_lines ()) in
  Format.eprintf "%d@." sum

let is_adjacent i (number, start, stop) =
  if start - 1 <= i && i <= stop then
    Some number
  else
    None

let sum_gears line previous_line next_line =
  let previous_numbers = Option.map find_numbers previous_line in
  let numbers = find_numbers line in
  let next_numbers = Option.map find_numbers next_line in
  let filter_gear (i, c) =
    if c = '*' then
      let adjacent_numbers =
        List.filter_map (is_adjacent i) numbers @
        List.filter_map (is_adjacent i) (Option.value ~default:[] previous_numbers) @
        List.filter_map (is_adjacent i) (Option.value ~default:[] next_numbers) in
      match adjacent_numbers with
      | [a; b] -> Some (a * b)
      | _ -> None
    else
      None in
  Common.sum (Seq.filter_map filter_gear (String.to_seqi line))

let part2 () =
  let sum = sum_adjacent sum_gears (Common.input_lines ()) in
  Format.eprintf "%d@." sum

let () = part2 ()

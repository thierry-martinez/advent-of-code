let get_winning_ways time distance =
  (* #hold s.t. (time - hold) * hold > distance
     i.e. - hold ** 2 + time * hold - distance > 0
   *)
  let time_f = float_of_int time in
  let distance_f = float_of_int distance in
  let delta = time_f ** 2. -. 4. *. distance_f in
  let sqrt_delta = sqrt delta in
  let alpha = (time_f -. sqrt_delta) /. 2. in
  let ceil_alpha = ceil alpha in
  let min_holding_time_f =
    if ceil_alpha = alpha then
      alpha +. 1.
    else
      ceil_alpha in
  let min_holding_time = int_of_float min_holding_time_f in
  min_holding_time, time - min_holding_time

(*
  We could also compute

  let beta = (time_f +. sqrt_delta) /. 2. in
  let max_holding_time_f =
    if floor beta = beta then
      beta -. 1.
    else
      floor beta in
  int_of_float min_holding_time_f, int_of_float max_holding_time_f

  but it worth noticing that max_holding_time = time - min_holding_time
*)

let get_number_of_winning_ways (time, distance) =
  let (min, max) = get_winning_ways time distance in
  max - min + 1

(* It appears that inputs are small enough for the following solution
   to be suitable even for Part 2.

  let result = ref 0 in
  for i = 0 to time do
    if i * (time - i) > distance then
      incr result
  done;
  !result
*)

let solve input get_numbers =
  let times_str, input = Option.get (Seq.uncons input) in
  let distances_str, _input = Option.get (Seq.uncons input) in
  let times = get_numbers times_str in
  let distances = get_numbers distances_str in
  let records = List.combine times distances in
  let number_of_winning_ways = List.map get_number_of_winning_ways records in
  List.fold_left ( * ) 1 number_of_winning_ways

let get_numbers1 str =
  List.map int_of_string (List.filter (fun s -> s <> "") (List.tl (String.split_on_char ' ' str)))

let part1 input =
  solve input get_numbers1

let get_numbers2 str =
  let colon_index = String.index str ':' in
  let value_str = String.sub str (colon_index + 1) (String.length str - colon_index - 1) in
  let b = Buffer.create 17 in
  String.iter (fun c -> if c <> ' ' then Buffer.add_char b c) value_str;
  [int_of_string (Buffer.contents b)]

let part2 input =
  solve input get_numbers2

let () =
  let input = Seq.memoize (Common.input_lines ()) in
  Format.eprintf "Part 1: %d@." (part1 input);
  Format.eprintf "Part 2: %d@." (part2 input)

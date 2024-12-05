let rec derive numbers =
  match numbers with
  | [] | [_] -> []
  | a :: (b :: _ as tl) ->
     b - a :: derive tl

let rec last list =
  match list with
  | [] -> invalid_arg "last"
  | [a] -> a
  | _ :: tl -> last tl

let rec extrapolate side op numbers =
  if List.for_all (( = ) 0) numbers then
    0
  else
    let previous = extrapolate side op (derive numbers) in
    op (side numbers) previous

let solve side op numbers =
  Seq.fold_left ( + ) 0 (Seq.map (extrapolate side op) numbers)

let parse line =
  List.map int_of_string (String.split_on_char ' ' line)

let () =
  let input = Seq.memoize (Seq.map parse (Common.input_lines ())) in
  Format.eprintf "Part 1: %d@." (solve last ( + ) input);
  Format.eprintf "Part 2: %d@." (solve List.hd ( - ) input);

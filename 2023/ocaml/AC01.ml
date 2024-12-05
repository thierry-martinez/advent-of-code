module type Part = sig
  val first_digit : string -> int

  val last_digit : string -> int
end

module PartOne : Part = struct
  let int_of_digit c =
    match c with
      | '0' .. '9' -> Some (int_of_char c - int_of_char '0')
      | _ -> None

  let rec first_digit_from str index =
    if index < String.length str then
      match int_of_digit str.[index] with
      | Some value -> value
      | None -> first_digit_from str (succ index)
    else
      raise Not_found

  let first_digit str =
    first_digit_from str 0

  let rec last_digit_from str index =
    if index >= 0 then
      match int_of_digit str.[index] with
      | Some value -> value
      | None -> last_digit_from str (pred index)
    else
      raise Not_found

  let last_digit str =
    last_digit_from str (String.length str - 1)
end

module PartTwo : Part = struct
  let values =
    List.init 10 (fun i -> (string_of_int i, i)) @ [
        ("zero", 0);
        ("one", 1);
        ("two", 2);
        ("three", 3);
        ("four", 4);
        ("five", 5);
        ("six", 6);
        ("seven", 7);
        ("eight", 8);
        ("nine", 9);
      ]

  let int_of_digit_at str index =
    List.find_map (fun (s, v) ->
      if String.length str >= String.length s + index &&
         String.sub str index (String.length s) = s then
        Some v
      else
        None) values

  let rec first_digit_from str index =
    if index < String.length str then
      match int_of_digit_at str index with
      | Some value -> value
      | None -> first_digit_from str (succ index)
    else
      raise Not_found

  let first_digit str =
    first_digit_from str 0

  let rec last_digit_from str index =
    if index >= 0 then
      match int_of_digit_at str index with
      | Some value -> value
      | None -> last_digit_from str (pred index)
    else
      raise Not_found

  let last_digit str =
    last_digit_from str (String.length str - 1)
end

let input_lines () =
  Seq.of_dispenser (fun () ->
    try Some (input_line stdin)
    with End_of_file -> None)

module MakeCalibrate (Part : Part) = struct
  let calibration_value str =
    let first = Part.first_digit str in
    let last = Part.last_digit str in
    Format.eprintf "%d %d@." first last;
    first * 10 + last
  
  let calibration_sum lines =
    Seq.fold_left (fun sum line -> sum + calibration_value line) 0 lines
end

let () =
  let module Calibrate = MakeCalibrate (PartTwo) in
  Format.eprintf "%d@." (Calibrate.calibration_sum (input_lines ()))

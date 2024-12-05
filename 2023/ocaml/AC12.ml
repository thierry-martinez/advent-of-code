let rec align memo i previous_bad damaged sequences =
  try
    Hashtbl.find memo (i, previous_bad, sequences)
  with Not_found ->
    let result =
      if i >= String.length damaged then
        match previous_bad, sequences with
        | 0, [] -> 1
        | _, [bad] when previous_bad = bad -> 1
        | _ -> 0
      else
        let handle_ok () =
          if previous_bad = 0 then
            align memo (i + 1) 0 damaged sequences
          else
            match sequences with
            | hd :: tl when hd = previous_bad ->
               align memo (i + 1) 0 damaged tl
            | _ -> 0 in
        let handle_bad () =
          match sequences with
          | hd :: _tl when hd > previous_bad ->
             align memo (i + 1) (previous_bad + 1) damaged sequences
          | _ -> 0 in
        match damaged.[i] with
        | '.' -> handle_ok ()
        | '#' -> handle_bad ()
        | '?' -> handle_ok () + handle_bad ()
        | _ -> failwith "align" in
    Hashtbl.add memo (i, previous_bad, sequences) result;
    result

let count (damaged, sequences) =
  align (Hashtbl.create 16) 0 0 damaged sequences

let parse_line line =
  Scanf.sscanf line "%s %s" (fun damaged sequences ->
    let sequences = String.split_on_char ',' sequences in
    damaged, List.map int_of_string sequences)

let unfold n (damaged, sequences) =
  (String.concat "?" (List.init n (fun _ -> damaged)),
   List.flatten (List.init n (fun _ -> sequences)))

let () =
  let lines = Seq.memoize (Seq.map parse_line (Common.input_lines())) in
  let result_part1 = Seq.fold_left ( + ) 0 (Seq.map count lines) in
  Format.eprintf "Part 1: %d@." result_part1;
  let result_part2 =
    Seq.fold_left ( + ) 0 (Seq.map (fun line -> count (unfold 5 line)) lines) in
  Format.eprintf "Part 2: %d@." result_part2


[@@@ocaml.warning "-32"]

let memo_partition = Hashtbl.create 16

let rec partition total n =
  try
    Hashtbl.find memo_partition (total, n)
  with Not_found ->
    let result =
  if n = 0 then
    failwith "partition"
  else if n = 1 then
    1
  else if n = 2 then
    total + 1
  else
    List.fold_left (fun count i -> count + partition i (n - 1)) 0 (List.init (total + 1) Fun.id) in
    Hashtbl.add memo_partition (total, n) result;
    result

let rec skip_bad i damaged =
  if i >= String.length damaged || damaged.[i] != '?' then
    i
  else
    skip_bad (i + 1) damaged

let memo = Hashtbl.create 16

let rec align i previous_bad damaged sequences =
  try
    Hashtbl.find memo (i, previous_bad, damaged, sequences)
  with Not_found ->
    let result =
  (*Format.eprintf "@[align %d %d sequences:%a@]@." i previous_bad (Format.pp_print_list Format.pp_print_int ~pp_sep:Format.pp_print_space) sequences;*)
  if i >= String.length damaged then
    match previous_bad, sequences with
    | 0, [] -> 1
    | _, [bad] when previous_bad = bad -> 1
    | _ -> 0
  else
    let handle_ok () =
      if previous_bad = 0 then
        align (i + 1) 0 damaged sequences
      else
        match sequences with
        | hd :: tl when hd = previous_bad ->
           align (i + 1) 0 damaged tl
        | _ -> 0 in
    let handle_bad () =
      match sequences with
      | hd :: _tl when hd > previous_bad ->
         align (i + 1) (previous_bad + 1) damaged sequences
      | _ -> 0 in
    match damaged.[i] with
    | '.' -> handle_ok ()
    | '#' -> handle_bad ()
    | '?' -> handle_ok () + handle_bad ()
(*
       if previous_bad > 0 then
         if previous_bad < List.hd sequences then
           handle_bad ()
         else
           handle_ok ()
       else
         let j = skip_bad (i + 1) damaged in
         let bad_count = j - i in
         place_bad j bad_count damaged sequences
*)
    | _ -> failwith "align" in
    Hashtbl.add memo (i, previous_bad, damaged, sequences) result;
    result
and place_bad i bad_count damaged sequences =
  (*Format.eprintf "@[place_bad i:%d bad_count:%d sequences:%a@]@." i bad_count (Format.pp_print_list Format.pp_print_int ~pp_sep:Format.pp_print_space) sequences;*)
  let try_cut seq_taken seq_left =
    match seq_taken with
    | [] -> align i 0 damaged seq_left
    | last :: others ->
       let rec end_with_bad end_bad =
         (*Format.eprintf "end_with_bad %d seq_left:%a@." end_bad  (Format.pp_print_list Format.pp_print_int ~pp_sep:Format.pp_print_space) seq_left;*)
         if end_bad > last then
           0
         else
         let this_try =
           if end_bad = 0 then
             let fixed =
               List.fold_left (fun total seq -> total + seq + 1) 0 seq_taken in
             let left_to_place = bad_count - fixed in
             if left_to_place < 0 then
               0
             else
               partition left_to_place (List.length seq_taken + 1) *
               align i 0 damaged seq_left
           else if end_bad <= last then
             let fixed =
               List.fold_left (fun total seq -> total + seq + 1) end_bad others in
             let left_to_place = bad_count - fixed in
             if left_to_place < 0 then
               0
             else
               partition left_to_place (List.length others + 1) *
               align i end_bad damaged (last :: seq_left)
           else
             0 in
         (*Format.eprintf "result end_with_bad %d seq_left:%a %d@." end_bad  (Format.pp_print_list Format.pp_print_int ~pp_sep:Format.pp_print_space) seq_left this_try;*)
         this_try + end_with_bad (end_bad + 1) in
       end_with_bad 0 in
  let rec find_cut total seq_taken seq_left =
    let take_that = try_cut seq_taken seq_left in
    let total = total + take_that in
    match seq_left with
    | [] -> total
    | hd :: tl -> find_cut total (hd :: seq_taken) tl in
  find_cut 0 [] sequences

let count (damaged, sequences) =
  align 0 0 damaged sequences

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


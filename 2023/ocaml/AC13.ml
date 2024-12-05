exception Too_many_defects

let reflection_count ~smudge_count size0 size1 get =
  let counter = ref 0 in
  for line = 1 to size0 - 1 do
    let allowed_defects_ref = ref smudge_count in
    try
      for i = 0 to min (line - 1) (size0 - line - 1) do
        for j = 0 to size1 - 1 do
          if get (line - i - 1) j <> get (line + i) j then
            begin
              let allowed_defects = !allowed_defects_ref in
              if allowed_defects > 0 then
                allowed_defects_ref := allowed_defects - 1
              else
                raise Too_many_defects
            end
          done
      done;
      if !allowed_defects_ref = 0 then
        counter := !counter + line
    with Too_many_defects -> ()
  done;
  !counter

let evaluate_reflection ~smudge_count pattern =
  let height = Array.length pattern in
  let width = String.length pattern.(0) in
  let vertical_line_count =
    reflection_count ~smudge_count width height (fun i j -> pattern.(j).[i]) in
  let horizontal_line_count =
    reflection_count ~smudge_count height width (fun i j -> pattern.(i).[j]) in
  vertical_line_count + horizontal_line_count * 100

let () =
  let patterns =
    List.map Array.of_list
      (Common.split (fun s -> s = "") (List.of_seq (Common.input_lines()))) in
  let result_part1 =
    List.fold_left ( + ) 0 (List.map (evaluate_reflection ~smudge_count:0) patterns) in
  Format.eprintf "Part 1: %d@." result_part1;
  let result_part2 =
    List.fold_left ( + ) 0 (List.map (evaluate_reflection ~smudge_count:1) patterns) in
  Format.eprintf "Part 2: %d@." result_part2

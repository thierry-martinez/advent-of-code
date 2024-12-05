module StringMap = Map.Make (String)

type range = { len: int; src: int; tgt: int }

let rec parse_map_ranges ranges input =
  match Seq.uncons input with
  | None -> ranges
  | Some (line, input) ->
     if line = "" then
       ranges
     else
       match String.split_on_char ' ' line with
       | [tgt; src; len] ->
          parse_map_ranges ({ len = int_of_string len; src = int_of_string src; tgt = int_of_string tgt } :: ranges) input
       | _ -> assert false

let rec parse_maps maps input =
  match Seq.uncons input with
  | None -> maps
  | Some (line, input) ->
     if line = "" then
       parse_maps maps input
     else
       let ranges = parse_map_ranges [] input in
       let maps = StringMap.add line ranges maps in
       parse_maps maps input

let convert_value_range value { len; src; tgt } =
  if value >= src && value <= src + len then
    Some (value - src + tgt)
  else
    None

let convert_value ranges value =
  Option.value ~default:value
    (List.find_map (convert_value_range value) ranges)

let part1 input =
  let seeds_str, input = Option.get (Seq.uncons input) in
  let seeds = List.map int_of_string (List.tl (String.split_on_char ' ' seeds_str)) in
  let maps = parse_maps StringMap.empty input in
  Format.eprintf "%a@." (Format.pp_print_list Format.pp_print_int ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())) seeds;
  let soils = List.map (convert_value (StringMap.find "seed-to-soil map:" maps)) seeds in
  Format.eprintf "%a@." (Format.pp_print_list Format.pp_print_int ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())) soils;
  let ferts = List.map (convert_value (StringMap.find "soil-to-fertilizer map:" maps)) soils in
  Format.eprintf "%a@." (Format.pp_print_list Format.pp_print_int ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())) ferts;
  let wates = List.map (convert_value (StringMap.find "fertilizer-to-water map:" maps)) ferts in
  Format.eprintf "%a@." (Format.pp_print_list Format.pp_print_int ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())) wates;
  let lights = List.map (convert_value (StringMap.find "water-to-light map:" maps)) wates in
  Format.eprintf "%a@." (Format.pp_print_list Format.pp_print_int ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())) lights;
  let temps = List.map (convert_value (StringMap.find "light-to-temperature map:" maps)) lights in
  Format.eprintf "%a@." (Format.pp_print_list Format.pp_print_int ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())) temps;
  let hums = List.map (convert_value (StringMap.find "temperature-to-humidity map:" maps)) temps in
  Format.eprintf "%a@." (Format.pp_print_list Format.pp_print_int ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())) hums;
  let locs = List.map (convert_value (StringMap.find "humidity-to-location map:" maps)) hums in
  Format.eprintf "%a@." (Format.pp_print_list Format.pp_print_int ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())) locs;
  List.fold_left min (List.hd locs) (List.tl locs)

type seed_range = { start: int; count: int }

let rec parse_seeds ranges seeds =
  match seeds with
  | [] -> ranges
  | start :: count :: seeds ->
     parse_seeds ({ start; count } :: ranges) seeds
  | _ -> assert false

let convert_seed_range { len; src; tgt } { start; count } =
  if start >= src && start + count <= src + len then
    [{ start = start - src + tgt; count }], []
  else if start >= src && start <= src + len then
    [{ start = start - src + tgt; count = src + len - start }],
    [{ start = src + len; count = start + count - src - len }]
  else if start < src && start + count > src + len then
    [{ start = tgt; count = len }],
    [{ start; count = src - start }; { start = src + len; count = start + count - src - len }]
  else if start < src && start + count >= src && start + count <= src + len then
    [{ start = tgt; count = start + count - src }],
    [{ start; count = src - start }]
  else
    [], [{ start; count }]

let convert_seed_ranges (seed_ranges, converted) range =
  let conv, ranges = List.split (List.map (convert_seed_range range) seed_ranges) in
  (List.flatten ranges, List.flatten conv @ converted)

let convert_values ranges seed_range =
  let seed_ranges, converted =
    List.fold_left convert_seed_ranges ([seed_range], []) ranges in
  seed_ranges @ converted

let print_seed_range fmt { start; count } =
  Format.fprintf fmt "%d:%d" start count

let part2 input =
  let seeds_str, input = Option.get (Seq.uncons input) in
  let seeds = List.map int_of_string (List.tl (String.split_on_char ' ' seeds_str)) in
  let maps = parse_maps StringMap.empty input in
  let seeds = parse_seeds [] seeds in
  Format.eprintf "%a@." (Format.pp_print_list print_seed_range ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())) seeds;
  Format.eprintf "@.";
  let soils = List.concat_map (convert_values (StringMap.find "seed-to-soil map:" maps)) seeds in
  Format.eprintf "%a@." (Format.pp_print_list print_seed_range ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())) soils;
  let ferts = List.concat_map (convert_values (StringMap.find "soil-to-fertilizer map:" maps)) soils in
  let wates = List.concat_map (convert_values (StringMap.find "fertilizer-to-water map:" maps)) ferts in
  let lights = List.concat_map (convert_values (StringMap.find "water-to-light map:" maps)) wates in
  let temps = List.concat_map (convert_values (StringMap.find "light-to-temperature map:" maps)) lights in
  let hums = List.concat_map (convert_values (StringMap.find "temperature-to-humidity map:" maps)) temps in
  let locs = List.concat_map (convert_values (StringMap.find "humidity-to-location map:" maps)) hums in
  let locs = List.map (fun { start; _ } -> start) locs in
  List.fold_left min (List.hd locs) (List.tl locs)

let () =
  let input = Seq.memoize (Common.input_lines ()) in
  Format.eprintf "Part 1: %d@." (part1 input);
  Format.eprintf "Part 2: %d@." (part2 input)

type category = X | M | A | S

let category_of_char c =
  match c with
  | 'x' -> X
  | 'm' -> M
  | 'a' -> A
  | 's' -> S
  | _ -> failwith "parse_category"

type comparison = LT | GT

let comparison_of_char c =
  match c with
  | '<' -> LT
  | '>' -> GT
  | _ -> failwith "comparison_of_char"

type answer =
  | Accept
  | Reject

type action =
  | Send of string
  | Answer of answer

let action_of_string s =
  match s with
  | "A" -> Answer Accept
  | "R" -> Answer Reject
  | _ -> Send s

type rule = {
    category : category;
    comparison : comparison;
    value : int;
    action : action;
  }

let rule_of_string s =
  Scanf.sscanf s "%c%c%d:%s" (fun category comparison value action ->
      { category = category_of_char category;
        comparison = comparison_of_char comparison;
        value;
        action = action_of_string action;
      })

type workflow = {
    rules : rule list;
    default : action;
  }

let workflow_of_string s =
  Scanf.sscanf s "%s@{%s@}" (fun name rule_str ->
      let rules = String.split_on_char ',' rule_str in
      let default, rev_rules =
        match List.rev rules with
        | [] -> assert false
        | default :: rev_rules -> default, rev_rules in
      let rules = List.rev_map rule_of_string rev_rules in
      name, { rules; default = action_of_string default }
    )

type 'a part = {
    x : 'a;
    m : 'a;
    a : 'a;
    s : 'a;
  }

let part_of_string s =
  Scanf.sscanf s "{x=%d,m=%d,a=%d,s=%d}"
    (fun x m a s -> { x; m; a; s })

let sum_part { x; m; a; s } =
  x + m + a + s

module StringMap = Map.Make (String)

let get_value { x; m; a; s } category =
  match category with
  | X -> x
  | M -> m
  | A -> a
  | S -> s

let set_value part category value =
  match category with
  | X -> { part with x = value }
  | M -> { part with m = value }
  | A -> { part with a = value }
  | S -> { part with s = value }

let get_comparison comparison =
  match comparison with
  | LT -> ( < )
  | GT -> ( > )

let check_rule part rule =
  get_comparison rule.comparison (get_value part rule.category) rule.value

let find_action workflow part =
  match List.find_opt (check_rule part) workflow.rules with
  | None -> workflow.default
  | Some rule -> rule.action

let rec find_answer workflows workflow part =
  let workflow = StringMap.find workflow workflows in
  match find_action workflow part with
  | Send target -> find_answer workflows target part
  | Answer answer -> answer

let is_accepted workflows part =
  find_answer workflows "in" part = Accept

let split_range (low, high) comparison value =
  match comparison with
  | LT ->
     if high < value then
       Some (low, high), None
     else if low < value then
       Some (low, value - 1), Some (value, high)
     else
       None, Some (low, high)
  | GT ->
     if low > value then
       Some (low, high), None
     else if high > value then
       Some (value + 1, high), Some (low, value)
     else
       None, Some (low, high)

let count_range (low, high) =
  high - low + 1

let count_parts { x; m; a; s } =
  count_range x * count_range m * count_range a * count_range s

let rec count_accepted_ratings workflows workflow parts =
  let count_action action parts =
    match action with
    | Send workflow_name ->
       let workflow = StringMap.find workflow_name workflows in
       count_accepted_ratings workflows workflow parts
    | Answer Accept -> count_parts parts
    | Answer Reject -> 0 in
  let rec split_rule rules parts =
    match rules with
    | [] -> count_action workflow.default parts
    | hd :: tl ->
       let yes_parts, no_parts =
         split_range (get_value parts hd.category) hd.comparison hd.value in
       let yes_count =
         match yes_parts with
         | None -> 0
         | Some yes_parts ->
            count_action hd.action (set_value parts hd.category yes_parts) in
       let no_count =
         match no_parts with
         | None -> 0
         | Some no_parts ->
            split_rule tl (set_value parts hd.category no_parts) in
       yes_count + no_count in
  split_rule workflow.rules parts

let () =
  let lines = List.of_seq (Common.input_lines ()) in
  let workflows, parts = Common.break (( = ) "") lines in
  let workflows =
    StringMap.of_list (List.map workflow_of_string workflows) in
  let parts = List.map part_of_string (List.tl parts) in
  let accepted = List.filter (is_accepted workflows) parts in
  let result_part1 = List.fold_left ( + ) 0 (List.map sum_part accepted) in
  Format.eprintf "Part 1: %d@." result_part1;
  let result_part2 =
    count_accepted_ratings workflows (StringMap.find "in" workflows)
      { x = (1, 4000); m = (1, 4000); a = (1, 4000); s = (1, 4000) } in
  Format.eprintf "Part 2: %d@." result_part2

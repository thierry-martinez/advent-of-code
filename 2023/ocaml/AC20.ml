type kind =
  FlipFlop | Conjunction | Broadcaster

type module_description = {
    kind : kind;
    destinations : Common.Atom.t list;
  }

let split_kind_and_name kind_and_name =
  if kind_and_name = "broadcaster" then
    Broadcaster, kind_and_name
  else
    begin
      let name =
        String.sub kind_and_name 1 (String.length kind_and_name - 1) in
      let kind =
        match kind_and_name.[0] with
        | '%' -> FlipFlop
        | '&' -> Conjunction
        | _ -> failwith "split_kind_and_name" in
      kind, name
    end

let get_destination s =
  Common.Atom.get (String.trim s)

let module_description_of_string s =
  let kind_and_name, destinations =
    Scanf.sscanf s "%s -> %s@\n" (fun kind_and_name destinations ->
        kind_and_name,
        List.map get_destination (String.split_on_char ',' destinations)) in
  let kind, name = split_kind_and_name kind_and_name in
  Common.Atom.get name, { kind; destinations }

type pulse = Low | High

module State = struct
  type t = {
      flip_flop : Common.AtomSet.t;
      conjunction : Common.AtomSet.t Common.AtomMap.t;
    }

  let initial input_map = {
      flip_flop = Common.AtomSet.empty;
      conjunction = input_map;
    }
end

type message = {
    source : Common.Atom.t;
    target : Common.Atom.t;
    pulse : pulse;
  }

let flip_flop send { target; pulse; _ } (state : State.t) =
  match pulse with
  | High -> state
  | Low ->
     if Common.AtomSet.mem target state.flip_flop then
       begin
         send Low;
         { state with flip_flop = Common.AtomSet.remove target state.flip_flop }
       end
     else
       begin
         send High;
         { state with flip_flop = Common.AtomSet.add target state.flip_flop }
       end

let conjunction send { source; target; pulse } (state : State.t) =
  let low_set = Common.AtomMap.find target state.conjunction in
  let low_set', changed =
    match pulse, Common.AtomSet.mem source low_set with
    | Low, false -> Common.AtomSet.remove source low_set, true
    | High, true -> Common.AtomSet.add source low_set, true
    | Low, true
    | High, false -> low_set, false in
  let state =
    if changed then
      { state with conjunction = Common.AtomMap.add target low_set' state.conjunction }
    else
      state in
  if Common.AtomSet.is_empty low_set' then
    send Low
  else
    send High;
  state

let button = Common.Atom.get "button"

let broadcaster = Common.Atom.get "broadcaster"

let push_button observe configuration state =
  let pulse_queue = Queue.create () in
  let push ~source ~target pulse =
    observe ~source ~target pulse;
    Queue.push { source; target; pulse } pulse_queue in
  let rec loop state =
    match Queue.pop pulse_queue with
    | exception Queue.Empty -> state
    | message ->
       let state =
         match Common.AtomMap.find message.target configuration with
         | exception Not_found -> state
         | description ->
           let send pulse =
             let source = message.target in
             let send_to target =
               push ~source ~target pulse in
             List.iter send_to description.destinations in
           match description.kind with
           | FlipFlop ->
              flip_flop send message state
           | Conjunction ->
              conjunction send message state
           | Broadcaster ->
              send message.pulse;
              state in
       loop state in
  push ~source:button ~target:broadcaster Low;
  loop state

let add_input source input_map target =
  let inputs =
    try
      Common.AtomMap.find target input_map
    with Not_found ->
      Common.AtomSet.empty in
  Common.AtomMap.add target (Common.AtomSet.add source inputs) input_map

let add_destinations source description input_map =
  List.fold_left (add_input source) input_map description.destinations

let get_input_map configuration =
  Common.AtomMap.fold add_destinations configuration Common.AtomMap.empty

let rec iterate stop_predicate observe configuration input_map state =
  if stop_predicate () then
    state
  else
    begin
      let state' = push_button observe configuration state in
      iterate stop_predicate observe configuration input_map state'
    end

let part1 configuration input_map =
  let counter_low = ref 0 in
  let counter_high = ref 0 in
  let observe ~source:_ ~target:_ pulse =
    match pulse with
    | Low -> incr counter_low
    | High -> incr counter_high in
  let counter = ref 0 in
  let stop_predicate () =
    if !counter < 1000 then
      begin
        incr counter;
        false
      end
    else
      true in
  let _ =
    iterate stop_predicate observe configuration input_map
      (State.initial input_map) in
  !counter_low * !counter_high

let find_first_high_pulses configuration input_map sources =
  let indices = ref Common.AtomMap.empty in
  let counter = ref 0 in
  let stop_predicate () =
    incr counter;
    Common.AtomSet.for_all (fun source -> Common.AtomMap.mem source !indices) sources in
  let observe ~source ~target:_ pulse =
    if
      pulse = High && Common.AtomSet.mem source sources &&
        not (Common.AtomMap.mem source !indices)
    then
      indices := Common.AtomMap.add source !counter !indices in
  let _ =
    iterate stop_predicate observe configuration input_map
      (State.initial input_map) in
  !indices

let part2 configuration input_map =
  let rx = Common.Atom.get "rx" in
  let rx_sources = Common.AtomMap.find rx input_map in
  assert (Common.AtomSet.cardinal rx_sources = 1);
  let kh = Common.AtomSet.choose rx_sources in
  let kh_configuration = Common.AtomMap.find kh configuration in
  assert (kh_configuration.kind = Conjunction);
  let kh_sources = Common.AtomMap.find kh input_map in
  let indices = find_first_high_pulses configuration input_map kh_sources in
  List.fold_left Common.lcm 1 (List.map snd (Common.AtomMap.bindings indices))

let () =
  let configuration =
    Common.AtomMap.of_seq
      (Seq.map module_description_of_string (Common.input_lines ())) in
  let input_map = get_input_map configuration in
  Printf.eprintf "Part 1 : %d\n" (part1 configuration input_map);
  Printf.eprintf "Part 2 : %d\n" (part2 configuration input_map)

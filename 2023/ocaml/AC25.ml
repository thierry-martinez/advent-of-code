let parse_line line =
  Scanf.sscanf line "%s@: %s@\n" (fun lhs rhs ->
      let rhs_list = String.split_on_char ' ' rhs in
      Common.Atom.get lhs, List.map Common.Atom.get rhs_list)

let add_directed_edge a b graph =
  Common.AtomMap.update a (fun neighbors ->
    Some (Common.AtomSet.add b
      (Option.value ~default:Common.AtomSet.empty neighbors)))
    graph

let add_edge graph (a, b) =
  add_directed_edge a b (add_directed_edge b a graph)

let remove_directed_edge a b graph =
  Common.AtomMap.update a (Option.map (Common.AtomSet.remove b)) graph

let remove_edge (a, b) graph =
  remove_directed_edge a b (remove_directed_edge b a graph)

module EdgeSet = Set.Make (Common.OrderedPair (Common.Atom) (Common.Atom))

let rec bfs_add_neighbors
          flow succ k return u (neighbors : Common.Atom.t Seq.t) pred q =
  match neighbors () with
  | Nil -> k pred q
  | Cons (v, neighbors) ->
     if Common.AtomMap.mem v pred || flow u v then
       bfs_add_neighbors flow succ k return u neighbors pred q
     else
       begin
         let pred = Common.AtomMap.add v u pred in
         if Common.AtomMap.mem v succ then
           return v pred
         else
           bfs_add_neighbors flow succ k return u neighbors pred (v :: q)
       end

let rec bfs_dequeue graph flow succ k return q_s pred q =
  match q_s with
  | [] ->
     if q = [] then
       None
     else
       k pred q
  | u :: q_s ->
     let neighbors = Common.AtomMap.find u graph in
     bfs_add_neighbors flow succ (bfs_dequeue graph flow succ k return q_s)
       return u (Common.AtomSet.to_seq neighbors) pred q

let rec bidirectional_bfs_loop graph flow pred q_s succ q_t =
  if List.length q_s < List.length q_t then
    bfs_dequeue graph (fun u v -> EdgeSet.mem (u, v) flow) succ
      (fun pred q -> bidirectional_bfs_loop graph flow pred q succ q_t)
      (fun v pred -> Some (v, pred, succ))
      q_s pred []
  else
    bfs_dequeue graph (fun u v -> EdgeSet.mem (v, u) flow) pred
      (fun succ q -> bidirectional_bfs_loop graph flow pred q_s succ q)
      (fun v succ -> Some (v, pred, succ))
      q_t succ []

let bidirectional_bfs graph s t flow =
  bidirectional_bfs_loop graph flow (Common.AtomMap.singleton s s) [s]
    (Common.AtomMap.singleton t t) [t]

let[@tail_mod_cons] rec trace_path succ src tgt =
  if src = tgt then
    [tgt]
  else
    src :: trace_path succ (Common.AtomMap.find src succ) tgt

let rec augment flow path =
  match path with
  | [] | [_] -> flow
  | u :: v :: path ->
     augment (EdgeSet.add (u, v) flow) (v :: path)

let rec edmonds_karp_loop graph s t flow value =
  match bidirectional_bfs graph s t flow with
  | None -> value, flow
  | Some (v, pred, succ) ->
     let backward = trace_path pred v s in
     let forward = trace_path succ v t in
     let path = List.rev_append backward forward in
     let flow = augment flow path in
     edmonds_karp_loop graph s t flow (value + 1)
     
let edmonds_karp graph s t =
  edmonds_karp_loop graph s t EdgeSet.empty 0  

let reduce op list =
  match list with
  | [] -> None
  | hd :: tl -> Some (List.fold_left op hd tl)

let min_by cmp list =
  reduce (fun a b -> if cmp a b <= 0 then a else b) list

let min_by_key cmp key list =
  let op a b = cmp (key a) (key b) in 
  min_by op list

let dominating_set graph (node, dominated_nodes) =
  let rec add_remaining_nodes dominating_set remaining_nodes =
    match Common.AtomMap.choose remaining_nodes with
    | exception Not_found -> dominating_set
    | (node, dominated_nodes) ->
       add_remaining_nodes (Common.AtomSet.add node dominating_set)
         (Common.AtomSet.fold Common.AtomMap.remove dominated_nodes
            (Common.AtomMap.remove node remaining_nodes)) in
  add_remaining_nodes (Common.AtomSet.singleton node)
    (Common.AtomSet.fold Common.AtomMap.remove dominated_nodes
       (Common.AtomMap.remove node graph))

let check_dominating_set graph (node, dominated_nodes) =
  let set = dominating_set graph (node, dominated_nodes) in
  if Common.AtomSet.cardinal set >= 2 then
    Some set
  else
    None

let connected_component graph node =
  let rec add_neighbor node accu_set =
    let set = Common.AtomSet.diff (Common.AtomMap.find node graph) accu_set in
    Common.AtomSet.fold add_neighbor set (Common.AtomSet.union accu_set set) in
  add_neighbor node (Common.AtomSet.singleton node)

let nodes_of_graph graph =
  Common.AtomSet.of_seq (Seq.map fst (Common.AtomMap.to_seq graph))

let minimum_st_edge_cut graph s t =
  let value, residual = edmonds_karp graph s t in
  let graph = EdgeSet.fold remove_edge residual graph in
  let reachable = connected_component graph s in
  let non_reachable = Common.AtomSet.diff (nodes_of_graph graph) reachable in
  value, (reachable, non_reachable)

let minimum_edge_cut graph =
  let nodes = Common.AtomMap.bindings graph in
  match List.find_map (check_dominating_set graph) nodes with
  | None ->
     (* This case does not occur in AOC inputs,
        but we handle it for completeness *)
     let node_with_min_degree, _neighbors =
       fst (Option.get (min_by_key Int.compare snd
         (List.map (fun (label, wires) ->
           ((label, wires), Common.AtomSet.cardinal wires)) nodes))) in
     Common.AtomSet.singleton node_with_min_degree,
     Common.AtomSet.remove node_with_min_degree (nodes_of_graph graph)
  | Some set ->
     let v = Common.AtomSet.choose set in
     let cuts =
       List.map (fun w -> minimum_st_edge_cut graph v w)
         (Common.AtomSet.elements (Common.AtomSet.remove v set)) in
     snd (Option.get (min_by_key Int.compare fst cuts))

let part1 wires =
  let wire_pairs =
    List.concat_map (fun (lhs, rhs) -> List.map (fun c -> (lhs, c)) rhs)
      wires in
  let connections =
    List.fold_left add_edge Common.AtomMap.empty wire_pairs in
  let group1, group2 = minimum_edge_cut connections in
  Common.AtomSet.cardinal group1 * Common.AtomSet.cardinal group2

let () =
  let wires = List.of_seq (Seq.map parse_line (Common.input_lines ())) in
  let result_part1 = part1 wires in
  Printf.eprintf "Part 1: %d\n" result_part1

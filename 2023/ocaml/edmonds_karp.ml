
module Flow = Map.Make (Common.OrderedPair (Common.Atom) (Common.Atom))

let find_flow u v flow =
  match Flow.find (u, v) flow with
  | exception Not_found -> 0
  | v -> v

let update_flow u v f flow =
  Flow.update (u, v) (fun v -> Some (f (Option.value ~default:0 v))) flow

let rec bfs_add_neighbors
          flow succ k return u (neighbors : Common.Atom.t Seq.t) pred q =
  match neighbors () with
  | Nil -> k pred q
  | Cons (v, neighbors) ->
     if Common.AtomMap.mem v pred || flow u v >= 1 then
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
    bfs_dequeue graph (fun u v -> find_flow u v flow) succ
      (fun pred q -> bidirectional_bfs_loop graph flow pred q succ q_t)
      (fun v pred -> Some (v, pred, succ))
      q_s pred []
  else
    bfs_dequeue graph (fun u v -> find_flow v u flow) pred
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
     augment (update_flow u v succ (update_flow v u pred flow)) (v :: path)

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
  edmonds_karp_loop graph s t Flow.empty 0  

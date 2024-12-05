let sum seq =
  Seq.fold_left ( + ) 0 seq

let input_lines () =
  Seq.of_dispenser (fun () ->
    try Some (input_line stdin)
    with End_of_file -> None)

let int_of_digit c =
  match c with
  | '0' .. '9' -> Some (int_of_char c - int_of_char '0')
  | _ -> None

let rec break p list =
  match list with
  | [] -> [], []
  | hd :: tl ->
     if p hd then
       [], list
     else
       let left, right = break p tl in
       hd :: left, right

let rec split p list =
  let hd, tl = break p list in
  match tl with
  | [] -> [hd]
  | _ :: tl -> hd :: split p tl

let[@tail_mod_cons] rec tails list =
  list :: strict_tails list
and[@tail_mod_cons] strict_tails list =
  match list with
  | [] -> []
  | _ :: tl -> tails tl

let pairs_with_head list =
  match list with
  | [] -> []
  | hd :: tl -> List.map (fun item -> (hd, item)) tl

let pairs list =
  List.concat_map pairs_with_head (tails list)

module IntHashtbl = Hashtbl.Make (Int)

module Indexer (Key : Hashtbl.HashedType) = struct
  module Table = Hashtbl.Make (Key)

  type t = int

  let compare = Int.compare

  let key_to_int_table = Table.create 16

  let int_to_key_table = IntHashtbl.create 16

  let counter = ref 0

  let get key =
    try
      Table.find key_to_int_table key
    with Not_found ->
      let index = !counter in
      Table.add key_to_int_table key index;
      IntHashtbl.add int_to_key_table index key;
      counter := succ index;
      index

  let get_key i =
    IntHashtbl.find int_to_key_table i
end

module Atom = Indexer (String)

module AtomMap = Map.Make (Atom)

module AtomSet = Set.Make (Atom)

module OrderedPair (A : Set.OrderedType) (B : Set.OrderedType) = struct
  type t = A.t * B.t

  let compare (a0, b0) (a1, b1) =
    match A.compare a0 a1 with
    | 0 -> B.compare b0 b1
    | cmp -> cmp
end

let print_matrix matrix =
  for i = 0 to Array.length matrix - 1 do
    for j = 0 to Array.length matrix.(i) - 1 do
      Printf.eprintf "%f " matrix.(i).(j)
    done;
    Printf.eprintf "\n";
  done;
  Printf.eprintf "\n%!"

let rec range_max_with get ord key low high value =
  if low >= high then
    value
  else
    begin
      let new_value = get low in
      let max_value =
        if ord (key new_value) (key value) > 0 then
          new_value
        else
          value in
      range_max_with get ord key (low + 1) high max_value
    end

let range_max get ord key low high =
  if low >= high then
    failwith "range_max"
  else
    range_max_with get ord key (low + 1) high (get low)

let rec gcd a b =
  if a mod b = 0 then
    b
  else
    gcd b (a mod b)

let lcm a b =
  a * b / gcd a b  

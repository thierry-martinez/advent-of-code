module Coords2D = struct
  type 'a t = {
      x : 'a;
      y : 'a;
    }

  let map f { x; y } =
    { x = f x; y = f y }

  let map2 f a b =
    { x = f a.x b.x; y = f a.y b.y }

  let _format format_coord fmt { x; y } =
    Format.fprintf fmt "%a, %a" format_coord x format_coord y

  module Float = struct
    let det a b =
      a.x *. b.y -. b.x *. a.y

    let ( + ) a b = map2 ( +. ) a b

    let ( * ) k a = map (( *. ) k) a
  end
end

module Coords3D = struct
  type 'a t = {
      x : 'a;
      y : 'a;
      z : 'a;
    }

  let _format format_coord fmt { x; y; z } =
    Format.fprintf fmt "%a, %a, %a" format_coord x format_coord y format_coord z

  let xy { x; y; z = _ } : _ Coords2D.t =
    { x; y }

  let map f { x; y; z } =
    { x = f x; y = f y; z = f z }
end

module Hailstone = struct
  type 'a t = {
      p : 'a;
      v : 'a;
    }

  let map f h = {
      p = f h.p;
      v = f h.v;
    }

  let _format format_coords fmt { p; v } =
    Format.fprintf fmt "%a @@ %a" format_coords p format_coords v
end

let intersect2D (ai : int Coords2D.t Hailstone.t) (bi : int Coords2D.t Hailstone.t) : float Coords2D.t option =
  let a = Hailstone.map (Coords2D.map float_of_int) ai in
  let b = Hailstone.map (Coords2D.map float_of_int) bi in
  let d = Coords2D.Float.det a.v b.v in
  if d = 0. then
    None
  else
    begin
      let t = (Coords2D.Float.det b.p b.v +. Coords2D.Float.det b.v a.p) /. d in
      let t' = (Coords2D.Float.det a.p a.v +. Coords2D.Float.det a.v b.p) /. (-. d) in
      if t >= 0. && t' >= 0. then
        Some Coords2D.Float.(a.p + t * a.v)
      else
        None
    end

let intersect_within min max (a, b) =
  match intersect2D a b with
  | None -> false
  | Some p ->
     min <= p.x && p.x <= max && min <= p.y && p.y <= max

let parse_line s =
  Scanf.sscanf s "%d, %d, %d @ %d, %d, %d"
    (fun px py pz vx vy vz : int Coords3D.t Hailstone.t ->
      { p = { x = px; y = py; z = pz }; v = { x = vx; y = vy; z = vz }})


let gauss_jordan matrix =
  let row_count = Array.length matrix in
  let column_count = Array.length matrix.(0) in
  let rec aux column row =
    if column < column_count && row < row_count then
      begin
        let pivot_row, pivot_value =
          Common.range_max (fun i -> i, matrix.(i).(column)) Float.compare
            (fun (_, v) -> Float.abs v)
            row row_count in
        if pivot_value = 0. then
          aux (column + 1) row
        else
          begin
            if pivot_row <> row then
              begin
                let pivot_row' = matrix.(pivot_row) in
                matrix.(pivot_row) <- matrix.(row);
                matrix.(row) <- pivot_row'
              end;
            matrix.(row).(column) <- 1.;
            for j = column + 1 to column_count - 1 do
              matrix.(row).(j) <- matrix.(row).(j) /. pivot_value;
            done;
            for i = 0 to row_count - 1 do
              if i <> row then
                begin
                  let k = matrix.(i).(column) in
                  for j = column to column_count - 1 do
                    matrix.(i).(j) <- matrix.(i).(j) -. k *. matrix.(row).(j);
                  done;
                end
            done;
            aux (column + 1) (row + 1)
          end
      end in
  aux 0 0

let part2 (hailstones : int Coords3D.t Hailstone.t list) =
  let s0, s1, s2, s3, s4 =
    match hailstones with
    | s0 :: s1 :: s2 :: s3 :: s4 :: _ ->
       Hailstone.map (Coords3D.map float_of_int) s0,
       Hailstone.map (Coords3D.map float_of_int) s1,
       Hailstone.map (Coords3D.map float_of_int) s2,
       Hailstone.map (Coords3D.map float_of_int) s3,
       Hailstone.map (Coords3D.map float_of_int) s4
    | _ -> assert false in
  let matrix_line (s0 : float Coords3D.t Hailstone.t) (s1  : float Coords3D.t Hailstone.t) =
    [| s0.v.y -. s1.v.y; s1.v.x -. s0.v.x; s1.p.y -. s0.p.y; s0.p.x -. s1.p.x;
       s1.p.y *. s1.v.x -. s0.p.y *. s0.v.x +. s0.p.x *. s0.v.y -. s1.p.x *. s1.v.y |] in
  let matrix = [|
      matrix_line s0 s1;
      matrix_line s0 s2;
      matrix_line s0 s3;
      matrix_line s0 s4;
    |] in
  gauss_jordan matrix;
  let x = matrix.(0).(4) in
  let y = matrix.(1).(4) in
  let x' = matrix.(2).(4) in
  let _y' = matrix.(3).(4) in
  let matrix = [|
      [| x' -. s0.v.x; s0.p.x -. x; s0.p.z *. (x' -. s0.v.x) +. s0.v.z *. (s0.p.x -. x) |];
      [| x' -. s1.v.x; s1.p.x -. x; s1.p.z *. (x' -. s1.v.x) +. s1.v.z *. (s1.p.x -. x) |]
    |] in
  gauss_jordan matrix;
  let z = matrix.(0).(2) in
  let _z' = matrix.(1).(2) in
  int_of_float (x +. y +. z)

let () =
  let hailstones = List.of_seq (Seq.map parse_line (Common.input_lines ())) in
(*
  let min, max = 7., 27. in
*)
  let min, max = 200000000000000., 400000000000000. in
  let result_part1 =
    List.length (List.filter (intersect_within min max)
      (Common.pairs (List.map (Hailstone.map Coords3D.xy) hailstones))) in
  Printf.eprintf "Part 1: %d\n" result_part1;
  let result_part2 = part2 hailstones in
  Printf.eprintf "Part 2: %d\n" result_part2

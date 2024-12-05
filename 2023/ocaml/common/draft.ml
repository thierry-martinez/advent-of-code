(*
let rec tails_aux seq =
  Cons (seq, fun () ->
    match seq () with
    | Nil -> Nil
    | Cons (_hd, tl) -> tails_aux tl)

let tails seq () =
  tails_aux seq
*)

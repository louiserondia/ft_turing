type tape = { left : char list; right : char list; v : char; blank : char }

let rec move_left tape =
  match tape.left with
  | [] -> move_left { tape with left = [ tape.blank ] }
  | l_hd :: l_tl ->
      { tape with left = l_tl; v = l_hd; right = tape.v :: tape.right }

let rec move_right tape =
  match tape.right with
  | [] -> move_right { tape with right = [ tape.blank ] }
  | r_hd :: r_tl ->
      { tape with left = tape.v :: tape.left; v = r_hd; right = r_tl }

let rec move_max_left tape =
  match tape with
  | { left = []; _ } -> tape
  | _ -> move_max_left (move_left tape)

let rec move_max_right tape =
  match tape with
  | { right = []; _ } -> tape
  | _ -> move_max_right (move_right tape)

let edit tape v = { tape with v }

let to_string tape =
  (tape.left |> List.rev |> List.to_seq |> String.of_seq)
  ^ "[" ^ String.make 1 tape.v ^ "]"
  ^ (tape.right |> List.to_seq |> String.of_seq)

let create str blank =
  let l = str |> String.to_seq |> List.of_seq in
  let tape = { left = []; v = blank; right = []; blank } in
  let rec f acc l =
    match l with [] -> acc | hd :: tl -> f (move_right (edit acc hd)) tl
  in
  move_max_left (f tape l)

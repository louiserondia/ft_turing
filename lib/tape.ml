type tape = { left : char list; right : char list; v : char }

let move_left tape =
  match tape.left with
  | [] -> tape
  | l_hd :: l_tl -> { left = l_tl; v = l_hd; right = tape.v :: tape.right }

let move_right tape =
  match tape.right with
  | [] -> tape
  | r_hd :: r_tl -> { left = tape.v :: tape.left; v = r_hd; right = r_tl }

let add_left tape v =
  match tape with
  | { left = []; _ } -> { tape with left = v :: tape.left }
  | _ -> assert false

let add_right tape v =
  match tape with
  | { right = []; _ } -> { tape with right = v :: tape.right }
  | _ -> assert false

let rec move_max_left tape =
  match tape with
  | { left = []; _ } -> tape
  | _ -> move_max_left (move_left tape)

let rec move_max_right tape =
  match tape with
  | { right = []; _ } -> tape
  | _ -> move_max_right (move_right tape)

let edit tape v = { tape with v }
let to_list tape = (tape.left |> List.rev) @ [ tape.v ] @ tape.right
let to_string tape = String.of_seq (List.to_seq (to_list tape))
let print tape = print_endline (to_string tape)

let create str =
  let l = str |> String.to_seq |> List.of_seq in
  match l with
  | [] -> assert false
  | l_hd :: l_tl ->
      let tape = { left = []; v = l_hd; right = [] } in
      let rec add_letters tape s =
        match s with
        | [] -> move_max_left tape
        | hd :: tl -> add_letters (move_right (add_right tape hd)) tl
      in
      add_letters tape l_tl

let () =
  let tape = Turing.Tape.init '.' in
  let tape = Turing.Tape.add_right tape 'y' in
  let tape = Turing.Tape.move_right tape in
  let tape = Turing.Tape.add_right tape 'o' in
  let tape = Turing.Tape.move_max_left tape in
  let s = String.of_seq (List.to_seq (Turing.Tape.to_list tape)) in
  print_endline s

module StringMap = Map.Make (String)

let m = StringMap.empty
let m = StringMap.add "coucou" 4 m
let m = StringMap.add "yo" 45 m

let () =
  assert (StringMap.find "coucou" m = 4);
  let instructions = Turing.Instructions.from_json_file "yo.json" in
  let yo =
    Turing.Util.CharMap.find '='
      (Turing.Util.StringMap.find "scanright" instructions.transitions)
  in
  print_endline yo.to_state

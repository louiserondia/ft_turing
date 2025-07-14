let () =
  let tape = Turing.Tape.init '.' in
  let tape = Turing.Tape.add_right tape 'y' in 
  let tape = Turing.Tape.move_right tape in 
  let tape = Turing.Tape.add_right tape 'o' in 
  let tape = Turing.Tape.move_max_left tape in 
let s = String.of_seq (List.to_seq (Turing.Tape.to_list tape)) in
print_endline s

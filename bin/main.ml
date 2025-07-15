let () =
  let instructions =
    match Turing.Instructions.from_json_file "programs/unary_add.json" with
    | Ok instructions -> instructions
    | Error msg -> failwith msg
  in
  let machine = Turing.Machine.init instructions "1111+11=" instructions.initial in

  let machine = Turing.Machine.operations machine in
  Turing.Tape.print machine.tape

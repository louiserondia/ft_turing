let () =
  (*    UNARY ADDITION    *)
  let unary_add_instructions =
    match Turing.Instructions.from_json_file "programs/unary_add.json" with
    | Ok instructions -> instructions
    | Error msg -> failwith msg
  in
  let unary_add =
    Turing.Machine.init unary_add_instructions "1+1="
      unary_add_instructions.initial
  in

  let unary_add = Turing.Machine.operations unary_add in
  Turing.Tape.print unary_add.tape;

  (*      PALINDROM       *)
  let palindrom_instructions =
    match Turing.Instructions.from_json_file "programs/palindrom.json" with
    | Ok instructions -> instructions
    | Error msg -> failwith msg
  in
  let palindrom =
    Turing.Machine.init palindrom_instructions "booob"
      palindrom_instructions.initial
  in

  let palindrom = Turing.Machine.operations palindrom in
  Turing.Tape.print palindrom.tape;

  (*         0n1n         *)
  let onin_instructions =
    match Turing.Instructions.from_json_file "programs/0n1n.json" with
    | Ok instructions -> instructions
    | Error msg -> failwith msg
  in
  let onin =
    Turing.Machine.init onin_instructions "000111" onin_instructions.initial
  in

  let onin = Turing.Machine.operations onin in
  Turing.Tape.print onin.tape;

  (*         02n         *)
  let o2n_instructions =
    match Turing.Instructions.from_json_file "programs/02n.json" with
    | Ok instructions -> instructions
    | Error msg -> failwith msg
  in
  let o2n =
    Turing.Machine.init o2n_instructions "000000000000" o2n_instructions.initial
  in

  let o2n = Turing.Machine.operations o2n in
  Turing.Tape.print o2n.tape

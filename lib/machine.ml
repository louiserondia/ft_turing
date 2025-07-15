open Util

type machine = {
  instructions : Instructions.instructions;
  tape : Tape.tape;
  op : string;
}

let init instructions tape_str op =
  { instructions; tape = Tape.create tape_str; op }

let operation machine =
  let transitions =
    StringMap.find machine.op machine.instructions.transitions
  in
  let transition_rule = CharMap.find machine.tape.v transitions in
  let tape = Tape.edit machine.tape transition_rule.write in
  let tape =
    match transition_rule.action with
    | Left -> (
        match tape with
        | { left = []; _ } -> Tape.add_left tape machine.instructions.blank
        | _ -> tape)
    | Right -> (
        match tape with
        | { right = []; _ } -> Tape.add_right tape machine.instructions.blank
        | _ -> tape)
  in
  let tape =
    match transition_rule.action with
    | Left -> Tape.move_left tape
    | Right -> Tape.move_right tape
  in
  { instructions = machine.instructions; tape; op = transition_rule.to_state }

let rec operations machine =
  (* Tape.print machine.tape; *)
  match StringSet.mem machine.op machine.instructions.finals with
  | true -> machine
  | false -> operations (operation machine)

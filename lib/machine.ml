open Util

type machine = {
  instructions : Instructions.instructions;
  tape : Tape.tape;
  op : string;
}

let init instructions tape op = { instructions; tape; op }

let operation machine (tape : Tape.tape) =
  let transitions =
    StringMap.find machine.op machine.instructions.transitions
  in
  let transition_rule = CharMap.find tape.v transitions in
  let tape = Tape.edit tape transition_rule.write in
  let tape =
    match transition_rule.action with
    | Left -> Tape.move_left tape
    | Right -> Tape.move_right tape
  in
  { instructions = machine.instructions; tape; op = transition_rule.to_state }

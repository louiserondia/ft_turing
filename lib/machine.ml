open Util

type machine = {
  instructions : Instructions.instructions;
  tape : Tape.tape;
  op : string;
}

let init instructions tape_str op =
  { instructions; tape = Tape.create tape_str instructions.blank; op }

let print_info machine =
  Printf.printf "name: %s\n" machine.instructions.name;
  let s =
    list_to_string (CharSet.to_list machine.instructions.alphabet) (fun c ->
        String.make 1 c)
  in
  Printf.printf "alphabet: %s\n" s;
  let s =
    Util.list_to_string (StringSet.to_list machine.instructions.states)
      (fun s -> s)
  in
  Printf.printf "states: %s\n" s;
  Printf.printf "initial: %s\n" machine.instructions.initial;
  let s =
    Util.list_to_string (StringSet.to_list machine.instructions.finals)
      (fun s -> s)
  in
  Printf.printf "finals: %s\n" s

let operation machine =
  let transitions =
    StringMap.find machine.op machine.instructions.transitions
  in
  let transition_rule = CharMap.find machine.tape.v transitions in
  let tape = Tape.edit machine.tape transition_rule.write in
  let tape =
    match transition_rule.action with
    | Left -> Tape.move_left tape
    | Right -> Tape.move_right tape
  in
  { instructions = machine.instructions; tape; op = transition_rule.to_state }

let rec operations machine =
  machine.tape |> Tape.to_string |> print_endline;
  match StringSet.mem machine.op machine.instructions.finals with
  | true -> machine
  | false -> operations (operation machine)

open Util

type machine = {
  instructions : Instructions.instructions;
  tape : Tape.tape;
  op : string;
}

exception InvalidInput

let check_tape_str s (instructions : Instructions.instructions) =
  if String.exists (fun c -> not (CharSet.mem c instructions.alphabet)) s then
    raise InvalidInput;
  if String.contains s instructions.blank then raise InvalidInput

let init instructions tape_str op =
  try
    check_tape_str tape_str instructions;
    Ok { instructions; tape = Tape.create tape_str instructions.blank; op }
  with InvalidInput -> Error "invalid input"

let transition_to_string s c (rule : Instructions.transition_rule) =
  Format.sprintf "(%s, %c) -> (%s, %c, %s)" s c rule.to_state rule.write
    (match rule.action with Left -> "LEFT" | Right -> "RIGHT")

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
  Printf.printf "finals: %s\n" s;
  StringMap.iter
    (fun s ->
      fun m ->
       CharMap.iter
         (fun c ->
           fun (rule : Instructions.transition_rule) ->
            transition_to_string s c rule |> print_endline)
         m)
    machine.instructions.transitions

let operation machine =
  let stuck_message =
    "the machine is stuck by a problem in states logic"
  in
  let transitions =
    match StringMap.find_opt machine.op machine.instructions.transitions with
    | Some v -> v
    | None ->
        Printf.eprintf "%s\n" stuck_message;
        exit 1
  in
  let transition_rule =
    match CharMap.find_opt machine.tape.v transitions with
    | Some v -> v
    | None ->
        Printf.eprintf "%s\n" stuck_message;
        exit 1
  in
  (machine.tape |> Tape.to_string)
  ^ "\t"
  ^ transition_to_string machine.op machine.tape.v transition_rule
  |> print_endline;
  let tape = Tape.edit machine.tape transition_rule.write in
  let tape =
    match transition_rule.action with
    | Left -> Tape.move_left tape
    | Right -> Tape.move_right tape
  in
  { instructions = machine.instructions; tape; op = transition_rule.to_state }

let rec operations machine =
  match StringSet.mem machine.op machine.instructions.finals with
  | true -> machine
  | false -> operations (operation machine)

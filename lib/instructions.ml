open Util

type transition_rule = { to_state : string; write : char; action : action }

type instructions = {
  name : string;
  alphabet : CharSet.t;
  blank : char;
  states : StringSet.t;
  initial : string;
  finals : StringSet.t;
  transitions : transition_rule CharMap.t StringMap.t;
}

open Yojson.Safe

type transition_json = {
  read : string;
  to_state : string;
  write : string;
  action : string;
}
[@@deriving yojson]

type instructions_json = {
  name : string;
  alphabet : string list;
  blank : string;
  states : string list;
  initial : string;
  finals : string list;
  transitions : Yojson.Safe.t;
}
[@@deriving yojson]

let parse_transitions (json : Yojson.Safe.t) :
    (string * transition_json list) list =
  match json with
  | `Assoc state_transitions ->
      List.map
        (fun (state, transitions_json) ->
          let transitions_list = Util.to_list transitions_json in
          let transitions =
            List.map
              (fun json ->
                match transition_json_of_yojson json with
                | Ok t -> t
                | Error msg -> failwith msg)
              transitions_list
          in
          (state, transitions))
        state_transitions
  | _ -> assert false

exception SchemaError

let from_json_file filename : (instructions, string) result =
  try
    let json = from_file filename in
    match instructions_json_of_yojson json with
    | Ok instructions_json ->
        Ok
          {
            name = instructions_json.name;
            alphabet =
              List.fold_left
                (fun acc c -> CharSet.add c.[0] acc)
                CharSet.empty instructions_json.alphabet;
            blank = instructions_json.blank.[0];
            states = to_string_set instructions_json.states;
            initial = instructions_json.initial;
            finals = to_string_set instructions_json.finals;
            transitions =
              (match instructions_json.transitions with
              | `Assoc transitions ->
                  List.fold_left
                    (fun acc (name, transitions) ->
                      StringMap.add name
                        (List.fold_left
                           (fun acc transition ->
                             let transition =
                               match transition_json_of_yojson transition with
                               | Ok t -> t
                               | Error _ -> assert false
                             in
                             CharMap.add transition.read.[0]
                               {
                                 to_state = transition.to_state;
                                 write = transition.write.[0];
                                 action =
                                   (match transition.action with
                                   | "LEFT" -> Left
                                   | "RIGHT" -> Right
                                   | _ -> raise SchemaError);
                               }
                               acc)
                           CharMap.empty (Util.to_list transitions))
                        acc)
                    StringMap.empty transitions
              | _ -> assert false);
          }
    | Error _ -> raise SchemaError
  with
  | Yojson.Json_error _ -> Error "invalid JSON"
  | Sys_error msg -> Error msg
  | SchemaError -> Error "invalid schema/logic"

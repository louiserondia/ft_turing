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

exception SchemaError

let string_to_char s e = match String.length s with 1 -> s.[0] | _ -> raise e

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
                (fun acc s -> CharSet.add (string_to_char s SchemaError) acc)
                CharSet.empty instructions_json.alphabet;
            blank = string_to_char instructions_json.blank SchemaError;
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
                               | Error _ -> raise SchemaError
                             in
                             CharMap.add
                               (string_to_char transition.read SchemaError)
                               {
                                 to_state = transition.to_state;
                                 write =
                                   string_to_char transition.write SchemaError;
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
              | _ -> raise SchemaError);
          }
    | Error _ -> raise SchemaError
  with
  | Yojson.Json_error _ -> Error "invalid JSON"
  | Sys_error msg -> Error msg
  | SchemaError -> Error "invalid schema/logic"

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

val from_json_file : string -> (instructions, string) result

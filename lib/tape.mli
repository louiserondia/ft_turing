type tape = { left : char list; right : char list; v : char }

val move_left : tape -> tape
val move_right : tape -> tape
val add_left : tape -> char -> tape
val add_right : tape -> char -> tape
val move_max_left : tape -> tape
val move_max_right : tape -> tape
val edit : tape -> char -> tape
val to_string : tape -> string
val print : tape -> unit
val create : string -> tape

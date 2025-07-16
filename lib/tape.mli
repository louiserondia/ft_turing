type tape = { left : char list; right : char list; v : char ; blank : char }

val move_left : tape -> tape
val move_right : tape -> tape
val move_max_left : tape -> tape
val move_max_right : tape -> tape
val edit : tape -> char -> tape
val to_string : tape -> string
val create : string -> char -> tape

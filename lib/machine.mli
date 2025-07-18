type machine = {
  instructions : Instructions.instructions;
  tape : Tape.tape;
  op : string;
}

val init :
  Instructions.instructions -> string -> string -> (machine, string) result

val print_info : machine -> unit
val operation : machine -> machine
val operations : machine -> machine

let () =
  let filename, input =
    let help () =
      let usage =
        "usage: ft_turing [-h] jsonfile input\n\
         positional arguments:\n\
         \tjsonfile: json description of the machine\n\
         \tinput: input of the machine\n\
         optional arguments:\n\
         \t-h, --help: show this help message and exit"
      in
      print_endline usage;
      exit 0
    in
    match Array.sub Sys.argv 1 (Array.length Sys.argv - 1) |> Array.to_list with
    | [ filename; input ] -> (filename, input)
    | [ "-h" ] | [ "--help" ] -> help ()
    | l when List.mem "-h" l || List.mem "--help" l -> help ()
    | _ ->
        Printf.eprintf "wrong number of arguments\n";
        exit 1
  in
  let instructions =
    match Turing.Instructions.from_json_file filename with
    | Ok instructions -> instructions
    | Error msg ->
        Printf.eprintf "%s\n" msg;
        exit 1
  in
  let machine =
    match Turing.Machine.init instructions input instructions.initial with
    | Ok v -> v
    | Error msg ->
        Printf.eprintf "%s\n" msg;
        exit 1
  in
  Turing.Machine.print_info machine;
  String.make 50 '*' |> print_endline;
  let machine = Turing.Machine.operations machine in
  Turing.Tape.to_string machine.tape |> print_endline

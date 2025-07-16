let () =
  let filename, input =
    let help () =
      print_endline "usage...";
      exit 0
    in
    match Array.sub Sys.argv 1 (Array.length Sys.argv - 1) |> Array.to_list with
    | [ filename; input ] -> (filename, input)
    | [ "-h" ] | [ "--help" ] -> help ()
    | l when List.mem "-h" l || List.mem "--help" l -> help ()
    | _ -> failwith "wrong number of arguments"
  in
  let instructions =
    match Turing.Instructions.from_json_file filename with
    | Ok instructions -> instructions
    | Error msg -> failwith msg
  in
  let machine = Turing.Machine.init instructions input instructions.initial in
  Turing.Machine.print_info machine;
  let machine = Turing.Machine.operations machine in
  Turing.Tape.to_string machine.tape |> print_endline

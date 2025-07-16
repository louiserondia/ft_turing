module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module CharMap = Map.Make (Char)
module CharSet = Set.Make (Char)

let to_string_set l =
  List.fold_left (fun acc s -> StringSet.add s acc) StringSet.empty l

let list_to_string l converter =
  let rec f acc l =
    match l with
    | [] -> " ]" :: acc
    | hd :: [] -> f (converter hd :: acc) []
    | hd :: tl -> f ((converter hd ^ ", ") :: acc) tl
  in
  String.concat "" (f [ "[ " ] l |> List.rev)

type action = Left | Right

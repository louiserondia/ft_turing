module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module CharMap = Map.Make (Char)
module CharSet = Set.Make (Char)

let to_string_set l =
  List.fold_left (fun acc s -> StringSet.add s acc) StringSet.empty l

type action = Left | Right

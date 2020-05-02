type typ =
  | Int
  | Fun of typ * typ

let rec type_to_string t =
  match t with
  | Int -> "int"
  | Fun (t1,t2) -> "("^type_to_string t1^"->"^type_to_string t2^")"
;;

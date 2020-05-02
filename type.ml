type typ =
  | Int
  | Fun of typ * typ
  | Var of int

let rec type_to_string t =
  match t with
  | Int -> "int"
  | Fun (t1,t2) -> "("^type_to_string t1^"->"^type_to_string t2^")"
  | Var i -> "T_"^string_of_int i
;;
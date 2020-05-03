open Printf

type typ =
  | Int
  | Float
  | Fun of typ * typ
  | Var of int

let rec type_to_string t =
  match t with
  | Int -> "int"
  | Float -> "float"
  | Fun (t1,t2) -> "("^type_to_string t1^"->"^type_to_string t2^")"
  | Var i -> "T_"^string_of_int i
;;

let show_type t = printf "%s\n" (type_to_string t) ;;

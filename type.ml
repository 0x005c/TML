open Printf

type typ =
  | Int
  | Float
  | Bool
  | String
  | Fun of typ * typ
  | Var of int
  | Unit
  | Tuple of typ list
;;

let rec type_to_string t =
  match t with
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | String -> "string"
  | Fun (t1,t2) -> "("^type_to_string t1^"->"^type_to_string t2^")"
  | Var i -> "T_"^string_of_int i
  | Unit -> "unit"
  | Tuple (x::xs) -> List.fold_left
      (fun s -> fun x -> s^","^type_to_string x)
      ("("^type_to_string x) xs ^ ")"
  | Tuple [] -> Printf.eprintf "broken tuple\n" ; exit 1
;;

let show_type t = printf "%s\n" (type_to_string t) ;;

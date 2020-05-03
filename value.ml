type value =
  | Int of int
  | Float of float
  | Closure of string * (string * value) list * Exp.exp
;;

let value_to_string v =
  match v with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Closure _ -> "(closure)"
;;

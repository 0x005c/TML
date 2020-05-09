type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Closure of string * (string * value) list * Exp.exp
  | LazyExp of Exp.exp
;;

let value_to_string v =
  match v with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | LazyExp e -> "(exp"^Exp.exp_to_string e^")"
  | Closure _ -> "(closure)"
;;

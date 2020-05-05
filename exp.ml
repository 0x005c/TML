type exp =
  | Int of int
  | Float of float
  | Bool of bool
  | Var of string
  | Apply of exp * exp
  | Not of exp
  | IAdd of exp * exp
  | ISub of exp * exp
  | IMul of exp * exp
  | IDiv of exp * exp
  | FAdd of exp * exp
  | FSub of exp * exp
  | FMul of exp * exp
  | FDiv of exp * exp
  | Fun of string * exp
;;

let rec exp_to_string e =
  match e with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Var s -> s
  | Apply (e1,e2) -> "("^exp_to_string e1^" "^exp_to_string e2^")"
  | Not e -> "(not "^exp_to_string e^")"
  | IAdd (e1,e2) -> "("^exp_to_string e1^"+"^exp_to_string e2^")"
  | ISub (e1,e2) -> "("^exp_to_string e1^"-"^exp_to_string e2^")"
  | IMul (e1,e2) -> "("^exp_to_string e1^"*"^exp_to_string e2^")"
  | IDiv (e1,e2) -> "("^exp_to_string e1^"/"^exp_to_string e2^")"
  | FAdd (e1,e2) -> "("^exp_to_string e1^"+."^exp_to_string e2^")"
  | FSub (e1,e2) -> "("^exp_to_string e1^"-."^exp_to_string e2^")"
  | FMul (e1,e2) -> "("^exp_to_string e1^"*."^exp_to_string e2^")"
  | FDiv (e1,e2) -> "("^exp_to_string e1^"/."^exp_to_string e2^")"
  | Fun (s,e) -> "(fun "^s^"->"^exp_to_string e^")"
;;

type exp =
  | Int of int
  | Float of float
  | Bool of bool
  | Var of string
  | If of exp * exp * exp
  | Let of string * exp * exp
  | Apply of exp * exp
  | Not of exp
  | And of exp * exp
  | Or of exp * exp
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
  | If (e1,e2,e3) -> "(if "^exp_to_string e1^" then "
      ^exp_to_string e2^" else "^exp_to_string e3^")"
  | Let (s,e1,e2) -> "(let "^s^"="^exp_to_string e1^" in "^exp_to_string e2^")"
  | Var s -> s
  | Apply (e1,e2) -> "("^exp_to_string e1^" "^exp_to_string e2^")"
  | Not e -> "(not "^exp_to_string e^")"
  | And (e1,e2) -> "("^exp_to_string e1^" and "^exp_to_string e2^")"
  | Or (e1,e2) -> "("^exp_to_string e1^" or "^exp_to_string e2^")"
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

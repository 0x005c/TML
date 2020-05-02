type exp =
  | Int of int
  | Var of string
  | Apply of exp * exp
  | IAdd of exp * exp
  | ISub of exp * exp

let rec exp_to_string e =
  match e with
  | Int i -> string_of_int i
  | Var s -> s
  | Apply (e1,e2) -> "("^exp_to_string e1^" "^exp_to_string e2^")"
  | IAdd (e1,e2) -> "("^exp_to_string e1^"+"^exp_to_string e2^")"
  | ISub (e1,e2) -> "("^exp_to_string e1^"-"^exp_to_string e2^")"

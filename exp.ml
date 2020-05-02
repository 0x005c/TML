type exp =
  | Int of int
  | Var of string
  | Apply of exp * exp
  | IAdd of exp * exp
  | ISub of exp * exp


type value =
  | Int of int
  | Float of float
  | Bool of bool
  | Closure of string * (string * value) list * Exp.exp
  | Builtin of (value -> value)
  | LazyExp of Exp.exp
  | Unit
;;

let value_to_string v =
  match v with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | LazyExp e -> "(exp"^Exp.exp_to_string e^")"
  | Closure _ -> "(closure)"
  | Builtin _ -> "(builtin)"
  | Unit -> "()"
;;

let runtime_error () =
  Printf.eprintf "Evaluation failed";
  exit 1
;;

let get_i v =
  match v with
  | Int i -> i
  | _ -> runtime_error ()
;;

let get_f v =
  match v with
  | Float f -> f
  | _ -> runtime_error ()
;;

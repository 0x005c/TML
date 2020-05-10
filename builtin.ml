let builtin_i2f v = Value.Float (float_of_int (Value.get_i v)) ;;
let builtin_f2i v = Value.Int (int_of_float (Value.get_f v)) ;;
let builtin_print_i v = Printf.printf "%d\n" (Value.get_i v) ; Value.Unit ;;
let builtin_print_f v = Printf.printf "%f\n" (Value.get_f v) ; Value.Unit ;;
let builtin_print_b v = Printf.printf "%b\n" (Value.get_b v) ; Value.Unit ;;

let builtin_table =
  [ ("i2f", Type.Fun (Type.Int, Type.Float), Value.Builtin builtin_i2f)
  ; ("f2i", Type.Fun (Type.Float, Type.Int), Value.Builtin builtin_f2i)
  ; ("print_i", Type.Fun (Type.Int, Type.Unit), Value.Builtin builtin_print_i)
  ; ("print_f", Type.Fun (Type.Float, Type.Unit), Value.Builtin builtin_print_f)
  ; ("print_b", Type.Fun (Type.Bool, Type.Unit), Value.Builtin builtin_print_b) ]
;;

let builtin_tenv =
  List.fold_right (fun (s,t,_) -> fun r -> (s,t)::r) (List.rev builtin_table) []
;;

let builtin_venv =
  List.fold_right (fun (s,_,b) -> fun r -> (s,b)::r) (List.rev builtin_table) []
;;

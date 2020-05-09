let builtin_i2f v = Value.Float (float_of_int (Value.get_i v)) ;;
let builtin_f2i v = Value.Int (int_of_float (Value.get_f v)) ;;

let builtin_table =
  [ ("i2f", Type.Fun (Type.Int, Type.Float), Value.Builtin builtin_i2f)
  ; ("f2i", Type.Fun (Type.Float, Type.Int), Value.Builtin builtin_f2i) ]
;;

let builtin_tenv =
  List.fold_right (fun (s,t,_) -> fun r -> (s,t)::r) (List.rev builtin_table) []
;;

let builtin_venv =
  List.fold_right (fun (s,_,b) -> fun r -> (s,b)::r) (List.rev builtin_table) []
;;

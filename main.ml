open Printf

let unify_check t1 t2 =
  printf "unify %s %s\n" (Type.type_to_string t1) (Type.type_to_string t2);
  let asgn = Infer.unify [(t1,t2)] in
  printf "unified %s %s\n"
      (Type.type_to_string (Infer.assign asgn t1))
      (Type.type_to_string (Infer.assign asgn t2))
;;

let rec show_assign a =
  match a with
  | Infer.Set [] -> printf ""
  | Infer.Set ((s,t)::a') -> printf "%s:=%s\n" (Type.type_to_string s) (Type.type_to_string t); show_assign (Set a')
  | Infer.Compose (a1,a2) -> show_assign a1; printf "######\n"; show_assign a2
;;

let _ =
  let exp = Parse.top Lexer.token (Lexing.from_channel stdin) in
  let es = Exp.exp_to_string exp in
  eprintf "exp: %s\n" es;
  let t = Infer.infer exp in
  let ts = Type.type_to_string t in
  eprintf "type: %s\n" ts;
  let v = Eval.evaluate exp in
  let vs = Value.value_to_string v in
  eprintf "value: %s\n" vs
;;

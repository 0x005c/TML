open Parse
open Printf
open Exp
open Infer
open Type

let unify_check t1 t2 =
  printf "unify %s %s\n" (type_to_string t1) (type_to_string t2);
  let asgn = Infer.unify [(t1,t2)] in
  printf "unified %s %s\n"
      (type_to_string (Infer.assign asgn t1))
      (type_to_string (Infer.assign asgn t2))
;;

let rec show_assign a =
  match a with
  | Set [] -> printf ""
  | Set ((s,t)::a') -> printf "%s:=%s\n" (type_to_string s) (type_to_string t); show_assign (Set a')
  | Compose (a1,a2) -> show_assign a1; printf "######\n"; show_assign a2
;;

let _ =
  let exp = Parse.top Lexer.token (Lexing.from_channel stdin) in
  let t = infer exp in
  let es = exp_to_string exp in
  let ts = type_to_string t in
  printf "exp: %s\ntype: %s\n" es ts
;;

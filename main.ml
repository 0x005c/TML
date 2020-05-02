open Parse
open Printf
open Exp
open Infer
open Type

let _ =
let exp = Parse.top Lexer.token (Lexing.from_channel stdin) in
let t = infer exp in
let es = exp_to_string exp in
let ts =
  match t with
  | Some a -> type_to_string a
  | None -> "(none)"
in
printf "exp: %s\ntype: %s\n" es ts
;;

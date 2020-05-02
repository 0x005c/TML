open Parse
open Printf
open Exp

let _ =
let exp = Parse.top Lexer.token (Lexing.from_channel stdin) in
let s = exp_to_string exp^"\n" in
printf "%s" s


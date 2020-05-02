open Parse
open Printf

let _ = Parse.exp Lexer.token (Lexing.from_channel stdin) ;;


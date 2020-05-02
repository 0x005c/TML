open Parse
open Printf

let _ = Parse.top Lexer.token (Lexing.from_channel stdin) ;;


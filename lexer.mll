{
open Parse
}

let space = [' ' '\t' '\n' '\r']
let identc = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']

rule token = parse
| space+ { token lexbuf }
| eof { EOF }
| "int" { TY_INT }
| "fun" { FUN }
| "let" { LET }
| "in" { IN }
| "+" { PLUS }
| "-" { MINUS }
| "=" { EQUAL }
| "->" { ARROW }
| ":" { COLON }
| "(" { LPAREN }
| ")" { RPAREN }
| digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) }
| identc(identc|digit)* { IDENT(Lexing.lexeme lexbuf) }


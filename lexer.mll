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
| "bool" { TY_BOOL }
| "float" { TY_FLOAT }
| "true" { TRUE }
| "false" { FALSE }
| "not" { NOT }
| "and" { AND }
| "or" { OR }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "fun" { FUN }
| "let" { LET }
| "in" { IN }
| "+" { PLUS }
| "-" { MINUS }
| "*" { STAR }
| "/" { SLASH }
| "+." { PLUSDOT }
| "-." { MINUSDOT }
| "*." { STARDOT }
| "/." { SLASHDOT }
| "=" { EQUAL }
| "==" { EQEQ }
| "->" { ARROW }
| ":" { COLON }
| "(" { LPAREN }
| ")" { RPAREN }
| digit+"."digit+ { FLOAT(float_of_string(Lexing.lexeme lexbuf)) }
| digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) }
| identc(identc|digit)* { IDENT(Lexing.lexeme lexbuf) }

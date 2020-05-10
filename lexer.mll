{
open Parse
}

let space = [' ' '\t' '\n' '\r']
let identc = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let stringtext = [^'"']

rule token = parse
| space+ { token lexbuf }
| eof { EOF }
| "int" { TY_INT }
| "bool" { TY_BOOL }
| "float" { TY_FLOAT }
| "string" { TY_STRING }
| "unit" { TY_UNIT }
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
| "rec" { REC }
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
| "<" { LT }
| ">" { GT }
| "<=" { LE }
| ">=" { GE }
| "<>" { LTGT }
| "->" { ARROW }
| ":" { COLON }
| "(" { LPAREN }
| ")" { RPAREN }
| "\""stringtext+"\"" { STRING(Lexing.lexeme lexbuf) }
| digit+"."digit+ { FLOAT(float_of_string(Lexing.lexeme lexbuf)) }
| digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) }
| identc(identc|digit)* { IDENT(Lexing.lexeme lexbuf) }

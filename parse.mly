%{
open Exp
%}

%token <int> INT
%token <string> IDENT
%token TY_INT
%token FUN
%token LET
%token IN
%token PLUS
%token MINUS
%token EQUAL
%token ARROW
%token COLON
%token LPAREN
%token RPAREN

%type <Exp.exp> atexp appexp infexp exp

%start exp

%%

atexp:
  | INT { Int($1) }
  | IDENT { Var($1) }
  | LPAREN exp RPAREN { $2 }
  ;

appexp:
  | atexp { $1 }
  ;

infexp:
  | appexp { $1 }
  ;

exp:
  | infexp { $1 }
  ;


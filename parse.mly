%{
open Exp
%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token TY_INT
%token FUN
%token LET
%token IN
%token PLUS
%token MINUS
%token PLUSDOT
%token MINUSDOT
%token EQUAL
%token ARROW
%token COLON
%token LPAREN
%token RPAREN
%token EOF

%type <Exp.exp> top atexp appexp infexp exp

%start top

%%

top:
  | exp EOF { $1 }
  ;

atexp:
  | INT { Int($1) }
  | FLOAT { Float($1) }
  | IDENT { Var($1) }
  | LPAREN exp RPAREN { $2 }
  ;

appexp:
  | atexp { $1 }
  | appexp atexp { Apply($1, $2) }
  ;

infexp:
  | appexp { $1 }
  | infexp PLUS appexp { IAdd($1, $3) }
  | infexp MINUS appexp { ISub($1, $3) }
  | infexp PLUSDOT appexp { FAdd($1, $3) }
  | infexp MINUSDOT appexp { FSub($1, $3) }
  ;

exp:
  | infexp { $1 }
  | FUN IDENT ARROW exp { Lambda ($2, $4) }
  ;

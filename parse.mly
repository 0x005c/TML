%{
open Exp
%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token TY_BOOL
%token TRUE
%token FALSE
%token TY_INT
%token FUN
%token LET
%token IN
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token PLUSDOT
%token MINUSDOT
%token STARDOT
%token SLASHDOT
%token EQUAL
%token ARROW
%token COLON
%token LPAREN
%token RPAREN
%token EOF

%type <Exp.exp> top atexp appexp infexp exp

%left PLUS MINUS PLUSDOT MINUSDOT
%left STAR SLASH STARDOT SLASHDOT

%start top

%%

top:
  | exp EOF { $1 }
  ;

atexp:
  | INT { Int($1) }
  | FLOAT { Float($1) }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | IDENT { Var($1) }
  | LPAREN exp RPAREN { $2 }
  ;

appexp:
  | atexp { $1 }
  | appexp atexp { Apply($1, $2) }
  ;

infexp:
  | appexp { $1 }
  | infexp PLUS infexp { IAdd($1, $3) }
  | infexp MINUS infexp { ISub($1, $3) }
  | infexp STAR infexp { IMul($1, $3) }
  | infexp SLASH infexp { IDiv($1, $3) }
  | infexp PLUSDOT infexp { FAdd($1, $3) }
  | infexp MINUSDOT infexp { FSub($1, $3) }
  | infexp STARDOT infexp { FMul($1, $3) }
  | infexp SLASHDOT infexp { FDiv($1, $3) }
  ;

exp:
  | infexp { $1 }
  | FUN IDENT ARROW exp { Fun ($2, $4) }
  ;

%{
open Exp
%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token TY_BOOL
%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token TY_INT
%token IF
%token THEN
%token ELSE
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

%left OR
%left AND

%left PLUS MINUS
%left STAR SLASH

%left PLUSDOT MINUSDOT
%left STARDOT SLASHDOT

%start top

%%

top:
  | exp EOF { $1 }
  ;

atexp:
  | NOT atexp { Not $2 }
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
  | infexp OR infexp { Or($1,$3) }
  | infexp AND infexp { And($1,$3) }
  ;

exp:
  | infexp { $1 }
  | FUN IDENT ARROW exp { Fun ($2, $4) }
  | IF exp THEN exp ELSE exp { If ($2,$4,$6) }
  ;

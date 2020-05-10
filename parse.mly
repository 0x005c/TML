%{
open Exp
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token TY_INT
%token TY_BOOL
%token TY_FLOAT
%token TY_STRING
%token TY_UNIT
%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token IF
%token THEN
%token ELSE
%token FUN
%token LET
%token IN
%token REC
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token PLUSDOT
%token MINUSDOT
%token STARDOT
%token SLASHDOT
%token EQUAL
%token EQEQ
%token LTGT
%token LT
%token GT
%token LE
%token GE
%token ARROW
%token COLON
%token LPAREN
%token RPAREN
%token EOF

%type <Exp.exp> top atexp appexp infexp exp
%type <Type.typ> ty

%left OR
%left AND

%left EQEQ LTGT LT GT LE GE

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
  | STRING { String(String.sub $1 1 ((String.length $1)-2)) }
  | IDENT { Var($1) }
  | LPAREN exp RPAREN { $2 }
  | LPAREN RPAREN { Unit }
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
  | infexp EQEQ infexp { Eq($1,$3) }
  | infexp LTGT infexp { Ne($1,$3) }
  | infexp LT infexp { Lt($1,$3) }
  | infexp GT infexp { Gt($1,$3) }
  | infexp LE infexp { Le($1,$3) }
  | infexp GE infexp { Ge($1,$3) }
  | infexp OR infexp { Or($1,$3) }
  | infexp AND infexp { And($1,$3) }
  ;

exp:
  | infexp { $1 }
  | LPAREN exp COLON ty RPAREN { Annot ($2,$4) }
  | FUN IDENT ARROW exp { Fun ($2, $4) }
  | IF exp THEN exp ELSE exp { If ($2,$4,$6) }
  | LET IDENT EQUAL exp IN exp { Let ($2,$4,$6) }
  | LET IDENT idents EQUAL exp IN exp {
      let (s,ss,e1,e2) = ($2,$3,$5,$7) in
      (* let f f = ... みたいな式も通ってしまう *)
      let f = List.fold_right (fun s -> fun e -> Exp.Fun (s,e)) ss e1 in
      Exp.Let (s,f,e2)
  }
  | LET REC IDENT EQUAL exp IN exp { LetRec ($3,$5,$7) }
  | LET REC IDENT idents EQUAL exp IN exp {
      let (s,ss,e1,e2) = ($3,$4,$6,$8) in
      let f = List.fold_right (fun s -> fun e -> Exp.Fun (s,e)) ss e1 in
      Exp.LetRec (s,f,e2)
  }
  ;

ty:
  | TY_INT { Type.Int }
  | TY_FLOAT { Type.Float }
  | TY_BOOL { Type.Bool }
  | TY_STRING { Type.String }
  | TY_UNIT { Type.Unit }
  ;

idents:
  | IDENT { [$1] }
  | IDENT idents { $1::$2 }
  ;

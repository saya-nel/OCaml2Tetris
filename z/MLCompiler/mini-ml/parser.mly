%{
  open Parseutils
  open Ast
  let pos () = 
    make_position (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ())
  
  let lam = List.fold_right (fun x e -> Lam(x,e))

%}


/* (* reserved words *) */
%token LAM LET REC IN IF THEN ELSE WHILE DO DONE

%token <string> IDENT
%token <int> INT

%token PLUS MINUS TIMES DIV AND OR EQ NEQ GT LT GE LE NOT TRUE FALSE

/* (* control characters *) */
%token EOF TERMINAISON LEFT_ARROW LPAREN RPAREN

%nonassoc LET 
%nonassoc IN

%nonassoc  IF

%left      OR
%left      AND
%left      EQ NEQ GT GE LT LE
%left      PLUS MINUS        
%left      TIMES DIV                            
%nonassoc  IDENT LPAREN RPAREN DOT /* highest precedence */        


%start prog         /* the entry point */

%type <Ast.prog>      prog
%type <Ast.exp>       expr

%%

prog : 
| EOF                                   { [] }
| TERMINAISON prog                      { $2 }
| LET IDENT idents EQ expr prog         { ($2,(lam $3 $5))::$6 }
| LET REC IDENT idents EQ expr prog     { ($3,(lam $4 $6))::$7 }
;

idents:
|              { [] }
| IDENT idents { $1::$2 }
;

expr: 
 | expr exp                             { App($1,$2) }
 | exp                                  { $1 }
 ;

exp: 
 | LAM IDENT idents LEFT_ARROW expr     { Lam($2,(lam $3 $5)) } 
 | LET IDENT idents EQ expr IN expr     { Let(($2,(lam $3 $5)),$7) }
 | LET REC IDENT idents EQ expr IN expr { Rec(($3,(lam $4 $6)),$8) }
 | IF expr THEN expr ELSE expr          { If($2,$4,$6) }
 | WHILE expr DO expr DONE              { While($2,$4) }
 | expr expr                            { App($1,$2) }
 | LPAREN expr RPAREN                   { $2 }
 | IDENT                                { Var($1) }
 | INT                                  { Val($1) }
;
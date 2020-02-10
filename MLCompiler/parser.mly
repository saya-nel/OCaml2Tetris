%{
  open Parseutils
  open Ast
  let pos () = 
    make_position (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ())
%}


/* (* reserved words *) */
%token LET IN IF THEN ELSE WHILE FOR DO DONE MATCH WITH PIPE BEGIN END EXTERNAL
%token UNIT_TY BOOL_TY INT_TY STRING_TY ARRAY_TY 

%token <string> IDENT IDENT_CAPITALIZE VM_IDENT
%token <string> STRING
%token <int> INT
%token <bool> BOOL

%token PLUS MINUS TIMES DIV AND OR EQ NEQ GT LT GE LE NOT TRUE FALSE TYPE
%token REC
/* (* control characters *) */
%token EOF TERMINAISON DOT COLON LPAREN RPAREN LBRACKET RBRACKET SEMICOL BEGIN END
%token ARRAY_OPEN ARRAY_CLOSE ARRAY_ACCESS_OPEN LEFT_ARROW RIGHT_ARROW ASSIGN ACCESS REF WILDCARD


%nonassoc LET 
%right SEMICOL
%left apply
%nonassoc IN
%nonassoc ARRAY_OPEN ARRAY_CLOSE

%right COLON /* lowest precedence */ 
%nonassoc  IF
%right     LEFT_ARROW ASSIGN
/* %right     COMMA */
%left      OR
%left      AND
%left      EQ NEQ GT GE LT LE
%left      PLUS MINUS        
%left      TIMES DIV               
%left      DOT  
%left      ACCESS                
%nonassoc  IDENT LPAREN RPAREN BEGIN END        /* highest precedence */        


%start prog         /* the entry point */

%type <Ast.prog>       prog
%type <Ast.expr>       expr
%type <Ast.sum_type>   sum_type
%type <Ast.match_case> match_case

%%

prog :
 | EOF                    { [] }
 | decl prog              { $1::$2 }
 | decl TERMINAISON prog  { $1::$3 }
 ;

decl : 
 | TYPE IDENT EQ ty             { Type($2,$4,pos()) }
 | LET IDENT args EQ expr       { Decl($2,$3,$5,pos()) }
 | LET REC IDENT args EQ expr   { RecDecl($3,$4,$6,pos()) }
 | EXTERNAL IDENT 
   COLON expr_ty EQ STRING { (* let s = String.concat "."  (String.split_on_char '_' $7) in  *)
                                                              External($2,$4,$6,pos()) }
 | EXTERNAL IDENT COLON expr_ty EQ error                         { raise (Parse_Exception ("malformed external :",pos())) }
 | error                                 { raise (Parse_Exception ("malformed declaration :",pos())) }
 ;

ty :
 | sum_type                      { Sum($1) }
 /* | expr_ty                       { Ty_expr($1,pos()) }*/
 ;


sum_type :
| sum_type_aux { $1 }
| PIPE sum_type_aux { $2 }
;

sum_type_aux :
 | constructor               { [$1] }
 | constructor PIPE sum_type_aux { $1::$3 }
 ;

constructor :
|  IDENT_CAPITALIZE                { $1 }
;

expr_ty:
 | LPAREN expr_ty RPAREN         { $2 }
 | IDENT                         { Ident_ty($1,pos()) }
 | ident_in_mod                  { Ident_ty($1,pos()) }
 | star_ty                       { Star_ty($1,pos()) }
 | expr_ty RIGHT_ARROW expr_ty   { Arrow_ty($1,$3,pos()) }
;

star_ty :
| expr_ty TIMES star_ty_aux      {$1::$3}
;
star_ty_aux :
| expr_ty                         {[$1]}
| expr_ty TIMES star_ty_aux       {$1::$3}
args : 
| arg       { [$1] }
| arg args  { $1::$2 }
;
arg : 
| IDENT          { $1 }
| LPAREN RPAREN  { "_" }
;

/*idents : 
| IDENT         { [$1] }
| IDENT idents  { $1::$2 }
;*/

ident_in_mod:
| IDENT                  { $1 }
| IDENT_CAPITALIZE DOT ident_in_mod { $1 ^ "." ^ $3 }
;

exprs :
 | expr        { [$1] }
 | expr exprs  { $1::$2 }
 ;

expr: 
 | LPAREN expr RPAREN                    { $2 }
 | BEGIN expr END                        { $2 }
 | constant                              { Constant($1,pos()) }
 | IDENT                                 { Ident($1,pos()) }
 | ident_in_mod                          { Ident($1,pos()) }
 | expr exprs                            { App($1,$2,pos()) }
 | LET arg EQ expr IN expr               { Let($2,$4,$6,pos()) }
 | IF expr THEN expr ELSE expr           { If($2,$4,$6,pos())}
 | MATCH expr WITH match_body            { Match($2,$4,pos())}
 | expr PLUS expr                        { BinOp(Ast.Add, $1, $3,pos()) }
 | expr MINUS expr                       { BinOp(Ast.Minus, $1, $3,pos()) }
 | expr EQ expr                          { BinOp(Ast.Eq, $1, $3,pos()) }
 | expr NEQ expr                         { BinOp(Ast.Neq, $1, $3,pos()) }
 | expr GT expr                         { BinOp(Ast.Gt, $1, $3,pos()) }
 | expr LT expr                         { BinOp(Ast.Lt, $1, $3,pos()) }
 | expr GE expr                         { BinOp(Ast.Ge, $1, $3,pos()) }
 | expr LE expr                         { BinOp(Ast.Le, $1, $3,pos()) }
 | expr OR expr                         { BinOp(Ast.Or, $1, $3,pos()) }
 | expr AND expr                         { BinOp(Ast.And, $1, $3,pos()) }
 | NOT expr                              { UnOp(Ast.Not, $2,pos()) }
 | LPAREN MINUS expr RPAREN              { UnOp(Ast.UMinus, $3,pos()) }

 | WHILE expr DO expr DONE               { While($2,$4,pos()) }
 | FOR IDENT IN expr DO expr DONE        { For($2,$4,$6,pos()) }
 | ARRAY_OPEN array_content ARRAY_CLOSE  { Array_create($2,pos()) }
 | expr ARRAY_ACCESS_OPEN expr RPAREN    { Array_get($1,$3,pos()) }
 | expr ARRAY_ACCESS_OPEN expr RPAREN LEFT_ARROW expr { Array_assign($1,$3,$6,pos()) }
 | ACCESS expr                           { Access ($2,pos()) } 
 | expr ASSIGN expr                      { Assign ($1,$3,pos()) } 
 | REF expr                              { Ref ($2,pos())} 
 | expr SEMICOL expr                     { Seq($1,$3,pos()) }
 | error                                 { raise (Parse_Exception ("malformed expression",pos())) }
;

constant:
 | LPAREN RPAREN                         { Unit }
 | INT                                   { Int($1) }
 | BOOL                                  { Bool($1) }
 | STRING                                { String($1) }
 | constructor                           { Constructor($1) }
 ;

match_body:
| match_body_aux       {$1}
| PIPE match_body_aux  {$2}

match_body_aux:
| match_case                     { [$1] }
| match_case PIPE match_body_aux { $1::$3 }
;
match_case:
| WILDCARD RIGHT_ARROW expr  { Otherwise($3,pos()) }
| constant RIGHT_ARROW expr  { Case($1,$3,pos()) }
;

array_content:
|                            { [] }
| array_content_aux          { $1 }
;

array_content_aux:
|                                 { [] }
| expr                            { [$1] }
| expr SEMICOL array_content_aux  { $1::$3 }
;

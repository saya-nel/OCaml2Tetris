%{
  open Parseutils
  open Ast
  let pos () = 
    make_position (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ())
%}

/* (* reserved words *) */
%token LET IN IF THEN ELSE WHILE FOR DO DONE MATCH WITH PIPE

%token <string> IDENT
%token <string> STRING
%token <int> INT
%token <bool> BOOL

%token PLUS MINUS TIMES DIV AND OR EQ NEQ GT LT GE LE NOT TRUE FALSE
%token REC
/* (* control characters *) */
%token EOF TERMINAISON DOT COLON LPAREN RPAREN LBRACKET RBRACKET SEMICOL BEGIN END 
%token ARRAY_OPEN ARRAY_CLOSE ARRAY_ACCESS_OPEN LEFT_ARROW RIGHT_ARROW ASSIGN ACCESS REF WILDCARD

%nonassoc LET IN 
%nonassoc ARRAY_OPEN ARRAY_CLOSE LEFT_ARROW

/* (* operators *) */
%right     COLON SEMICOL  /* lowest precedence */ 
%left      OR
%left      AND
%left      EQ NEQ GT GE LT LE
%left      PLUS MINUS        
%left      TIMES DIV               
%left      DOT                  
%nonassoc  LPAREN RPAREN         /* highest precedence */        


%start prog         /* the entry point */

%type <Ast.prog>       prog
%type <Ast.expr>       expr
%type <Ast.match_case> match_case

%%


prog :
 | EOF                    { [] }
 | decl prog              { $1::$2 }
 | decl TERMINAISON prog  { $1::$3 }
 ;

decl :
 | LET IDENT args EQ expr       { Decl($2,$3,$5,pos()) }
 | LET REC IDENT args EQ expr       { RecDecl($3,$4,$6,pos()) }
 ;

args : 
| arg       { [$1] }
| arg args  { $1::$2 }
;
arg : 
| IDENT          { $1 }
| LPAREN RPAREN  { "_" }
;

idents : 
| IDENT         { [$1] }
| IDENT idents  { $1::$2 }
;

exprs :
 | expr        { [$1] }
 | expr exprs  { $1::$2 }
 ;

expr :
 | LPAREN expr RPAREN                    { $2 }
 | constant                              { Constant($1,pos()) }
 | IDENT                                 { Ident($1,pos()) }
 | expr exprs                            { App($1,$2,pos()) }
 | LET IDENT EQ expr IN expr             { Let($2,$4,$6,pos()) }
 | LPAREN sequence RPAREN                { $2 }
 | IF expr THEN expr ELSE expr           { If($2,$4,$6,pos())}
 | MATCH expr WITH match_body            { Match($2,$4,pos())}
 | expr PLUS expr                        { BinOp("+", $1, $3,pos()) }
 | expr MINUS expr                       { BinOp("-", $1, $3,pos()) }
 | expr TIMES expr                       { BinOp("*", $1, $3,pos()) }
 | expr DIV expr                         { BinOp("div", $1, $3,pos()) }
 | expr EQ expr                          { BinOp("=", $1, $3,pos()) }
 | expr NEQ expr                         { BinOp("<>", $1, $3,pos()) }
 | expr GE expr                          { BinOp(">=", $1, $3,pos()) }
 | expr GT expr                          { BinOp(">", $1, $3,pos()) }
 | expr LE expr                          { BinOp("<=", $1, $3,pos()) }
 | expr LT expr                          { BinOp("<", $1, $3,pos()) }
 | expr OR expr                          { BinOp("or", $1, $3,pos()) }
 | expr AND expr                         { BinOp("and", $1, $3,pos()) }
 | NOT expr                              { UnOp("not", $2,pos()) }
 | LPAREN MINUS expr RPAREN              { UnOp("~", $3,pos()) }

 | WHILE expr DO expr DONE               { While($2,$4,pos()) }
 | FOR IDENT IN expr DO expr DONE        { For($2,$4,$6,pos()) }
 | ARRAY_OPEN array_content ARRAY_CLOSE  { Array_create($2,pos()) }
 | expr ARRAY_ACCESS_OPEN expr RPAREN    { Array_get($1,$3,pos()) }
 | expr ARRAY_ACCESS_OPEN expr RPAREN LEFT_ARROW expr { Array_assign($1,$3,$6,pos()) }
 | ACCESS expr                           {Access ($2,pos())} 
 | expr ASSIGN expr                      {Assign ($1,$3,pos())} 
 | REF expr                              {Ref ($2,pos())} 
 | error                                 { raise (Parse_Exception ("malformed expression",pos())) }
;

constant:
 | LPAREN RPAREN                         { Unit }
 | INT                                   { Int($1) }
 | BOOL                                  { Bool($1) }
 | STRING                                { String($1) }
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
sequence:
| expr SEMICOL sequence_aux { Seq($1::$3,pos()) }
;

sequence_aux : 
| expr                     { [$1] }
| expr SEMICOL sequence_aux  { $1::$3 }
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

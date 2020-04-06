%{
  open Parseutils
  open Ast
  open Types
%}


/* (* reserved words *) */
%token LET WHERE IN IF THEN ELSE ASSERT WHILE FOR TO DO DONE MATCH WITH PIPE BEGIN END EXTERNAL AND_KW CONS
%token UNIT_TY BOOL_TY INT_TY STRING_TY ARRAY_TY ATAT FUN TVAR

%token <string> IDENT IDENT_CAPITALIZE VM_IDENT
%token <string> STRING
%token <char> CHAR
%token <int> INT
%token <bool> BOOL

%token PLUS MINUS TIMES DIV AND OR LAND LOR EQ NEQ GT LT GE LE NOT TRUE FALSE TYPE
%token REC
/* (* control characters *) */
%token EOF TERMINAISON DOT COLON LPAREN RPAREN LBRACKET RBRACKET SEMICOL BEGIN END COMMA OF
%token ARRAY_OPEN ARRAY_CLOSE ARRAY_ACCESS_OPEN LEFT_ARROW RIGHT_ARROW ASSIGN ACCESS WILDCARD


%nonassoc LET 
%nonassoc WHERE 
%right SEMICOL
%nonassoc IN
%nonassoc ARRAY_OPEN ARRAY_CLOSE

/* ATAT */

%right COLON /* lowest precedence */ 
%nonassoc  IF
%right     LEFT_ARROW ASSIGN
/* %right     COMMA */
%right     CONS /* ??? */
%left      OR
%left      AND
%left      EQ NEQ GT GE LT LE
%left      PLUS MINUS     
%left      LAND
%left      LOR 
%left TIMES DIV              
%left      DOT  
%left      ACCESS                
%nonassoc  IDENT LPAREN RPAREN BEGIN END        /* highest precedence */        


%start tmodule         /* the entry point */

%type <Ast.decl list>  tmodule
%type <Ast.exp>        expr
%type <Types.typ>         ty
%type <Ast.match_case> match_case

%%

tmodule:
decls {$1}
;

decls :
 | EOF                      { [] }
 | decl decls               { $1::$2 }
 | decl terminaison decls   { $1::$3 }
 | { error_exit (pos()) "programme malformé" }
 ;

 terminaison:
 |                         {}
 | TERMINAISON terminaison {}
 | error { error_exit (pos()) "fin de phrase. `;;` attendues." }
 ;

decl : 
 | LET argu EQ seq                            { match $2 with 
		                                       | None,None -> Exp($4)
		                                       | None,Some t -> Exp(Annotation($4,t))
		                                       | Some x,tyopt -> DefVar((x,tyopt),$4) }
 | LET defuns                                 { DefFun($2) }
 | LET REC defuns                             { DefFunRec($3) }
 | TYPE IDENT EQ expr_ty                      { Type($2,$4) }
 | LET error { error_exit (pos()) "déclaration `let` malformée. J'attend {let <ident> [...] = <expr> in <expr>}" }
 | error { error_exit (pos()) "déclaration malformée (`let` ou `type` attendu)" }
 ;

defun:
| IDENT argus EQ seq { ($1,$2,None,$4) }
| IDENT argus COLON expr_ty EQ seq { ($1,$2,Some $4,$6) }
;

defuns:
| defun                {[$1]}
| defun AND_KW defuns  {$1::$3}
;
ignore:
| WILDCARD {}
| LPAREN RPAREN {}
;

ty :
 /* | sum_type   { Sum($1) } */
 | expr_ty    { $1 }
 ;

/*sum_type:
| sum_ty {$1} 
| PIPE sum_ty {$2} 
;*/

/*sum_ty :
 | constructor             { [$1] }
 | constructor PIPE sum_ty { $1::$3 }
 | constructor OF            { error_exit (pos()) "constructeur paramétré non supporté" }
 ;*/

constructor :
| IDENT_CAPITALIZE                { $1 }
| IDENT_CAPITALIZE DOT constructor { $1 ^ "." ^ $3}
;

expr_ty:
 | LPAREN expr_ty RPAREN         { $2 }
 | IDENT                         { match $1 with 
 	                               | "int" -> Tint
 	                               | "unit" -> Tunit
 	                               | "bool" -> Tbool
 	                               | "char" -> Tchar
 	                               | "string" -> Tstring
 	                               | s -> Tident(s) }
 | TVAR IDENT                    { let v = Tvar (V.create ()) in  
 								   match $2 with 
                                   | "array" -> Tarray v 
                                   | "ref" -> Tref v
                                   | "tlist" -> Tlist v
                                   | s -> Tconstr(s,[v])  }
 | ident_in_mod                  { Tident($1) }
 | expr_ty TIMES expr_ty         { Tproduct($1,$3) }
 | expr_ty RIGHT_ARROW expr_ty   { Tarrow($1,$3) }
 | error { error_exit (pos()) "expression de type malformée." }
;

args : 
| arg                             { [$1] }
| LPAREN arg COLON expr_ty RPAREN { [$2] }
| arg args  { $1::$2 }
| error { error_exit (pos()) "liste d'arguments malformée." }
;
arg : 
| IDENT          { $1 }
| LPAREN RPAREN  { "_" }
| error { error_exit (pos()) "argument malformé." }
;

ident_in_mod:
| IDENT                  { $1 }
| IDENT_CAPITALIZE DOT ident_in_mod { $1 ^ "." ^ $3 }
;

seq :
| expression                 { $1 }
| expression SEMICOL seq     { Seq($1,$3) }
;

expression : 
| ACCESS expr                            { Ref_access($2) } 
| NOT expr                               { UnOp(Not,$2) }
| expr                                   { $1 }
| FUN argu_strict RIGHT_ARROW seq        { Fun($2,$4) }
| LET argu EQ seq IN seq                 { match $2 with 
	                                       | None,None -> Seq($4,$6)
	                                       | None,Some t -> Seq(Annotation($4,t),$6)
	                                       | Some x,tyopt -> Let((x,tyopt),$4,$6) }
| expression WHERE argu EQ seq           { match $3 with 
	                                       | None,None -> Seq($5,$1)
	                                       | None,Some t -> Seq(Annotation($5,t),$1)
	                                       | Some x,tyopt -> Let((x,tyopt),$5,$1) }
| IF seq THEN expression ELSE expression { If($2,$4,$6)}
| IF seq THEN expression                 { If($2,$4,Constant(Unit))}
| MATCH seq WITH match_body              { Match($2,$4)}
| WHILE seq DO seq DONE                  { While($2,$4) }
| FOR IDENT EQ seq TO seq DO seq DONE    { For($2,$4,$6,$8) }
;


argu:
| argu_aux                               { $1 }
| argu_aux COLON expr_ty                 { let (c,_) = $1 in (c,Some $3) }
;
argu_aux:
| IDENT                                  { (Some $1,None) }
| WILDCARD                               { (None,None) }
| LPAREN RPAREN                          { (None,Some Tunit)}
| LPAREN argu RPAREN                     { $2 }
;


argu_strict:
| LPAREN argu_strict RPAREN                     { $2 }
| IDENT                                         { ($1,None) }
| IDENT COLON expr_ty                           { ($1,Some $3) }
;

argu_p:
| IDENT                                { ($1,None) }
| LPAREN IDENT COLON expr_ty RPAREN    { ($2,Some $4) } 
| WILDCARD                             { ("_",None) } 
| LPAREN RPAREN                        { ("_",Some Tunit) } 
| LPAREN LPAREN RPAREN COLON expr_ty RPAREN  { if $5 <> Tunit
                                               then error_exit (pos()) "le motif () doit avoir le type unit." 
                                               else ("_",Some Tunit) } 
| LPAREN argu_p RPAREN          { $2 } 
;

argus : 
| argu_p             { [$1] }
| argu_p argus       { $1::$2 }
| error { error_exit (pos()) "liste d'arguments malformée." }
;

expr: 
 | app                                   { $1 } 
 | expression PLUS expression            { BinOp(Add,$1,$3) }
 | expression MINUS expression           { BinOp(Minus,$1,$3) }
 | expression TIMES expression           { BinOp(Mult,$1,$3) }
 | expression DIV expression             { BinOp(Div,$1,$3) }
 | expression EQ expression              { BinOp(Eq,$1,$3) }
 | expression NEQ expression             { BinOp(Neq,$1,$3) }
 | expression GT expression              { BinOp(Gt,$1,$3) }
 | expression LT expression              { BinOp(Lt,$1,$3) }
 | expression GE expression              { BinOp(Ge,$1,$3) }
 | expression LE expression              { BinOp(Le,$1,$3) }
 | expression OR expression              { BinOp(Or,$1,$3) }
 | expression AND expression             { BinOp(And,$1,$3) }
 | expression LOR expression             { BinOp(Lor,$1,$3) }
 | expression LAND expression            { BinOp(Land,$1,$3) }
 | expr ASSIGN expression                { Ref_assign($1,$3) } 
 | MINUS expr                            { UnOp(UMinus,$2) }
 | expression COMMA expression 		     { Pair($1,$3) }
 | expression CONS expression 		     { Cons($1,$3) }
/* | error                                 { raise (Parse_Exception ("malformed expression ")) }*/
;

app:
 | exp                                   { $1 }
 | exp exprs                             { App($1,$2) }
 | exp ATAT app                          { App($1,[$3]) }
 | ASSERT exp                            { Assert ($2) }
 ;

exprs :
 | exp        { [$1] }
 | exp exprs  { $1::$2 }
 ;

exp:
| LPAREN expression COLON expr_ty RPAREN { Annotation($2,$4) }
| LPAREN seq RPAREN                     { $2 }
| BEGIN seq END                         { $2 }
| constant                              { Constant($1) }
| IDENT                                 { Ident($1) }
| ident_in_mod                          { Ident($1) }
| ARRAY_OPEN array_content ARRAY_CLOSE  { Array_create($2) }
| exp ARRAY_ACCESS_OPEN seq RPAREN     { Array_access($1,$3) }
| exp ARRAY_ACCESS_OPEN seq RPAREN LEFT_ARROW expression { Array_assign($1,$3,$6) }
| error { error_exit (pos()) "expression malformée." }
;

constant:
 | LPAREN RPAREN                         { Unit }
 | INT                                   { Int($1) }
 | CHAR                                  { Char($1) }
 | BOOL                                  { Bool($1) }
 | STRING                                { String($1) }
 | constructor                           { Constr($1) }
 | LBRACKET RBRACKET                     { List_empty }
 | ARRAY_OPEN ARRAY_CLOSE                { Array_empty }
 ;

match_body:
| match_body_aux       {$1}
| PIPE match_body_aux  {$2}

match_body_aux:
| match_case                     { [$1] }
| match_case PIPE match_body_aux { $1::$3 }
;
match_case:
| WILDCARD RIGHT_ARROW seq  { Otherwise($3) }
| constant RIGHT_ARROW seq  { Case($1,$3) }
;

array_content:
|                            { [] }
| array_content_aux          { $1 }
;

array_content_aux:
|                                       { [] }
| expression                            { [$1] }
| expression SEMICOL array_content_aux  { $1::$3 }
;


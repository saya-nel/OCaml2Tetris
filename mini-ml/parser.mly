%{
  open Parseutils
  open Past
  open Types

let decl_create d = Past.{decl_desc = d; decl_loc = pos()}
let exp_create e = Past.{exp_desc = e; exp_loc = pos()}


%}


/* (* reserved words *) */
%token LET WHERE IN IF THEN ELSE ASSERT WHILE FOR TO DO DONE MATCH WITH PIPE BEGIN END EXTERNAL AND_KW CONS
%token UNIT_TY BOOL_TY INT_TY STRING_TY ARRAY_TY ATAT FUN SHARP

%token <string> IDENT IDENT_CAPITALIZE VM_IDENT
%token <string> STRING
%token <char> CHAR
%token <int> INT
%token <bool> BOOL

%token <char> TVAR

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

%type <Past.decl list>  tmodule
%type <Past.exp>        expr
%type <Types.typ>         ty
%type <Past.match_case> match_case

%%

tmodule:
decls {$1}
;

decls :
 | EOF                      { [] }
 | decl decls               { (decl_create $1)::$2 }
 | decl terminaison decls   { (decl_create $1)::$3 }
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
		                                       | None,Some t -> Exp(exp_create @@ Annotation($4,t))
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
/*
constructor :
| IDENT_CAPITALIZE                { $1 }
| IDENT_CAPITALIZE DOT constructor { $1 ^ "." ^ $3}
;
*/
expr_ty:
 | LPAREN expr_ty RPAREN         { $2 }
 | IDENT                         { match $1 with 
 	                               | "int" -> Tint
 	                               | "unit" -> Tunit
 	                               | "bool" -> Tbool
 	                               | "char" -> Tchar
 	                               | "string" -> Tstring
 	                               | s -> Tident(s) }
 | TVAR                          { Tvar (V.create ()) }
 | expr_ty IDENT                 { match $2 with 
                                   | "array" -> Tarray $1 
                                   | "ref" -> Tref $1
                                   | "list" -> Tlist $1
                                   | s -> Tconstr(s,[$1])  }
 | ident_in_mod                  { Tident($1) }
 | expr_ty TIMES expr_ty         { Tproduct($1,$3) }
 | expr_ty RIGHT_ARROW expr_ty   { Tarrow($1,$3) }
 | error { error_exit (pos()) "expression de type malformée." }
;


ident_in_mod:
| IDENT                  { $1 }
| IDENT_CAPITALIZE DOT ident_in_mod { $1 ^ "." ^ $3 }
;

seq :
| expression                 { $1 }
| expression SEMICOL seq     { exp_create @@ Seq($1,$3) }
;

expression : 
| ACCESS expr                            { exp_create @@ Ref_access($2) } 
| NOT expr                               { exp_create @@ UnOp(Not,$2) }
| expr                                   { $1 }
| FUN argu_strict RIGHT_ARROW seq        { exp_create @@ Fun($2,$4) }
| LET argu_strict EQ seq IN seq          { exp_create @@ Let($2,$4,$6) }
| LET defuns IN seq                      
 { 
    List.fold_right
       (fun (name,args,tyopt,e) exp ->
    	exp_create @@ Let((name,None),
    		List.fold_right 
    		  (fun a e -> exp_create @@ Fun(a,e)) 
    		  args (match tyopt with
		    		| None -> e 
		    		| Some ty -> exp_create @@ Annotation(e,ty)),
    		exp))
         $2 $4}
| expression WHERE argu EQ seq           { exp_create @@ 
	                                       match $3 with 
	                                       | None,None -> Seq($5,$1)
	                                       | None,Some t -> Seq(exp_create @@ Annotation($5,t),$1)
	                                       | Some x,tyopt -> Let((x,tyopt),$5,$1) }
| IF seq THEN expression ELSE expression { exp_create @@ If($2,$4,$6) }
| IF seq THEN expression                 { exp_create @@ If($2,$4,exp_create @@ Constant(Unit))}
| MATCH seq WITH match_body              { exp_create @@ Match($2,$4)}
| WHILE seq DO seq DONE                  { exp_create @@ While($2,$4) }
| FOR IDENT EQ seq TO seq DO seq DONE    { exp_create @@ For($2,$4,$6,$8) }
;


argu:
| argu_aux                               { $1 }
| argu_aux COLON expr_ty                 { let (c,_) = $1 in (c,Some $3) }
| error { error_exit (pos()) "argument malformé." }
;
argu_aux:
| IDENT                                  { (Some $1,None) }
| WILDCARD                               { (None,None) }
| LPAREN RPAREN                          { (None,Some Tunit)}
| LPAREN argu RPAREN                     { $2 }
| error { error_exit (pos()) "argument malformé." }
;


argu_strict:
| LPAREN argu_strict RPAREN                     { $2 }
| LPAREN RPAREN                                 { ("_",Some Tunit) } 
| argu_strict_aux                               { ($1,None)}
| argu_strict_aux COLON expr_ty                 { ($1,Some $3) }
| error { error_exit (pos()) "argument malformé." }
;
argu_strict_aux:
| IDENT                                         { $1 }
| WILDCARD                                      { "_" } 
;


argu_p:
| IDENT                                { ($1,None) }
| LPAREN IDENT COLON expr_ty RPAREN    { ($2,Some $4) } 
| WILDCARD                             { ("_",None) } 
| LPAREN RPAREN                        { ("_",Some Tunit) } 
| LPAREN argu_p RPAREN          { $2 } 
| error { error_exit (pos()) "argument malformé." }
;

argus : 
| argu_p             { [$1] }
| argu_p argus       { $1::$2 }
| error { error_exit (pos()) "liste d'arguments malformée." }
;

expr: 
 | app                                   { $1 } 
 | expression PLUS expression            { exp_create @@ BinOp(Add,$1,$3) }
 | expression MINUS expression           { exp_create @@ BinOp(Minus,$1,$3) }
 | expression TIMES expression           { exp_create @@ BinOp(Mult,$1,$3) }
 | expression DIV expression             { exp_create @@ BinOp(Div,$1,$3) }
 | expression EQ expression              { exp_create @@ BinOp(Eq,$1,$3) }
 | expression NEQ expression             { exp_create @@ BinOp(Neq,$1,$3) }
 | expression GT expression              { exp_create @@ BinOp(Gt,$1,$3) }
 | expression LT expression              { exp_create @@ BinOp(Lt,$1,$3) }
 | expression GE expression              { exp_create @@ BinOp(Ge,$1,$3) }
 | expression LE expression              { exp_create @@ BinOp(Le,$1,$3) }
 | expression OR expression              { exp_create @@ BinOp(Or,$1,$3) }
 | expression AND expression             { exp_create @@ BinOp(And,$1,$3) }
 | expression LOR expression             { exp_create @@ BinOp(Lor,$1,$3) }
 | expression LAND expression            { exp_create @@ BinOp(Land,$1,$3) }
 | expr ASSIGN expression                { exp_create @@ Ref_assign($1,$3) } 
 | MINUS expr                            { exp_create @@ UnOp(UMinus,$2) }
 | expression COMMA expression 		     { exp_create @@ Pair($1,$3) }
 | expression CONS expression 		     { exp_create @@ Cons($1,$3) }
/* | error                                 { raise (Parse_Exception ("malformed expression ")) }*/
;

app:
 | exp                                   { $1 }
 | exp exprs                             { exp_create @@ App($1,$2) }
 | exp ATAT app                          { exp_create @@ App($1,[$3]) }
 | SHARP exp                             { exp_create @@ Magic($2) }
 | ASSERT exp                            { exp_create @@ Assert ($2,pos()) }
 ;

exprs :
 | exp        { [$1] }
 | exp exprs  { $1::$2 }
 ;

exp:
| LPAREN expression COLON expr_ty RPAREN { exp_create @@ Annotation($2,$4) }
| LPAREN seq RPAREN                     { $2 }
| BEGIN seq END                         { $2 }
| constant                              { exp_create @@ Constant($1) }
| IDENT                                 { exp_create @@ Ident($1) }
| ident_in_mod                          { exp_create @@ Ident($1) }
| ARRAY_OPEN array_content ARRAY_CLOSE  { exp_create @@ Array_create($2) }
| exp ARRAY_ACCESS_OPEN seq RPAREN     { exp_create @@ Array_access($1,$3) }
| exp ARRAY_ACCESS_OPEN seq RPAREN LEFT_ARROW expression { exp_create @@ Array_assign($1,$3,$6) }
| error { error_exit (pos()) "expression malformée." }
;

constant:
 | LPAREN RPAREN                         { Unit }
 | INT                                   { Int($1) }
 | CHAR                                  { Char($1) }
 | BOOL                                  { Bool($1) }
 | STRING                                { String($1) }
 /* | constructor                           { Constr($1) }*/
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


type token =
  | LET
  | IN
  | IF
  | THEN
  | ELSE
  | WHILE
  | FOR
  | DO
  | DONE
  | MATCH
  | WITH
  | PIPE
  | BEGIN
  | END
  | EXTERNAL
  | UNIT_TY
  | BOOL_TY
  | INT_TY
  | STRING_TY
  | ARRAY_TY
  | IDENT of (string)
  | IDENT_CAPITALIZE of (string)
  | VM_IDENT of (string)
  | STRING of (string)
  | INT of (int)
  | BOOL of (bool)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | AND
  | OR
  | EQ
  | NEQ
  | GT
  | LT
  | GE
  | LE
  | NOT
  | TRUE
  | FALSE
  | TYPE
  | REC
  | EOF
  | TERMINAISON
  | DOT
  | COLON
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | SEMICOL
  | ARRAY_OPEN
  | ARRAY_CLOSE
  | ARRAY_ACCESS_OPEN
  | LEFT_ARROW
  | RIGHT_ARROW
  | ASSIGN
  | ACCESS
  | REF
  | WILDCARD

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog

type token =
  | LAM
  | LET
  | REC
  | IN
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | DONE
  | IDENT of (string)
  | INT of (int)
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
  | EOF
  | TERMINAISON
  | LEFT_ARROW
  | LPAREN
  | RPAREN

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog

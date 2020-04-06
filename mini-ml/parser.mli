type token =
  | LET
  | WHERE
  | IN
  | IF
  | THEN
  | ELSE
  | ASSERT
  | WHILE
  | FOR
  | TO
  | DO
  | DONE
  | MATCH
  | WITH
  | PIPE
  | BEGIN
  | END
  | EXTERNAL
  | AND_KW
  | CONS
  | UNIT_TY
  | BOOL_TY
  | INT_TY
  | STRING_TY
  | ARRAY_TY
  | ATAT
  | FUN
  | TVAR
  | IDENT of (string)
  | IDENT_CAPITALIZE of (string)
  | VM_IDENT of (string)
  | STRING of (string)
  | CHAR of (char)
  | INT of (int)
  | BOOL of (bool)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | AND
  | OR
  | LAND
  | LOR
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
  | COMMA
  | OF
  | ARRAY_OPEN
  | ARRAY_CLOSE
  | ARRAY_ACCESS_OPEN
  | LEFT_ARROW
  | RIGHT_ARROW
  | ASSIGN
  | ACCESS
  | WILDCARD

val tmodule :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.decl list

{
  open Parseutils
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}

let ident = ['a'-'z''_'] ['a'-'z''A'-'Z''0'-'9''_']*

rule token = parse
| ['0'-'9']+ as lxm  { INT(int_of_string lxm) }
| ['(']              { LPAREN }
| [')']              { RPAREN }
| ";;"               { TERMINAISON }
| "fun"              { LAM }
| "->"               { LEFT_ARROW }
| "let"              { LET }
| "rec"              { REC }
| "in"               { IN }
| "if"               { IF }
| "then"             { THEN }
| "else"             { ELSE }
| "while"            { WHILE }
| "do"               { DO }
| "done"             { DONE }
| "not"              { NOT }
| "+"                { PLUS }
| "-"                { MINUS }
| "*"                { TIMES }
| "/"                { DIV }
| "&&"               { AND }
| "||"               { OR }
| "="                { EQ }
| "<>"               { NEQ }
| ">"                { GT }
| "<"                { LT }
| ">="               { GE }
| "<="               { LE }
| ident as lxm       { IDENT(lxm) }
| ['\n']             { (Lexing.new_line lexbuf) ; (token lexbuf) }
| [' ' '\t']         { token lexbuf }    (* skip blanks *)
| "(*"               { comment lexbuf }  (* Comment until closing *)
| eof | "eof"              { EOF }
| _  as lxm          { raise (Parse_Exception (Printf.sprintf "Unexpected character: %c"  lxm,  default_position)) }


and comment = parse 
| "*)" { token lexbuf }
| _    { comment lexbuf } 
{
  open Parseutils
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}

let vm_ident = ['a'-'z''A'-'Z''0'-'9''_']+
let ident = ['a'-'z''_'] ['a'-'z''A'-'Z''0'-'9''_']*
let ident_capitalize = ['A'-'Z'] ['a'-'z''A'-'Z''0'-'9''_']*
let module_ident = ['A'-'Z'] ['a'-'z''A'-'Z''0'-'9''_']*

rule token = parse
| ['0'-'9']+ as lxm  { INT(int_of_string lxm) }
| "type"             { TYPE }
| "external"         { EXTERNAL } (* external int_of_string : string -> int = f *)
| "true"             { BOOL(true) }
| "false"            { BOOL(false) }
| ':'                { COLON }
| '('                { LPAREN }
| ')'                { RPAREN }
| '['                { LBRACKET }
| ']'                { RBRACKET }
| ';'                { SEMICOL }
| ";;"               { TERMINAISON }
| '.'                { DOT }
| "let"              { LET }
| "rec"              { REC }
| "in"               { IN }
| "if"               { IF }
| "then"             { THEN }
| "else"             { ELSE }
| "assert"           { ASSERT }
| ['|']              { PIPE }
| ['_']              { WILDCARD }
| "match"            { MATCH }
| "with"             { WITH }
| "while"            { WHILE }
| "for"              { FOR }
| "do"               { DO }
| "done"             { DONE }
| "begin"            { BEGIN }
| "end"              { END }
| "[|"               { ARRAY_OPEN }
| "|]"               { ARRAY_CLOSE }
| ".("               { ARRAY_ACCESS_OPEN }
| "<-"               { LEFT_ARROW }
| "->"               { RIGHT_ARROW }
| ":="               { ASSIGN }
| "!"                { ACCESS }
| "ref"              { REF }
| (['"']((['a'-'z''A'-'Z''0'-'9'' ''-''_''!'','';''.'''']|['\"''\n''\t'])* as s)['"']) { STRING(s) }
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
| ident_capitalize as lxm { IDENT_CAPITALIZE(lxm) }
| ident as lxm       { IDENT(lxm) }
| vm_ident as lxm    { VM_IDENT(lxm) }
| ['\n' ]            { (Lexing.new_line lexbuf) ; (token lexbuf) }
| [' ' '\t']         { token lexbuf }    (* skip blanks *)
| "(*"               { comment lexbuf }  (* Comment until closing *)
| eof | "eof"              { EOF }
| _  as lxm          { raise (Parse_Exception (Printf.sprintf "Unexpected character: %c"  lxm,  default_position)) }


and comment = parse 
| "*)" { token lexbuf }
| _    { comment lexbuf } 
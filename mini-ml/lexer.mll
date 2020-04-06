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
| (("0x"['0'-'9''a'-'f''A'-'F']+)| ['0'-'9']+) as lxm  { let n = int_of_string lxm in 
	                            if abs n > 0xFFFF 
	                            then failwith "Integer literal exceeds the range of representable integers" 
	                            else INT(if n < 0x8000 then n else - n + 1) }
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
| "::"               { CONS }
| ";;"               { TERMINAISON }
| '.'                { DOT }
| ','                { COMMA }
| "let"              { LET }
| "where"            { WHERE }
| "and"              { AND_KW }
| "fun"              { FUN }
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
| "of"               { OF }
| "while"            { WHILE }
| "for"              { FOR }
| "to"               { TO }
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
| (['"'](([^'"'])* as s)['"']) { STRING(s) }
| (['''](['a'-'z''A'-'Z''0'-'9'' ''-''_''!'','';''.''''] as c)[''']) { CHAR(c) }
| ([''']['a'])       { TVAR }
| "not"              { NOT }
| "+"                { PLUS }
| "-"                { MINUS }
| "*"                { TIMES }
| "/"                { DIV }
| "&&"               { AND }
| "||"               { OR }
| "land"             { LAND }
| "lor"              { LOR }
| "="                { EQ }
| "<>"               { NEQ }
| ">"                { GT }
| "<"                { LT }
| ">="               { GE }
| "<="               { LE }
| "@@"               { ATAT }            
| ident_capitalize as lxm { IDENT_CAPITALIZE(lxm) }
| ident as lxm       { IDENT(lxm) }
| vm_ident as lxm    { VM_IDENT(lxm) }
| ['\n' ]            { (Lexing.new_line lexbuf) ; (token lexbuf) }
| [' ' '\t']         { token lexbuf }    (* skip blanks *)
| "(*"               { comment lexbuf }  (* Comment until closing *)
| eof | "eof"              { EOF }
| _  as lxm          { raise (Parse_Exception (Printf.sprintf "Unexpected character: %c"  lxm,  Parseutils.pos())) }


and comment = parse 
| "*)" { token lexbuf }
| _    { comment lexbuf } 
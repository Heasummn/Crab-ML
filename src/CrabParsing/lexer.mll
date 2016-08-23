{
  open Parser
  open Error
}

(* Values *)
let digit = ['0'-'9']
let int = digit+
let e = ['e' 'E']
let float = int '.' int e? '-'? int?
let letter = ['a'-'z''A'-'Z']
let alpha = letter+ (letter '_')*
let alphanum = alpha+digit*

(* Whitespace *)
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Keywords TODO: Replace these with Hashtbl *)
let def = "def"
let let_stmt = "let"
let in_stmt = "in"

rule read = 
    parse
    | int           { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | float         { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }

    | whitespace    { read lexbuf }
    | newline       { Lexing.new_line lexbuf; read lexbuf }
    | eof           { EOF }

    | '+'           { PLUS }
    | '-'           { MINUS }
    | '*'           { MULT }
    | '/'           { DIV }
    | '('           { LPAREN }
    | ')'           { RPAREN }
    | ';'           { SEMI }
    | '='           { EQUAL }
    | ':'           { COLON }
    | ','           { COMMA }

    | def           { DEF }
    | let_stmt      { LET }
    | in_stmt       { IN }

    | alphanum      { ALPHANUM (Lexing.lexeme lexbuf) }

    | "(*"          { nested_comment 1 lexbuf }
    | "//"          { comment lexbuf }

    | _             { raise (SyntaxError (
            "Unknown identifier(s): " ^ (Lexing.lexeme lexbuf) ^ 
            "\n\tAt line: " ^ (string_of_int (get_line lexbuf)) ^
            ", Columns: " ^ string_of_int (get_start_col lexbuf) ^
            "-" ^ string_of_int(get_end_col lexbuf)))
                    }

and comment =
    parse
    | newline   { Lexing.new_line lexbuf; read lexbuf }
    | _         { comment lexbuf }

and nested_comment level =
    parse
    | "(*"      { nested_comment (level+1) lexbuf }
    | "*)"      { if level = 1 then read lexbuf else nested_comment (level-1) lexbuf }
    | newline   { Lexing.new_line lexbuf; nested_comment level lexbuf }
    | _         { nested_comment level lexbuf }

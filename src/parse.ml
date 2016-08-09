open Lexing

exception ParsingError of string

  let get_line lexbuf = 
    let pos = lexeme_start_p lexbuf in
        pos.pos_lnum

  let get_start_col lexbuf =
    let pos = lexeme_start_p lexbuf in
        pos.pos_cnum - pos.pos_bol + 1

  let get_end_col lexbuf = 
    let pos = lexeme_end_p lexbuf in
        pos.pos_cnum - pos.pos_bol + 1

module I = Parser.MenhirInterpreter

let fail lexbuf _ = 
     raise (ParsingError (
            "Unexpected identifier(s) near: " ^ (lexeme lexbuf) ^ 
            "\n\tAt line: " ^ (string_of_int (get_line lexbuf)) ^
            ", Columns: " ^ string_of_int (get_start_col lexbuf) ^
            "-" ^ string_of_int(get_end_col lexbuf) ^ 
            (if ((get_start_col lexbuf) = 1) then ".\n\tHint: Are you missing a semi colon?"
                else ".")))

let loop lexbuf result =
    let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
        I.loop_handle (fun v -> v) (fail lexbuf) supplier result

let parse lexbuf = loop lexbuf (Parser.Incremental.program lexbuf.lex_curr_p)

let process_string prog = let lexbuf = from_string prog in
    parse lexbuf

let process_chan chan = let lexbuf = from_channel chan in
    parse lexbuf

open Lexing
open Error
open Location

module I = Parser.MenhirInterpreter


let fail lexbuf _  = 
    let loc = from_lex lexbuf in
        raise (ParsingError (
            " identifier(s) near: " ^ (lexeme lexbuf) ^ 
            "\n\tAt line: " ^ string_of_int (loc.s_line) ^
            ", Columns: " ^ string_of_int (loc.s_column) ^
            "-" ^ string_of_int(loc.e_column)))

let loop lexbuf result =
    let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
        I.loop_handle (fun v -> v) (fail lexbuf) supplier result

let parse lexbuf = loop lexbuf (Parser.Incremental.program lexbuf.lex_curr_p)

let process_string prog = let lexbuf = from_string prog in
    parse lexbuf

let process_chan chan = let lexbuf = from_channel chan in
    parse lexbuf

open Lexing

exception LexerError of string * position

let print_error lines = function
  | LexerError (msg, pos) ->
      let line_num = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol in
      let line =
        if line_num - 1 >= 0 && line_num - 1 < Array.length lines then
          lines.(line_num - 1)
        else ""
      in
      Printf.eprintf "Error: %s\n" msg;
      Printf.eprintf " --> line %d, column %d\n" line_num col;
      Printf.eprintf "  %d | %s\n" line_num line;
      Printf.eprintf "    | %s^\n" (String.make col ' ');
      flush stderr
  | _ -> ()

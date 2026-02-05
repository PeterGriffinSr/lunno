open Lexing

type error_code =
  | E_Lex_UnexpectedChar
  | E_Lex_InvalidInt
  | E_Lex_InvalidFloat
  | E_Lex_UnterminatedString
  | E_Lex_InvalidEscape
  | E_Lex_NewlineInString
  | E_Lex_EmptyString

exception LexerError of { code : error_code; msg : string; span : Span.t }

let string_of_code = function
  | E_Lex_UnexpectedChar -> "E1001"
  | E_Lex_InvalidInt -> "E1002"
  | E_Lex_InvalidFloat -> "E1003"
  | E_Lex_UnterminatedString -> "E1004"
  | E_Lex_InvalidEscape -> "E1005"
  | E_Lex_NewlineInString -> "E1006"
  | E_Lex_EmptyString -> "E1007"

let print_error (lines : string array) = function
  | LexerError { code; msg; span = start_pos, _ } ->
      let line_num = start_pos.pos_lnum in
      let col = start_pos.pos_cnum - start_pos.pos_bol + 1 in

      let line =
        if line_num > 0 && line_num <= Array.length lines then
          lines.(line_num - 1)
        else ""
      in

      Printf.eprintf "Error[%s]: %s\n" (string_of_code code) msg;
      Printf.eprintf " --> line %d, column %d\n" line_num col;
      Printf.eprintf "  %d | %s\n" line_num line;
      Printf.eprintf "    | %s^\n" (String.make (col - 1) ' ');
      flush stderr
  | _ -> ()

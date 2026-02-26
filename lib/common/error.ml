open Lexing

type error_code =
  | E_Lex_UnexpectedChar
  | E_Lex_InvalidInt
  | E_Lex_InvalidFloat
  | E_Lex_UnterminatedString
  | E_Lex_InvalidEscape
  | E_Lex_NewlineInString
  | E_Lex_EmptyString
  | E_Parse_UnexpectedToken
  | E_Type_AlreadyDefined
  | E_Type_UndefinedVariable
  | E_Type_TypeMismatch
  | E_Type_ArityMismatch
  | E_Type_MissingAnnotation
  | E_Type_IfBranchMismatch
  | E_Type_MissingElseBranch
  | E_Type_NotAFunction
  | E_Type_MatchBranchMismatch
  | E_Type_PatternTypeMismatch
  | E_Type_MissingMain
  | E_Type_TopLevelExpression

exception LexerError of { code : error_code; msg : string; span : Span.t }
exception TypeError of { code : error_code; msg : string; span : Span.t }
exception ParseError of { code : error_code; msg : string; span : Span.t }

let string_of_code = function
  | E_Lex_UnexpectedChar -> "E1001"
  | E_Lex_InvalidInt -> "E1002"
  | E_Lex_InvalidFloat -> "E1003"
  | E_Lex_UnterminatedString -> "E1004"
  | E_Lex_InvalidEscape -> "E1005"
  | E_Lex_NewlineInString -> "E1006"
  | E_Lex_EmptyString -> "E1007"
  | E_Parse_UnexpectedToken -> "E2001"
  | E_Type_AlreadyDefined -> "E3001"
  | E_Type_UndefinedVariable -> "E3002"
  | E_Type_TypeMismatch -> "E3003"
  | E_Type_ArityMismatch -> "E3004"
  | E_Type_MissingAnnotation -> "E3005"
  | E_Type_IfBranchMismatch -> "E3006"
  | E_Type_MissingElseBranch -> "E3007"
  | E_Type_NotAFunction -> "E3008"
  | E_Type_MatchBranchMismatch -> "E3009"
  | E_Type_PatternTypeMismatch -> "E3010"
  | E_Type_MissingMain -> "E3011"
  | E_Type_TopLevelExpression -> "E3012"

let print_error (lines : string array) = function
  | LexerError { code; msg; span = start_pos, _ }
  | ParseError { code; msg; span = start_pos, _ }
  | TypeError { code; msg; span = start_pos, _ } ->
      let line_num = start_pos.pos_lnum in
      let col = start_pos.pos_cnum - start_pos.pos_bol + 1 in
      let line =
        if line_num > 0 && line_num <= Array.length lines then
          lines.(line_num - 1)
        else ""
      in
      let gutter = String.length (string_of_int line_num) in
      let pad = String.make gutter ' ' in
      Printf.eprintf "Error[%s]: %s\n" (string_of_code code) msg;
      Printf.eprintf " %s--> line %d, column %d\n" pad line_num col;
      Printf.eprintf "  %d | %s\n" line_num line;
      Printf.eprintf "  %s | %s^\n" pad (String.make (col - 1) ' ');
      flush stderr
  | _ -> ()

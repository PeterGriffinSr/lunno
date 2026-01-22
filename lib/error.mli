type error_code =
  | E_Lex_UnexpectedChar
  | E_Lex_InvalidInt
  | E_Lex_UnterminatedString
  | E_Lex_InvalidEscape
  | E_Lex_NewlineInString
  | E_Lex_EmptyString

exception
  LexerError of { code : error_code; msg : string; pos : Lexing.position }

val string_of_code : error_code -> string
val print_error : string array -> exn -> unit

type error_code =
  | E_Lex_UnexpectedChar
      (** Encountered an unexpected character in the input. *)
  | E_Lex_InvalidInt
      (** Invalid integer literal (e.g., too large or malformed). *)
  | E_Lex_InvalidFloat
      (** Invalid floating-point literal (e.g., too large or malformed). *)
  | E_Lex_UnterminatedString  (** String literal was not properly closed. *)
  | E_Lex_InvalidEscape  (** Invalid escape sequence in a string literal. *)
  | E_Lex_NewlineInString  (** Newline encountered inside a string literal. *)
  | E_Lex_EmptyString  (** Empty string literals are not allowed. *)

exception
  LexerError of {
    code : error_code;  (** The kind of lexical error. *)
    msg : string;  (** A human-readable error message. *)
    span : Token.span;  (** Span in the source where the error occurred. *)
  }
(** Exception raised by the lexer on lexical errors. *)

val string_of_code : error_code -> string
(** [string_of_code code] returns a short human-readable string representing the
    lexical error code [code]. *)

val print_error : string array -> exn -> unit
(** [print_error lines exn] prints a formatted error message for a lexical
    error.

    @param lines
      An array of source lines corresponding to the input being lexed.
    @param exn The exception to report (expected to be [LexerError]).

    This function prints the error message along with the line of code and a
    marker indicating the error position. *)

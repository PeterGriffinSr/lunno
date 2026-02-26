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
  | E_Parse_UnexpectedToken
      (** Encountered an unexpected token during parsing. *)
  | E_Type_AlreadyDefined
      (** A name was defined more than once in the same scope. *)
  | E_Type_UndefinedVariable
      (** A variable was referenced before being defined. *)
  | E_Type_TypeMismatch
      (** An expression has a different type than expected. *)
  | E_Type_ArityMismatch
      (** A function was applied to the wrong number of arguments. *)
  | E_Type_MissingAnnotation  (** A required type annotation is absent. *)
  | E_Type_IfBranchMismatch
      (** The then and else branches of an if expression have different types.
      *)
  | E_Type_MissingElseBranch  (** An if expression is missing an else branch. *)
  | E_Type_NotAFunction  (** A non-function type was applied as a function. *)
  | E_Type_MatchBranchMismatch
      (** A match branch has a different type from the first branch. *)
  | E_Type_PatternTypeMismatch
      (** A pattern does not match the type of the scrutinee. *)
  | E_Type_MissingMain  (** The program is missing a main function. *)
  | E_Type_TopLevelExpression  (** Top-level expressions are not allowed. *)

exception
  LexerError of {
    code : error_code;  (** The kind of lexical error. *)
    msg : string;  (** A human-readable error message. *)
    span : Span.t;  (** Span in the source where the error occurred. *)
  }
(** Exception raised by the lexer on lexical errors. *)

exception
  ParseError of {
    code : error_code;  (** The kind of syntax error. *)
    msg : string;  (** A human-readable error message. *)
    span : Span.t;  (** Span in the source where the error occurred. *)
  }
(** Exception raised by the parser on syntax errors. *)

exception
  TypeError of {
    code : error_code;  (** The kind of type error. *)
    msg : string;  (** A human-readable error message. *)
    span : Span.t;  (** Span in the source where the error occurred. *)
  }
(** Exception raised by the typechecker on type errors. *)

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

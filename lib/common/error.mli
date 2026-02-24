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

val already_defined : string -> Span.t -> exn
(** [already_defined name span] returns a {!TypeError} for a name that has
    already been defined in the current scope. *)

val undefined_variable : string -> Span.t -> exn
(** [undefined_variable name span] returns a {!TypeError} for a reference to an
    unbound variable. *)

val type_mismatch : string -> string -> Span.t -> exn
(** [type_mismatch expected got span] returns a {!TypeError} when an expression
    has type [got] but type [expected] was required. *)

val arity_mismatch : int -> int -> Span.t -> exn
(** [arity_mismatch expected got span] returns a {!TypeError} when a function is
    applied to the wrong number of arguments. *)

val missing_annotation : string -> Span.t -> exn
(** [missing_annotation name span] returns a {!TypeError} when a required type
    annotation is absent for [name]. *)

val if_branch_mismatch : string -> string -> Span.t -> exn
(** [if_branch_mismatch then_ty else_ty span] returns a {!TypeError} when the
    then and else branches of an if expression have different types. *)

val missing_else_branch : Span.t -> exn
(** [missing_else_branch span] returns a {!TypeError} when an if expression has
    no else branch. *)

val not_a_function : string -> Span.t -> exn
(** [not_a_function ty span] returns a {!TypeError} when a non-function type is
    applied as a function. *)

val match_branch_mismatch : string -> string -> Span.t -> exn
(** [match_branch_mismatch expected got span] returns a {!TypeError} when a
    match branch has a different type from the first branch. *)

val pattern_type_mismatch : string -> string -> Span.t -> exn
(** [pattern_type_mismatch expected got span] returns a {!TypeError} when a
    pattern does not match the type of the scrutinee. *)

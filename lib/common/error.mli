type error_code =
  | E_Lex_UnexpectedChar
  | E_Lex_InvalidInt  (** Too large or malformed. *)
  | E_Lex_InvalidFloat  (** Too large or malformed. *)
  | E_Lex_UnterminatedString
  | E_Lex_InvalidEscape
  | E_Lex_NewlineInString
  | E_Lex_EmptyString
  | E_Parse_UnexpectedToken
  | E_Type_AlreadyDefined
      (** A name was defined more than once in the same scope. *)
  | E_Type_UndefinedVariable
  | E_Type_TypeMismatch
  | E_Type_ArityMismatch
  | E_Type_MissingAnnotation
  | E_Type_IfBranchMismatch
      (** The then and else branches have different types. *)
  | E_Type_MissingElseBranch
  | E_Type_NotAFunction
  | E_Type_MatchBranchMismatch
      (** A match branch has a different type from the first branch. *)
  | E_Type_PatternTypeMismatch
      (** A pattern does not match the type of the scrutinee. *)
  | E_Type_MissingMain
  | E_Type_TopLevelExpression

type error_kind = LexError | ParseError | TypeError

type compiler_error = {
  kind : error_kind;
  code : error_code;
  msg : string;
  span : Span.t;
}

type 'a result = ('a, compiler_error) Stdlib.Result.t

val lex_error : error_code -> string -> Span.t -> 'a result
val parse_error : error_code -> string -> Span.t -> 'a result
val type_error : error_code -> string -> Span.t -> 'a result

val string_of_code : error_code -> string
(** Returns the short error code string, e.g. ["E1001"]. *)

val string_of_kind : error_kind -> string
(** Returns a human-readable phase name, e.g. ["Lexer"]. *)

val print_error : string array -> compiler_error -> unit
(** Prints a formatted error to stderr, including the relevant source line and a
    caret pointing to the error position. *)

val ( let* ) : 'a result -> ('a -> 'b result) -> 'b result
(** Monadic bind for [result], allowing [let*] syntax to chain fallible compiler
    phases. *)

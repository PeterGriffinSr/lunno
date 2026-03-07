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

type error_kind =
  | LexError  (** Error originated in the lexer. *)
  | ParseError  (** Error originated in the parser. *)
  | TypeError  (** Error originated in the typechecker. *)

type compiler_error = {
  kind : error_kind;
  code : error_code;
  msg : string;
  span : Span.t;
}
(** A structured compiler error carrying its origin, code, message and source
    location. *)

type 'a result = ('a, compiler_error) Stdlib.Result.t
(** [('a, compiler_error) result] is the return type for all compiler phases.
    Use [Ok v] for success and [Error e] for failure. *)

val lex_error : error_code -> string -> Span.t -> 'a result
(** [lex_error code msg span] constructs a lexer [Error]. *)

val parse_error : error_code -> string -> Span.t -> 'a result
(** [parse_error code msg span] constructs a parser [Error]. *)

val type_error : error_code -> string -> Span.t -> 'a result
(** [type_error code msg span] constructs a typechecker [Error]. *)

val string_of_code : error_code -> string
(** [string_of_code code] returns the short error code string, e.g. ["E1001"].
*)

val string_of_kind : error_kind -> string
(** [string_of_kind kind] returns a human-readable phase name, e.g. ["Lexer"].
*)

val print_error : string array -> compiler_error -> unit
(** [print_error lines err] prints a formatted error message to stderr,
    including the relevant source line and a caret pointing to the error
    position.

    @param lines The source file split into lines.
    @param err The compiler error to display. *)

val ( let* ) : 'a result -> ('a -> 'b result) -> 'b result
(** Monadic bind for [result]. Allows [let*] syntax to chain fallible compiler
    phases:
    {[
      let* ast = parse lexbuf in
      let* typed = typecheck ast in
      Ok typed
    ]} *)

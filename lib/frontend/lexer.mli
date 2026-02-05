val string_buffer : Buffer.t
(** A reusable buffer for accumulating characters when parsing string literals.
*)

val reserved : (string, Lunno_common.Span.t -> Parser.token) Hashtbl.t
(** A hashtable of reserved keywords and their corresponding token
    representations. *)

val strip_underscores : string -> string
(** [strip_underscores s] returns a new string with all underscore characters
    ('_') removed from the input string [s].

    This function is useful for processing numeric literals that may include
    underscores for readability (e.g., "1_000_000" becomes "1000000").

    @param s The input string potentially containing underscores.
    @return A new string with all underscores removed. *)

val with_pos : Lexing.lexbuf -> (Lunno_common.Span.t -> 'a) -> 'a
(** [with_pos lexbuf ctor] constructs a token using the provided constructor
    [ctor] and the current lexeme's start and end positions from [lexbuf].

    This function retrieves the start and end positions of the current lexeme in
    the lexing buffer and applies the constructor [ctor] to create a token with
    the associated span.

    @param lexbuf The lexing buffer containing the input source code.
    @param ctor
      A constructor function that takes a [Span.t] and returns a token of type
      ['a].
    @return A token of type ['a] constructed with the current lexeme's span. *)

val token : Lexing.lexbuf -> Parser.token
(** [token lexbuf] reads and returns the next token from the given lexing
    buffer.

    This is the main entry point for the lexer. It handles whitespace, comments,
    punctuation, identifiers, literals (integers, floats, strings), and
    keywords. It advances the lexing cursor as it consumes characters.

    @param lexbuf The lexing buffer containing the input source code.
    @return The next token of type [Parser.token].
    @raise Error.LexerError
      Raised if an invalid character, malformed number, or other lexical error
      is encountered. *)

val read_string : Buffer.t -> Lexing.position -> Lexing.lexbuf -> Parser.token
(** [read_string buffer start_pos lexbuf] parses a string literal starting at
    the current lexing position.

    The function accumulates characters in [buffer] until it reaches a closing
    double-quote. It handles valid escape sequences (e.g., '\n', '\t', '\"') and
    raises errors for invalid escapes, newlines, or unterminated strings.

    @param buffer A [Buffer.t] to accumulate string contents.
    @param start_pos
      The starting position of the string literal in the source, used for
      accurate error reporting.
    @param lexbuf The lexing buffer containing the input source code.
    @return A [Token.token] representing the string literal.
    @raise Error.LexerError
      Raised if the string is empty, contains invalid escape sequences, includes
      a newline, or is unterminated. *)

val read_comment : Lexing.lexbuf -> Parser.token
(** [read_comment lexbuf] skips over a comment starting with '#' until the end
    of the line or EOF.

    The function advances the lexing cursor past the comment and returns the
    next token after the comment.

    @param lexbuf The lexing buffer containing the input source code.
    @return The next token following the comment.
    @raise Error.LexerError
      Raised if an unexpected end-of-file occurs within a comment, although line
      comments usually terminate at the end of line. *)

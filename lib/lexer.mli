val reserved : (string * Token.t) list
(** A list of reserved keywords and their corresponding token representations.
*)

val token : Lexing.lexbuf -> Token.t
(** [token lexbuf] reads and returns the next token from the given lexing
    buffer.

    This is the main entry point for the lexer. It handles whitespace, comments,
    punctuation, identifiers, literals (integers, floats, strings), and
    keywords. It advances the lexing cursor as it consumes characters.

    @param lexbuf The lexing buffer containing the input source code.
    @return The next token of type [Token.token].
    @raise Error.LexerError
      Raised if an invalid character, malformed number, or other lexical error
      is encountered. *)

val read_string : Buffer.t -> Lexing.position -> Lexing.lexbuf -> Token.t
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

val read_comment : Lexing.lexbuf -> Token.t
(** [read_comment lexbuf] skips over a comment starting with '#' until the end
    of the line or EOF.

    The function advances the lexing cursor past the comment and returns the
    next token after the comment.

    @param lexbuf The lexing buffer containing the input source code.
    @return The next token following the comment.
    @raise Error.LexerError
      Raised if an unexpected end-of-file occurs within a comment, although line
      comments usually terminate at the end of line. *)

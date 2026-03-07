exception LexError of Lunno_common.Error.compiler_error
(** Internal exception used to propagate lexical errors through the Menhir
    supplier. Caught and converted to [Error.result] at the CLI boundary. *)

val token : Lexing.lexbuf -> Parser.token
(** [token lexbuf] reads and returns the next token from the given lexing
    buffer.

    This is the main entry point for the lexer. It handles whitespace, comments,
    punctuation, identifiers, literals (integers, floats, strings), and
    keywords. It advances the lexing cursor as it consumes characters.

    @param lexbuf The lexing buffer containing the input source code.
    @return The next token of type [Parser.token].
    @raise LexError
      Raised if an invalid character, malformed number, or other lexical error
      is encountered. *)

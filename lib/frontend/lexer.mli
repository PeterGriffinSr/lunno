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

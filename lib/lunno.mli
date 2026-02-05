module Debug = Lunno_frontend.Debug

val parse_string : string -> Lunno_frontend.Ast.program
(** [parse_string s] parses the input string [s] and returns the resulting AST
    program.

    @param s The input source code as a string.
    @return The AST program parsed from the input string.
    @raise Error.LexerError
      Raised if a lexical error is encountered during tokenization.
    @raise Error.ParserError
      Raised if a syntax error is encountered during parsing. *)

val parse_file : string -> Lunno_frontend.Ast.program
(** [parse_file filename] reads the contents of the file [filename], parses it,
    and returns the resulting AST program.

    @param filename The path to the source code file.
    @return The AST program parsed from the file.
    @raise Error.LexerError
      Raised if a lexical error is encountered during tokenization.
    @raise Error.ParserError
      Raised if a syntax error is encountered during parsing. *)

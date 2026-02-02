val read_file_lines : string -> string array
(** [read_file_lines filename] reads all lines from the file [filename] and
    returns them as a string array. The file is safely closed even if an
    exception occurs. *)

val lex : Lexing.lexbuf -> string array -> Token.t list
(** [lex lexbuf lines] repeatedly calls the lexer on [lexbuf] and returns a list
    of tokens.

    @param lexbuf The lexing buffer to read from.
    @param lines The array of source lines for error reporting.

    Exits the program with code [1] if a [LexerError] is raised, printing a
    formatted error message. *)

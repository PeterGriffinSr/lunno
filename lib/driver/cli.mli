val read_file_lines : string -> string array
(** [read_file_lines filename] reads all lines from the file [filename] and
    returns them as a string array. The file is safely closed even if an
    exception occurs. *)

val parse : Lexing.lexbuf -> string array -> Lunno_common.Ast.program
(** [parse lexbuf lines] parses the input from [lexbuf] and returns the
    resulting AST program.

    @param lexbuf The lexing buffer to read from.
    @param lines
      The array of source lines for error reporting. Exits the program with code
      [1] if a [LexerError] or [ParserError] is raised, printing a formatted
      error message. *)

val typecheck :
  Lunno_common.Ast.program -> string array -> Lunno_common.Typed_ast.program
(** [typecheck program lines] typechecks [program] and returns the resulting
    typed AST program on success.

    @param program The AST program to typecheck.
    @param lines
      The array of source lines for error reporting. Exits the program with code
      [1] if a type error is encountered, printing a formatted error message. *)

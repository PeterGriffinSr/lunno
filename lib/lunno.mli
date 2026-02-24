module Debug = Lunno_frontend.Debug
module Flags = Lunno_driver.Flags

val parse_string : string -> Lunno_common.Ast.program
(** [parse_string s] parses the input string [s] and returns the resulting AST
    program.

    @param s The input source code as a string.
    @return The AST program parsed from the input string.
    @raise Error.LexerError
      Raised if a lexical error is encountered during tokenization.
    @raise Error.ParserError
      Raised if a syntax error is encountered during parsing. *)

val parse_file : string -> string array * Lunno_common.Ast.program
(** [parse_file filename] reads the contents of the file [filename], parses it,
    and returns the resulting AST program.

    @param filename The path to the source code file.
    @return The AST program parsed from the file.
    @raise Error.LexerError
      Raised if a lexical error is encountered during tokenization.
    @raise Error.ParserError
      Raised if a syntax error is encountered during parsing. *)

val typecheck_program : string array -> Lunno_common.Ast.program -> unit
(** [typecheck_program lines program] typechecks [program], printing a type
    error and exiting if one is found.

    @param lines The source lines, used for error reporting.
    @param program The AST program to typecheck.
    @raise Error.TypeError Raised if a type error is encountered. *)

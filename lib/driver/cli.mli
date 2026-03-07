val read_file_lines : string -> string array
(** [read_file_lines filename] reads all lines from [filename] and returns them
    as a string array. The file is safely closed even if an exception occurs. *)

val parse : Lexing.lexbuf -> Lunno_common.Ast.program Lunno_common.Error.result
(** [parse lexbuf] lexes and parses the input from [lexbuf], returning a
    [result] containing either the parsed AST or a [compiler_error].

    Lexer errors are caught at this boundary and converted from the internal
    [Lexer.LexError] exception into [Error e].

    @param lexbuf The lexing buffer to read tokens from.
    @return [Ok program] on success or [Error e] on a lex or parse error. *)

val typecheck :
  Lunno_common.Ast.program ->
  Lunno_common.Typed_ast.program Lunno_common.Error.result
(** [typecheck program] typechecks [program] and returns a [result] containing
    either the typed AST or a [compiler_error].

    @param program The AST program to typecheck.
    @return [Ok typed_program] on success or [Error e] on a type error. *)

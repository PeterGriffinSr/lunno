val dump_tokens : Lexing.lexbuf -> unit
(** [dump_tokens lexbuf] reads and prints all tokens from the given lexing
    buffer until the end of file.

    This function repeatedly calls the lexer to obtain tokens and prints their
    string representations to standard output. It continues until it encounters
    the [EndOfFile] token.

    @param lexbuf The lexing buffer containing the input source code.
    @return Unit. *)

val dump_program : Lunno_common.Ast.program -> unit
(** [dump_program prog] prints the structure of the entire AST program [prog].

    This function iterates over all top-level expressions in the program and
    invokes [dump_expr] to print each one.

    @param prog The AST program to dump.
    @return Unit. *)

val string_of_ty : Lunno_common.Ast.ty -> string
(** [string_of_ty ty] returns a human-readable string representation of the type
    [ty].

    @param ty The AST type to convert.
    @return A string representing the given type. *)

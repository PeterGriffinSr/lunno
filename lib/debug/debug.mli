val dump_program_typed : Lunno_common.Typed_ast.program -> unit
(** [dump_program_typed prog] prints the structure of the entire AST program
    [prog].

    This function iterates over all top-level expressions in the program and
    invokes [dump_expr] to print each one.

    @param prog The AST program to dump.
    @return Unit. *)

val dump_program_untyped : Lunno_common.Ast.program -> unit
(** [dump_program_untyped prog] prints the structure of the entire AST program
    [prog].

    This function iterates over all top-level expressions in the program and
    invokes [dump_expr] to print each one.

    @param prog The AST program to dump.
    @return Unit. *)

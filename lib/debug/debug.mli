val dump_program : Lunno_common.Typed_ast.program -> unit
(** [dump_program prog] prints the structure of the entire AST program [prog].

    This function iterates over all top-level expressions in the program and
    invokes [dump_expr] to print each one.

    @param prog The AST program to dump.
    @return Unit. *)

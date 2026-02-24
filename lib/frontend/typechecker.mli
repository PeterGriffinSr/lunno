val infer_program : Lunno_common.Ast.program -> unit
(** [infer_program program] infers types for the given program, raising on the
    first error encountered.

    @param program The AST program to infer types for.
    @raise Error.TypeError Raised if a type error is encountered. *)

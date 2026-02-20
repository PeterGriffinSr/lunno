val check_program : Ast.program -> unit
(** [check_program program] typechecks the given program, raising on the first
    error encountered.

    @param program The AST program to typecheck.
    @raise Error.TypeError Raised if a type error is encountered. *)

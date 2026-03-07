val infer_program :
  Lunno_common.Ast.program ->
  Lunno_common.Typed_ast.program Lunno_common.Error.result
(** [infer_program program] infers and checks types for [program], returning a
    [result] containing either the fully typed and monomorphized AST or a
    [compiler_error] describing the first type error encountered.

    @param program The AST program to typecheck.
    @return [Ok typed_program] on success or [Error e] on a type error. *)

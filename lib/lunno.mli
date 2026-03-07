module Debug = Lunno_debug.Debug
module Flags = Lunno_driver.Flags

val parse_file :
  string -> string array * Lunno_common.Ast.program Lunno_common.Error.result
(** [parse_file filename] reads the contents of [filename], lexes and parses it,
    and returns the source lines alongside a [result] containing either the
    parsed AST or a [compiler_error].

    @param filename The path to the source file.
    @return
      A pair [(lines, result)] where [lines] is the raw source split by line,
      and [result] is [Ok program] on success or [Error e] on a lex/parse error.
*)

val typecheck_program :
  'a ->
  Lunno_common.Ast.program ->
  Lunno_common.Typed_ast.program Lunno_common.Error.result
(** [typecheck_program _lines program] typechecks [program] and returns a
    [result] containing either the typed AST or a [compiler_error].

    @param _lines Unused; retained for call-site compatibility.
    @param program The AST program to typecheck.
    @return [Ok typed_program] on success or [Error e] on a type error. *)

val run_lsp : unit -> unit
(** [run_lsp ()] starts the Lunno LSP server, reading JSON-RPC messages from
    stdin and writing responses to stdout. Does not return. *)

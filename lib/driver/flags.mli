(** Controls which AST representation to dump, if any. *)
type dump_mode =
  | DumpUntyped  (** Print the untyped AST and exit. *)
  | DumpTyped  (** Print the typed AST and exit. *)

(** A named subcommand. *)
type subcommand = Lsp  (** Run the LSP server. *)

type t = {
  subcommand : subcommand option;
      (** The subcommand to run, or [None] for normal compilation. *)
  dump_mode : dump_mode option;
      (** Which AST dump to perform before exiting, or [None] for normal
          compilation. Ignored when [subcommand] is set. *)
  files : string list;
      (** The list of input files to process. Empty when [subcommand] is set. *)
}
(** The parsed CLI arguments. *)

val parse : string array -> t
(** [parse argv] parses the command line via Cmdliner and returns a [t].

    Subcommands: [lsp], [dump-untyped], [dump-typed]. Running with no subcommand
    compiles the given files. [--help] and [--version] are handled automatically
    by Cmdliner. Exits with code [1] on error.

    @param argv The argument vector, typically [Sys.argv]. *)

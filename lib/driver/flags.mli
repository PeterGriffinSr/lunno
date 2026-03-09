type dump_mode = DumpUntyped | DumpTyped
type subcommand = Lsp

type t = {
  subcommand : subcommand option;
  dump_mode : dump_mode option;  (** Ignored when [subcommand] is set. *)
  files : string list;  (** Empty when [subcommand] is set. *)
}
(** The parsed CLI arguments. *)

val parse : string array -> t
(** Parses [argv] via Cmdliner. Subcommands: [lsp], [dump-untyped],
    [dump-typed]. [--help] and [--version] are handled automatically. Exits with
    code [1] on error. *)

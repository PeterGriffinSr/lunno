type t = {
  dump_program : bool;  (** Whether to print the AST and exit. *)
  files : string list;
      (** The list of input files to process. Should contain exactly one file.
      *)
}
(** The parsed CLI flags and positional arguments. *)

val parse : string array -> t
(** [parse argv] parses [argv] (i.e. [Sys.argv]) into a [t].

    Handles [--help]/[-help] and [--version]/[-v] directly, printing to stdout
    and exiting. Exits with code [1] if an unknown flag is given or no filename
    is provided.

    @param argv The argument vector, typically [Sys.argv].
    @return A [t] with all flags and the input filename populated. *)

type t
(** The type environment, mapping names to their types. *)

val empty : t
(** The empty environment. *)

val mem : string -> t -> bool
(** [mem name env] returns [true] if [name] is bound in [env]. *)

val add : string -> Ast.ty -> t -> t
(** [add name ty env] returns a new environment with [name] bound to [ty]. *)

val find : string -> t -> Ast.ty option
(** [find name env] returns [Some ty] if [name] is bound in [env], or [None]. *)

val fold : (string -> Ast.ty -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f env acc] folds over the environment, applying [f] to each binding.

    @param f
      The function to apply to each binding, taking the name, type, and
      accumulator.
    @param env The environment to fold over.
    @param acc The initial accumulator value. *)

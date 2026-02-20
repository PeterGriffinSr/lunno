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

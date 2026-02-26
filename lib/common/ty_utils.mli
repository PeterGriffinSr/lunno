val string_of_binop : Ast.binary_op -> string
(** [string_of_binop op] converts a binary operator to its symbol, e.g. [OpAdd]
    becomes ["+"], [OpCons] becomes ["::"]. *)

val resolve : Ast.ty -> Ast.ty
(** [resolve ty] chases [TyMeta] indirections until it reaches either a concrete
    type or an unbound meta variable. *)

val generalize_display : Ast.ty -> Ast.ty
(** [generalize_display ty] replaces any unbound meta variables in [ty] with
    fresh type variable names ['a], ['b], etc. for display purposes. Used by the
    LSP to show polymorphic types like ['a -> 'a] instead of [?3 -> ?3]. *)

val string_of_ty : Ast.ty -> string
(** [string_of_ty ty] converts a type to its string representation. Meta
    variables that are unresolved are displayed as [?id]. *)

val string_of_unop : Ast.unary_op -> string
(** [string_of_unop op] converts a unary operator to its symbol, e.g. [OpNegate]
    becomes ["-"]. *)

val string_of_span : Span.t -> string
(** [string_of_span span] converts a source location span to a human-readable
    string of the form [[line:col-line:col]]. *)

type ty = Ast.ty
(** All types are fully resolved, no unfilled [TyMeta] nodes remain after
    typechecking. *)

type literal = Ast.literal
type import = Ast.import

type param = { param_name : string; param_ty : ty; param_span : Span.t }
(** Unlike [Ast.param], [param_ty] is non-optional since the typechecker fills
    in any unknowns. *)

type expr =
  | Literal of literal * ty * Span.t
  | Variable of string * ty * Span.t
      (** The [ty] may be instantiated from a polymorphic type. *)
  | Lambda of lambda
  | Apply of expr * expr list * ty * Span.t
      (** The [ty] is the resolved return type. *)
  | Let of let_expr
  | If of if_expr
  | Match of match_expr
  | Block of expr list * ty * Span.t
      (** The [ty] is the type of the last expression, or [TyUnit] if empty. *)
  | Binary of binary_expr
  | Unary of unary_expr
  | MemberAccess of expr * string * ty * Span.t
  | Range of expr * expr * Span.t
      (** Both bounds must be [int]; result is always [TyList TyInt]. *)
  | Constructor of string * expr list * ty * Span.t
      (** e.g., [Some(x)] or [Ok(val)]. The [ty] is the resolved constructed
          type. *)

and lambda = {
  params : param list;
  ret_ty : ty;
  lambda_body : expr;
  is_recursive : bool;
  lambda_ty : ty;  (** The full [TyFunction] type of this lambda. *)
  lambda_span : Span.t;
}

and let_expr = {
  name : string;
  let_ty : ty;
  let_body : expr;
  let_span : Span.t;
}

and if_expr = {
  cond : expr;  (** Must have type [TyBool]. *)
  then_ : expr;
  else_ : expr option;  (** Must have the same type as [then_] when present. *)
  if_ty : ty;
  if_span : Span.t;
}

and match_expr = {
  scrutinee : expr;
  cases : match_case list;  (** All cases must have the same body type. *)
  match_ty : ty;
  match_span : Span.t;
}

and match_case = {
  pattern : Ast.pattern;
      (** Reused from [Ast] since patterns carry no type annotations. *)
  guard : expr option;  (** Must have type [TyBool] when present. *)
  case_body : expr;
  case_span : Span.t;
}

and binary_expr = {
  binary_op : Ast.binary_op;
  left : expr;
  right : expr;
  binary_ty : ty;
  binary_span : Span.t;
}

and unary_expr = {
  unary_op : Ast.unary_op;
  expr : expr;
  unary_ty : ty;
  unary_span : Span.t;
}

val ty_of : expr -> ty
(** For [Range], always returns [TyList TyInt]. *)

val span_of : expr -> Span.t

type program = {
  imports : import list;
  type_decls : Ast.type_decl list;
  body : expr list;
}

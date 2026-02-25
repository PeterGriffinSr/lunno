(** Typed AST produced by the typechecker. Every node carries a resolved [ty],
    with no unresolved meta variables remaining after inference. *)

type ty = Ast.ty
(** Alias for [Ast.ty]. All types here are fully resolved — no [TyMeta] nodes
    with unfilled [contents] should appear after typechecking. *)

type literal = Ast.literal
(** Alias for [Ast.literal]. Literals are unchanged from the parse tree. *)

type param = { param_name : string; param_ty : ty; param_span : Span.t }
(** A function parameter with a fully resolved type. Unlike [Ast.param], the
    type is non-optional since the typechecker fills in any unknowns. *)

type import = Ast.import
(** Alias for [Ast.import]. Imports are unchanged from the parse tree. *)

type expr =
  | Literal of literal * ty * Span.t
      (** A literal value with its resolved type. *)
  | Variable of string * ty * Span.t
      (** A variable reference with its resolved and possibly instantiated type.
      *)
  | Lambda of lambda
      (** An anonymous function with fully resolved parameter and return types.
      *)
  | Apply of expr * expr list * ty * Span.t
      (** A function application. The [ty] is the resolved return type. *)
  | Let of let_expr  (** A let binding with its resolved type. *)
  | If of if_expr
      (** A conditional expression. Both branches must have the same type. *)
  | Match of match_expr
      (** A pattern match expression. All branches must have the same type. *)
  | Block of expr list * ty * Span.t
      (** A sequence of expressions. The [ty] is the type of the last
          expression, or [TyUnit] if the block is empty. *)
  | Binary of binary_expr
      (** A binary operation with its resolved result type. *)
  | Unary of unary_expr  (** A unary operation with its resolved result type. *)
  | MemberAccess of expr * string * ty * Span.t
      (** Member access on a module value, e.g. [io.println]. The [ty] is the
          type of the accessed member. *)
  | Range of expr * expr * Span.t
      (** A range expression [a..b]. Both bounds must be [int] and the result is
          always [TyList TyInt]. *)

and lambda = {
  params : param list;
      (** The parameters of the lambda, each with a resolved type. *)
  ret_ty : ty;  (** The resolved return type of the lambda body. *)
  lambda_body : expr;  (** The body of the lambda. *)
  is_recursive : bool;
      (** Whether the lambda refers to itself by name, enabling recursion. *)
  lambda_ty : ty;  (** The full [TyFunction] type of this lambda. *)
  lambda_span : Span.t;
}

and let_expr = {
  name : string;  (** The name being bound. *)
  let_ty : ty;  (** The resolved type of the bound expression. *)
  let_body : expr;  (** The expression being bound. *)
  let_span : Span.t;
}

and if_expr = {
  cond : expr;  (** The condition, which must have type [TyBool]. *)
  then_ : expr;  (** The then branch. *)
  else_ : expr option;
      (** The else branch. Must have the same type as [then_] when present. *)
  if_ty : ty;
      (** The resolved type of the if expression, equal to the type of both
          branches. *)
  if_span : Span.t;
}

and match_expr = {
  scrutinee : expr;  (** The expression being matched on. *)
  cases : match_case list;
      (** The list of match cases, all of which must have the same body type. *)
  match_ty : ty;
      (** The resolved type of the match expression, equal to the type of all
          case bodies. *)
  match_span : Span.t;
}

and match_case = {
  pattern : Ast.pattern;
      (** The pattern to match against the scrutinee. Reused from [Ast] since
          patterns are structural and carry no type annotations. *)
  guard : expr option;
      (** An optional guard expression that must have type [TyBool]. *)
  case_body : expr;
      (** The body expression evaluated when this case matches. *)
  case_span : Span.t;
}

and binary_expr = {
  binary_op : Ast.binary_op;  (** The binary operator. *)
  left : expr;  (** The left operand. *)
  right : expr;  (** The right operand. *)
  binary_ty : ty;  (** The resolved result type of the operation. *)
  binary_span : Span.t;
}

and unary_expr = {
  unary_op : Ast.unary_op;  (** The unary operator. *)
  expr : expr;  (** The operand. *)
  unary_ty : ty;  (** The resolved result type of the operation. *)
  unary_span : Span.t;
}

val ty_of : expr -> ty
(** Returns the resolved type of an expression. For [Range], always returns
    [TyList TyInt]. *)

val span_of : expr -> Span.t
(** Returns the source span of an expression. *)

type program = { imports : import list; body : expr list }
(** A fully typed program, produced by the typechecker from an [Ast.program].
    All expressions in [body] carry resolved types with no remaining meta
    variables. *)

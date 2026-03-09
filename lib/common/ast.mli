type family =
  | FInt  (** Covers [int], [i8], [i16], [i32], and [i64]. *)
  | FFloat  (** Covers [float], [f32], and [f64]. *)

type ty =
  | TyInt
  | TyI8
  | TyI16
  | TyI32
  | TyI64
  | TyFloat
  | TyF32
  | TyF64
  | TyString
  | TyBool
  | TyUnit
  | TyVar of string
  | TyBoundVar of string * family
      (** Bound type variable constrained to a specific family. *)
  | TyMeta of meta_var
  | TyFamilyMeta of family_meta
  | TyList of ty
  | TyFunction of ty list * ty
  | TyModule of string * string
  | TyAdt of string

and meta_var = { id : int; contents : ty option ref }
and family_meta = { fid : int; family : family; fcontents : ty option ref }

type literal =
  | LInt of int64
  | LFloat of float
  | LString of string
  | LBool of bool
  | LUnit
  | LNil  (** Empty list. *)

type param = { param_name : string; param_ty : ty option; param_span : Span.t }

type variant = {
  variant_name : string;
  variant_fields : ty list;
  variant_span : Span.t;
}

type type_decl = {
  type_name : string;
  variants : variant list;
  type_span : Span.t;
}

type import = { module_ : string; item : string; import_span : Span.t }

type expr =
  | Literal of literal * Span.t
  | Variable of string * Span.t
  | Lambda of lambda
  | Apply of expr * expr list * Span.t
  | Let of let_expr
  | If of if_expr
  | Match of match_expr
  | Block of expr list * Span.t
  | Binary of binary_expr
  | Unary of unary_expr
  | MemberAccess of expr * string * Span.t
  | Range of expr * expr * Span.t
  | Constructor of string * expr list * Span.t
      (** ADT constructor application, e.g., [Some(x)] or [Ok(val)]. *)

and lambda = {
  params : param list;
  ret_ty : ty option;
  lambda_body : expr;
  is_recursive : bool;
  lambda_span : Span.t;
}

and let_expr = {
  name : string;
  ty : ty option;
  let_body : expr;
  let_span : Span.t;
}

and if_expr = {
  cond : expr;
  then_ : expr;
  else_ : expr option;
  if_span : Span.t;
}

and match_expr = {
  scrutinee : expr;
  cases : match_case list;
  match_span : Span.t;
}

and match_case = {
  pattern : pattern;
  guard : expr option;
  case_body : expr;
  case_span : Span.t;
}

and pattern =
  | PWildcard of Span.t
  | PVariable of string * Span.t
  | PIntLiteral of int64 * Span.t
  | PFloatLiteral of float * Span.t
  | PStringLiteral of string * Span.t
  | PBooleanLiteral of bool * Span.t
  | PNil of Span.t
  | PCons of pattern * pattern * Span.t  (** [head :: tail]. *)
  | PConstructor of string * pattern list * Span.t
      (** e.g., [Some(x)] or [Ok(v, msg)]. *)

and binary_op =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpEqual
  | OpNotEqual
  | OpLess
  | OpGreater
  | OpCons

and unary_op = OpNegate

and binary_expr = {
  binary_op : binary_op;
  left : expr;
  right : expr;
  binary_span : Span.t;
}

and unary_expr = { unary_op : unary_op; expr : expr; unary_span : Span.t }

val span_of_expr : expr -> Span.t

type program = {
  imports : import list;
  type_decls : type_decl list;
  body : expr list;
}

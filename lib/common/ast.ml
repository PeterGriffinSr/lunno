type ty =
  | TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyUnit
  | TyVar of string
  | TyMeta of meta_var
  | TyList of ty
  | TyFunction of ty list * ty
  | TyModule of string * string

and meta_var = { id : int; contents : ty option ref }

type literal =
  | LInt of int64
  | LFloat of float
  | LString of string
  | LBool of bool
  | LUnit
  | LNil

type param = { param_name : string; param_ty : ty option; param_span : Span.t }
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
  | PCons of pattern * pattern * Span.t

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

let span_of_expr = function
  | Literal (_, s)
  | Variable (_, s)
  | Apply (_, _, s)
  | Block (_, s)
  | Binary { binary_span = s; _ }
  | Unary { unary_span = s; _ } ->
      s
  | Lambda { lambda_span = s; _ } -> s
  | Let { let_span = s; _ } -> s
  | If { if_span = s; _ } -> s
  | Match { match_span = s; _ } -> s
  | MemberAccess (_, _, s) -> s
  | Range (_, _, s) -> s

type program = { imports : import list; body : expr list }

type ty = Ast.ty
type literal = Ast.literal
type param = { param_name : string; param_ty : ty; param_span : Span.t }
type import = Ast.import

type expr =
  | Literal of literal * ty * Span.t
  | Variable of string * ty * Span.t
  | Lambda of lambda
  | Apply of expr * expr list * ty * Span.t
  | Let of let_expr
  | If of if_expr
  | Match of match_expr
  | Block of expr list * ty * Span.t
  | Binary of binary_expr
  | Unary of unary_expr
  | MemberAccess of expr * string * ty * Span.t
  | Range of expr * expr * Span.t

and lambda = {
  params : param list;
  ret_ty : ty;
  lambda_body : expr;
  is_recursive : bool;
  lambda_ty : ty;
  lambda_span : Span.t;
}

and let_expr = {
  name : string;
  let_ty : ty;
  let_body : expr;
  let_span : Span.t;
}

and if_expr = {
  cond : expr;
  then_ : expr;
  else_ : expr option;
  if_ty : ty;
  if_span : Span.t;
}

and match_expr = {
  scrutinee : expr;
  cases : match_case list;
  match_ty : ty;
  match_span : Span.t;
}

and match_case = {
  pattern : Ast.pattern;
  guard : expr option;
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

let ty_of = function
  | Literal (_, ty, _) -> ty
  | Variable (_, ty, _) -> ty
  | Lambda { lambda_ty; _ } -> lambda_ty
  | Apply (_, _, ty, _) -> ty
  | Let { let_ty; _ } -> let_ty
  | If { if_ty; _ } -> if_ty
  | Match { match_ty; _ } -> match_ty
  | Block (_, ty, _) -> ty
  | Binary { binary_ty; _ } -> binary_ty
  | Unary { unary_ty; _ } -> unary_ty
  | MemberAccess (_, _, ty, _) -> ty
  | Range _ -> Ast.TyList Ast.TyInt

let span_of = function
  | Literal (_, _, s) -> s
  | Variable (_, _, s) -> s
  | Lambda { lambda_span = s; _ } -> s
  | Apply (_, _, _, s) -> s
  | Let { let_span = s; _ } -> s
  | If { if_span = s; _ } -> s
  | Match { match_span = s; _ } -> s
  | Block (_, _, s) -> s
  | Binary { binary_span = s; _ } -> s
  | Unary { unary_span = s; _ } -> s
  | MemberAccess (_, _, _, s) -> s
  | Range (_, _, s) -> s

type program = { imports : import list; body : expr list }

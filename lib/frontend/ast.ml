open Lunno_common

type ty =
  | TyInt
  | TyFloat
  | TyString
  | TyBool
  | TyUnit
  | TyVar of string
  | TyList of ty
  | TyFunction of ty list * ty

type literal =
  | LInt of int64
  | LFloat of float
  | LString of string
  | LBool of bool
  | LUnit

type param = { name : string; ty : ty option; span : Span.t }

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

and lambda = {
  params : param list;
  ret_ty : ty option;
  body : expr;
  is_recursive : bool;
  span : Span.t;
}

and let_expr = { name : string; ty : ty option; body : expr; span : Span.t }
and if_expr = { cond : expr; then_ : expr; else_ : expr option; span : Span.t }
and match_expr = { scrutinee : expr; cases : match_case list; span : Span.t }

and match_case = {
  pattern : pattern;
  guard : expr option;
  body : expr;
  span : Span.t;
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
and binary_expr = { op : binary_op; left : expr; right : expr; span : Span.t }
and unary_expr = { op : unary_op; expr : expr; span : Span.t }

type program = expr list

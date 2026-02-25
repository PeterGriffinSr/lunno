(** The [ty] type represents types in the language. *)
type ty =
  | TyInt  (** Integer type. *)
  | TyFloat  (** Floating-point type. *)
  | TyString  (** String type. *)
  | TyBool  (** Boolean type. *)
  | TyUnit  (** Unit type. *)
  | TyVar of string  (** Type variable, e.g., 'a. *)
  | TyMeta of meta_var  (** Meta type variable used during type inference. *)
  | TyList of ty  (** List type, e.g., [int]. *)
  | TyFunction of ty list * ty  (** Function type, e.g., (int, int) -> int. *)
  | TyModule of string * string  (** Module type, e.g., std:io. *)

and meta_var = { id : int; contents : ty option ref }
(** The [meta_var] type represents a meta type variable used during type
    inference. *)

(** The [literal] type represents literal values in the language. *)
type literal =
  | LInt of int64  (** Integer literal. *)
  | LFloat of float  (** Floating-point literal. *)
  | LString of string  (** String literal. *)
  | LBool of bool  (** Boolean literal. *)
  | LUnit  (** Unit literal. *)
  | LNil  (** Nil literal for empty lists. *)

type param = {
  param_name : string;  (** Parameter name. *)
  param_ty : ty option;  (** Optional type annotation. *)
  param_span : Span.t;  (** Source code span of the parameter. *)
}
(** The [param] type represents a function parameter. *)

type import = {
  module_ : string;  (** The name of the module being imported. *)
  item : string;  (** The name of the item being imported from the module. *)
  import_span : Span.t;  (** The source code span of the import expression. *)
}
(** The [import] type represents an import expression. *)

(** The [expr] type represents all expressions in the language. *)
type expr =
  | Literal of literal * Span.t  (** Literal reference.*)
  | Variable of string * Span.t  (** Variable reference. *)
  | Lambda of lambda  (** Lambda / anonymous function. *)
  | Apply of expr * expr list * Span.t  (** Function application. *)
  | Let of let_expr  (** Let binding expression. *)
  | If of if_expr  (** If expression. *)
  | Match of match_expr  (** Match expression. *)
  | Block of expr list * Span.t  (** Block expression. *)
  | Binary of binary_expr  (** Binary operation. *)
  | Unary of unary_expr  (** Unary operation. *)
  | MemberAccess of expr * string * Span.t
      (** Member access, e.g., obj.field. *)
  | Range of expr * expr * Span.t  (** Range expression, e.g., [1..10]. *)

and lambda = {
  params : param list;  (** Parameters. *)
  ret_ty : ty option;  (** Optional return type annotation. *)
  lambda_body : expr;  (** Function body. *)
  is_recursive : bool;
  lambda_span : Span.t;  (** Span of the whole lambda. *)
}
(** The [lambda] type represents anonymous functions. *)

and let_expr = {
  name : string;  (** Name being bound. *)
  ty : ty option;  (** Optional type annotation. *)
  let_body : expr;  (** Expression being bound. *)
  let_span : Span.t;  (** Span of the let expression. *)
}
(** The [let_expr] type represents let bindings. *)

and if_expr = {
  cond : expr;  (** Condition expression. *)
  then_ : expr;  (** Then branch. *)
  else_ : expr option;  (** Else branch. *)
  if_span : Span.t;  (** Span of the if expression. *)
}
(** The [if_expr] type represents if expressions. *)

and match_expr = {
  scrutinee : expr;  (** Expression being matched on. *)
  cases : match_case list;  (** List of match cases. *)
  match_span : Span.t;  (** Span of the match expression. *)
}
(** The [match_expr] type represents match expressions. *)

and match_case = {
  pattern : pattern;  (** Pattern to match. *)
  guard : expr option;  (** Optional guard expression. *)
  case_body : expr;  (** Expression for the case body. *)
  case_span : Span.t;  (** Span of the case. *)
}
(** The [match_case] type represents a single match case. *)

(** The [pattern] type represents patterns in match expressions. *)
and pattern =
  | PWildcard of Span.t  (** Wildcard pattern [_]. *)
  | PVariable of string * Span.t  (** Variable pattern. *)
  | PIntLiteral of int64 * Span.t  (** Integer literal pattern. *)
  | PFloatLiteral of float * Span.t  (** Float literal pattern. *)
  | PStringLiteral of string * Span.t  (** String literal pattern. *)
  | PBooleanLiteral of bool * Span.t  (** Boolean literal pattern. *)
  | PNil of Span.t  (** Empty list pattern []. *)
  | PCons of pattern * pattern * Span.t  (** Cons pattern (head :: tail). *)

(** The [binary_op] type represents binary operators. *)
and binary_op =
  | OpAdd  (** + operator. *)
  | OpSub  (** - operator. *)
  | OpMul  (** * operator. *)
  | OpDiv  (** / operator. *)
  | OpEqual  (** = operator. *)
  | OpNotEqual  (** <> operator. *)
  | OpLess  (** < operator. *)
  | OpGreater  (** > operator. *)
  | OpCons  (** :: operator. *)

(** The [unary_op] type represents unary operators. *)
and unary_op = OpNegate  (** Unary minus operator. *)

and binary_expr = {
  binary_op : binary_op;  (** Operator. *)
  left : expr;  (** Left-hand side expression. *)
  right : expr;  (** Right-hand side expression. *)
  binary_span : Span.t;  (** Span of the binary expression. *)
}
(** The [binary_expr] type represents binary operations in expressions. *)

and unary_expr = {
  unary_op : unary_op;  (** Operator. *)
  expr : expr;  (** Expression operand. *)
  unary_span : Span.t;  (** Span of the unary expression. *)
}
(** The [unary_expr] type represents unary operations in expressions. *)

val span_of_expr : expr -> Span.t
(** [span_of_expr e] returns the source span of expression [e]. *)

type program = { imports : import list; body : expr list }
(** The [program] type represents a sequence of expressions (the top-level
    program). *)

open Lunno_common

type ty = Lunno.ty
type pattern = Lunno.pattern
type param = Lunno.param

type import = {
  import_module : string;
  import_item : string;
  import_span : Span.span;
}

type variant = { variant_name : string; variant_fields : ty list }
type type_decl = { type_name : string; variants : variant list }

type expr =
  | Lit of Lunno.literal * Span.span
  | Var of string * Span.span
  | Lam of param list * ty option * expr * bool * Span.span
  | App of expr * expr list * Span.span
  | LetE of string * ty option * expr * Span.span
  | IfE of expr * expr * expr option * Span.span
  | MatchE of expr * match_case list * Span.span
  | Block of expr list * Span.span
  | BinOp of Lunno.binary_op * expr * expr * Span.span
  | UnOp of Lunno.unary_op * expr * Span.span
  | MemberAccess of expr * string * Span.span
  | Range of expr * expr * Span.span
  | Constructor of string * expr list * Span.span

and match_case = {
  pat : pattern;
  guard : expr option;
  case_body : expr;
  case_span : Span.span;
}

type program = {
  imports : import list;
  type_decls : type_decl list;
  body : expr list;
}

val span_of : expr -> Span.span

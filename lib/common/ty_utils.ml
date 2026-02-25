let rec string_of_ty = function
  | Ast.TyInt -> "int"
  | Ast.TyFloat -> "float"
  | Ast.TyString -> "string"
  | Ast.TyBool -> "bool"
  | Ast.TyUnit -> "unit"
  | Ast.TyVar v -> v
  | Ast.TyMeta mv -> (
      match !(mv.Ast.contents) with
      | None -> Printf.sprintf "?%d" mv.Ast.id
      | Some t -> string_of_ty t)
  | Ast.TyModule (namespace, name) -> namespace ^ ":" ^ name
  | Ast.TyList t -> Printf.sprintf "List[%s]" (string_of_ty t)
  | Ast.TyFunction (args, ret) -> (
      match args with
      | [] -> Printf.sprintf "Fun(%s)" (string_of_ty ret)
      | _ ->
          Printf.sprintf "Fun(%s -> %s)"
            (String.concat ", " (List.map string_of_ty args))
            (string_of_ty ret))

let string_of_binop = function
  | Ast.OpAdd -> "+"
  | Ast.OpSub -> "-"
  | Ast.OpMul -> "*"
  | Ast.OpDiv -> "/"
  | Ast.OpEqual -> "=="
  | Ast.OpNotEqual -> "!="
  | Ast.OpLess -> "<"
  | Ast.OpGreater -> ">"
  | Ast.OpCons -> "::"

let string_of_unop = function Ast.OpNegate -> "-"

let string_of_span (start_pos, end_pos) =
  Printf.sprintf "[%d:%d-%d:%d]" start_pos.Lexing.pos_lnum
    (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
    end_pos.Lexing.pos_lnum
    (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol)

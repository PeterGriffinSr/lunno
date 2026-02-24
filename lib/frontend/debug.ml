[@@@warning "-45"]

open Lunno_common.Ast

let rec dump_tokens lexbuf =
  let token = Lexer.token lexbuf in
  Printf.printf "%s\n" (Token.to_string token);
  match token with
  | Parser.EndOfFile _ -> ()
  | Parser.Integer _ | Parser.FloatingPoint _ | Parser.Identifier _
  | Parser.String _ | Parser.Boolean _ | Parser.Unit _ | Parser.KwLet _
  | Parser.KwIf _ | Parser.KwThen _ | Parser.KwElse _ | Parser.KwMatch _
  | Parser.KwImport _ | Parser.IntegerType _ | Parser.FloatingPointType _
  | Parser.StringType _ | Parser.BooleanType _ | Parser.UnitType _
  | Parser.LeftParen _ | Parser.RightParen _ | Parser.LeftBrace _
  | Parser.RightBrace _ | Parser.LeftBracket _ | Parser.RightBracket _
  | Parser.Plus _ | Parser.Minus _ | Parser.Asterisk _ | Parser.Slash _
  | Parser.Equal _ | Parser.NotEqual _ | Parser.Less _ | Parser.Greater _
  | Parser.Comma _ | Parser.Colon _ | Parser.Pipe _ | Parser.Cons _
  | Parser.Arrow _ | Parser.Underscore _ | Parser.Dot _ ->
      dump_tokens lexbuf

let string_of_binop = function
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"
  | OpEqual -> "=="
  | OpNotEqual -> "!="
  | OpLess -> "<"
  | OpGreater -> ">"
  | OpCons -> "::"

let string_of_unop = function OpNegate -> "-"

let string_of_span (start_pos, end_pos) =
  Printf.sprintf "[%d:%d-%d:%d]" start_pos.Lexing.pos_lnum
    (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
    end_pos.Lexing.pos_lnum
    (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol)

let rec dump_expr ?(indent = 0) e =
  let pad = String.make indent ' ' in
  match e with
  | Literal (lit, sp) -> (
      match lit with
      | LInt i -> Printf.printf "%sInt(%Ld) %s\n" pad i (string_of_span sp)
      | LFloat f -> Printf.printf "%sFloat(%g) %s\n" pad f (string_of_span sp)
      | LString s -> Printf.printf "%sString(%S) %s\n" pad s (string_of_span sp)
      | LBool b -> Printf.printf "%sBool(%b) %s\n" pad b (string_of_span sp)
      | LUnit -> Printf.printf "%sUnit %s\n" pad (string_of_span sp)
      | LNil -> Printf.printf "%sNil %s\n" pad (string_of_span sp))
  | Variable (name, sp) ->
      Printf.printf "%sVar(%s) %s\n" pad name (string_of_span sp)
  | Binary { binary_op; left; right; binary_span } ->
      Printf.printf "%sBinary(%s) %s\n" pad
        (string_of_binop binary_op)
        (string_of_span binary_span);
      dump_expr ~indent:(indent + 2) left;
      dump_expr ~indent:(indent + 2) right
  | Unary { unary_op; expr; unary_span } ->
      Printf.printf "%sUnary(%s) %s\n" pad (string_of_unop unary_op)
        (string_of_span unary_span);
      dump_expr ~indent:(indent + 2) expr
  | Let { name; ty; let_body; let_span } ->
      let ty_str = get_let_type_string ty let_body in
      Printf.printf "%sLet(%s : %s) %s\n" pad name ty_str
        (string_of_span let_span);
      dump_expr ~indent:(indent + 2) let_body
  | Lambda { params; ret_ty; lambda_body; is_recursive; lambda_span } ->
      let params_str =
        String.concat ", "
          (List.map
             (fun (p : param) ->
               match p.param_ty with
               | None -> Printf.sprintf "%s: ?" p.param_name
               | Some t -> Printf.sprintf "%s: %s" p.param_name (string_of_ty t))
             params)
      in
      let ret_str =
        match ret_ty with None -> "?" | Some t -> string_of_ty t
      in
      let rec_str = if is_recursive then " [recursive]" else "" in
      (match (params, ret_ty) with
      | [], None ->
          Printf.printf "%sLambda%s %s\n" pad rec_str
            (string_of_span lambda_span)
      | [], Some _ ->
          Printf.printf "%sLambda(-> %s)%s %s\n" pad ret_str rec_str
            (string_of_span lambda_span)
      | _, _ ->
          Printf.printf "%sLambda(%s -> %s)%s %s\n" pad params_str ret_str
            rec_str
            (string_of_span lambda_span));
      dump_expr ~indent:(indent + 2) lambda_body
  | If { cond; then_; else_; if_span } -> (
      Printf.printf "%sIf %s\n" pad (string_of_span if_span);
      dump_expr ~indent:(indent + 2) cond;
      Printf.printf "%sThen:\n" (String.make (indent + 2) ' ');
      dump_expr ~indent:(indent + 4) then_;
      match else_ with
      | Some e ->
          Printf.printf "%sElse:\n" (String.make (indent + 2) ' ');
          dump_expr ~indent:(indent + 4) e
      | None -> ())
  | Match { scrutinee; cases; match_span } ->
      Printf.printf "%sMatch %s\n" pad (string_of_span match_span);
      dump_expr ~indent:(indent + 2) scrutinee;
      List.iter
        (fun c ->
          Printf.printf "%s  Case:\n" pad;
          dump_pattern ~indent:(indent + 4) c.pattern;
          (match c.guard with
          | Some g -> dump_expr ~indent:(indent + 4) g
          | None -> ());
          dump_expr ~indent:(indent + 4) c.case_body)
        cases
  | Block (exprs, span) ->
      Printf.printf "%sBlock %s\n" pad (string_of_span span);
      List.iter (dump_expr ~indent:(indent + 2)) exprs
  | Apply (f, args, span) -> (
      match f with
      | Variable (name, _) ->
          Printf.printf "%sApply(%s) with %d arg(s) %s\n" pad name
            (List.length args) (string_of_span span);
          List.iteri
            (fun i arg ->
              Printf.printf "%s  [%d]:\n" pad i;
              dump_expr ~indent:(indent + 4) arg)
            args
      | Lambda _ | Apply _ | Let _ | If _ | Match _ | Block _ | Binary _
      | Unary _ | Literal _ | MemberAccess _ ->
          Printf.printf "%sApply %s\n" pad (string_of_span span);
          Printf.printf "%s  Function:\n" pad;
          dump_expr ~indent:(indent + 4) f;
          Printf.printf "%s  Arguments (%d):\n" pad (List.length args);
          List.iteri
            (fun i arg ->
              Printf.printf "%s    [%d]:\n" pad i;
              dump_expr ~indent:(indent + 6) arg)
            args)
  | MemberAccess (obj, member, span) ->
      Printf.printf "%sMemberAccess(.%s) %s\n" pad member (string_of_span span);
      Printf.printf "%s  Object:\n" pad;
      dump_expr ~indent:(indent + 4) obj

and dump_pattern ?(indent = 0) = function
  | PWildcard sp ->
      Printf.printf "%s_Pattern Wildcard %s\n" (String.make indent ' ')
        (string_of_span sp)
  | PVariable (name, sp) ->
      Printf.printf "%s_Pattern Var(%s) %s\n" (String.make indent ' ') name
        (string_of_span sp)
  | PIntLiteral (i, sp) ->
      Printf.printf "%s_Pattern Int(%Ld) %s\n" (String.make indent ' ') i
        (string_of_span sp)
  | PFloatLiteral (f, sp) ->
      Printf.printf "%s_Pattern Float(%g) %s\n" (String.make indent ' ') f
        (string_of_span sp)
  | PStringLiteral (s, sp) ->
      Printf.printf "%s_Pattern String(%S) %s\n" (String.make indent ' ') s
        (string_of_span sp)
  | PBooleanLiteral (b, sp) ->
      Printf.printf "%s_Pattern Bool(%b) %s\n" (String.make indent ' ') b
        (string_of_span sp)
  | PNil sp ->
      Printf.printf "%s_Pattern Nil %s\n" (String.make indent ' ')
        (string_of_span sp)
  | PCons (h, t, sp) ->
      Printf.printf "%s_Pattern Cons %s\n" (String.make indent ' ')
        (string_of_span sp);
      dump_pattern ~indent:(indent + 2) h;
      dump_pattern ~indent:(indent + 2) t

and string_of_ty = function
  | TyInt -> "int"
  | TyFloat -> "float"
  | TyString -> "string"
  | TyBool -> "bool"
  | TyUnit -> "unit"
  | TyVar v -> v
  | TyMeta mv -> (
      match !(mv.contents) with
      | None -> Printf.sprintf "?%d" mv.id
      | Some t -> string_of_ty t)
  | TyModule (namespace, name) -> namespace ^ ":" ^ name
  | TyList t -> Printf.sprintf "List[%s]" (string_of_ty t)
  | TyFunction (args, ret) ->
      Printf.sprintf "Fun(%s -> %s)"
        (String.concat ", " (List.map string_of_ty args))
        (string_of_ty ret)

and get_let_type_string ty body =
  match ty with
  | Some t -> string_of_ty t
  | None -> (
      match body with
      | Lambda lambda ->
          let params_str =
            String.concat ", "
              (List.map
                 (fun (p : param) ->
                   match p.param_ty with
                   | Some t ->
                       Printf.sprintf "%s: %s" p.param_name (string_of_ty t)
                   | None -> Printf.sprintf "%s: ?" p.param_name)
                 lambda.params)
          in
          let ret_str =
            match lambda.ret_ty with Some t -> string_of_ty t | None -> "?"
          in
          if lambda.params = [] then ret_str
          else Printf.sprintf "Fun(%s -> %s)" params_str ret_str
      | Literal _ | Variable _ | Apply _ | Let _ | If _ | Match _ | Block _
      | Binary _ | Unary _ | MemberAccess _ ->
          "?")

let dump_import imp =
  Printf.printf "Import(\"%s:%s\") %s\n" imp.module_ imp.item
    (string_of_span imp.import_span)

let dump_program prog =
  List.iter dump_import prog.imports;
  List.iter (dump_expr ~indent:0) prog.body

open Ast

let rec dump_tokens lexbuf =
  let token = Lexer.token lexbuf in
  Printf.printf "%s\n" (Token.to_string token);
  match token with Parser.EndOfFile _ -> () | _ -> dump_tokens lexbuf

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
  | IntLiteral (i, sp) ->
      Printf.printf "%sInt(%Ld) %s\n" pad i (string_of_span sp)
  | FloatLiteral (f, sp) ->
      Printf.printf "%sFloat(%g) %s\n" pad f (string_of_span sp)
  | StringLiteral (s, sp) ->
      Printf.printf "%sString(%S) %s\n" pad s (string_of_span sp)
  | BooleanLiteral (b, sp) ->
      Printf.printf "%sBool(%b) %s\n" pad b (string_of_span sp)
  | UnitLiteral sp -> Printf.printf "%sUnit %s\n" pad (string_of_span sp)
  | Variable (name, sp) ->
      Printf.printf "%sVar(%s) %s\n" pad name (string_of_span sp)
  | Binary { op; left; right; span } ->
      Printf.printf "%sBinary(%s) %s\n" pad (string_of_binop op)
        (string_of_span span);
      dump_expr ~indent:(indent + 2) left;
      dump_expr ~indent:(indent + 2) right
  | Unary { op; expr; span } ->
      Printf.printf "%sUnary(%s) %s\n" pad (string_of_unop op)
        (string_of_span span);
      dump_expr ~indent:(indent + 2) expr
  | Let { name; ty; body; span } ->
      let ty_str =
        match ty with None -> "Infer" | Some ty -> string_of_ty ty
      in
      Printf.printf "%sLet(%s : %s) %s\n" pad name ty_str (string_of_span span);
      dump_expr ~indent:(indent + 2) body
  | Lambda { params; ret_ty = _; body; span } ->
      Printf.printf "%sLambda %s\n" pad (string_of_span span);
      List.iter
        (fun (p : param) ->
          let ty_str =
            match p.ty with None -> "Infer" | Some t -> string_of_ty t
          in
          Printf.printf "%s  Param(%s : %s)\n" pad p.name ty_str)
        params;
      dump_expr ~indent:(indent + 2) body
  | If { cond; then_; else_; span } ->
      Printf.printf "%sIf %s\n" pad (string_of_span span);
      dump_expr ~indent:(indent + 2) cond;
      dump_expr ~indent:(indent + 2) then_;
      dump_expr ~indent:(indent + 2) else_
  | Match { scrutinee; cases; span } ->
      Printf.printf "%sMatch %s\n" pad (string_of_span span);
      dump_expr ~indent:(indent + 2) scrutinee;
      List.iter
        (fun c ->
          Printf.printf "%s  Case:\n" pad;
          dump_pattern ~indent:(indent + 4) c.pattern;
          (match c.guard with
          | Some g -> dump_expr ~indent:(indent + 4) g
          | None -> ());
          dump_expr ~indent:(indent + 4) c.body)
        cases
  | Block (exprs, span) ->
      Printf.printf "%sBlock %s\n" pad (string_of_span span);
      List.iter (dump_expr ~indent:(indent + 2)) exprs
  | Apply (f, args, span) ->
      Printf.printf "%sApply %s\n" pad (string_of_span span);
      dump_expr ~indent:(indent + 2) f;
      List.iter (dump_expr ~indent:(indent + 2)) args

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
  | TyList t -> Printf.sprintf "List[%s]" (string_of_ty t)
  | TyFunction (args, ret) ->
      Printf.sprintf "Fun(%s -> %s)"
        (String.concat ", " (List.map string_of_ty args))
        (string_of_ty ret)

let dump_program prog = List.iter (dump_expr ~indent:0) prog

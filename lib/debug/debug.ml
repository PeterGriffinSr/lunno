open Lunno_common

let rec dump_pattern ?(indent = 0) = function
  | Ast.PWildcard sp ->
      Printf.printf "%s_Pattern Wildcard %s\n" (String.make indent ' ')
        (Ty_utils.string_of_span sp)
  | Ast.PVariable (name, sp) ->
      Printf.printf "%s_Pattern Var(%s) %s\n" (String.make indent ' ') name
        (Ty_utils.string_of_span sp)
  | Ast.PIntLiteral (i, sp) ->
      Printf.printf "%s_Pattern Int(%Ld) %s\n" (String.make indent ' ') i
        (Ty_utils.string_of_span sp)
  | Ast.PFloatLiteral (f, sp) ->
      Printf.printf "%s_Pattern Float(%g) %s\n" (String.make indent ' ') f
        (Ty_utils.string_of_span sp)
  | Ast.PStringLiteral (s, sp) ->
      Printf.printf "%s_Pattern String(%S) %s\n" (String.make indent ' ') s
        (Ty_utils.string_of_span sp)
  | Ast.PBooleanLiteral (b, sp) ->
      Printf.printf "%s_Pattern Bool(%b) %s\n" (String.make indent ' ') b
        (Ty_utils.string_of_span sp)
  | Ast.PNil sp ->
      Printf.printf "%s_Pattern Nil %s\n" (String.make indent ' ')
        (Ty_utils.string_of_span sp)
  | Ast.PCons (h, t, sp) ->
      Printf.printf "%s_Pattern Cons %s\n" (String.make indent ' ')
        (Ty_utils.string_of_span sp);
      dump_pattern ~indent:(indent + 2) h;
      dump_pattern ~indent:(indent + 2) t

and dump_expr ?(indent = 0) e =
  let pad = String.make indent ' ' in
  match e with
  | Typed_ast.Literal (lit, ty, sp) -> (
      let ty_str = Ty_utils.string_of_ty ty in
      match lit with
      | Ast.LInt i ->
          Printf.printf "%sInt(%Ld) : %s %s\n" pad i ty_str
            (Ty_utils.string_of_span sp)
      | Ast.LFloat f ->
          Printf.printf "%sFloat(%g) : %s %s\n" pad f ty_str
            (Ty_utils.string_of_span sp)
      | Ast.LString s ->
          Printf.printf "%sString(%S) : %s %s\n" pad s ty_str
            (Ty_utils.string_of_span sp)
      | Ast.LBool b ->
          Printf.printf "%sBool(%b) : %s %s\n" pad b ty_str
            (Ty_utils.string_of_span sp)
      | Ast.LUnit ->
          Printf.printf "%sUnit : %s %s\n" pad ty_str
            (Ty_utils.string_of_span sp)
      | Ast.LNil ->
          Printf.printf "%sNil : %s %s\n" pad ty_str
            (Ty_utils.string_of_span sp))
  | Typed_ast.Variable (name, ty, sp) ->
      Printf.printf "%sVar(%s) : %s %s\n" pad name (Ty_utils.string_of_ty ty)
        (Ty_utils.string_of_span sp)
  | Typed_ast.Binary
      {
        Typed_ast.binary_op;
        Typed_ast.left;
        Typed_ast.right;
        Typed_ast.binary_ty;
        Typed_ast.binary_span;
      } ->
      Printf.printf "%sBinary(%s) : %s %s\n" pad
        (Ty_utils.string_of_binop binary_op)
        (Ty_utils.string_of_ty binary_ty)
        (Ty_utils.string_of_span binary_span);
      dump_expr ~indent:(indent + 2) left;
      dump_expr ~indent:(indent + 2) right
  | Typed_ast.Unary
      {
        Typed_ast.unary_op;
        Typed_ast.expr;
        Typed_ast.unary_ty;
        Typed_ast.unary_span;
      } ->
      Printf.printf "%sUnary(%s) : %s %s\n" pad
        (Ty_utils.string_of_unop unary_op)
        (Ty_utils.string_of_ty unary_ty)
        (Ty_utils.string_of_span unary_span);
      dump_expr ~indent:(indent + 2) expr
  | Typed_ast.Let
      {
        Typed_ast.name;
        Typed_ast.let_ty;
        Typed_ast.let_body;
        Typed_ast.let_span;
      } ->
      Printf.printf "%sLet(%s : %s) %s\n" pad name
        (Ty_utils.string_of_ty let_ty)
        (Ty_utils.string_of_span let_span);
      dump_expr ~indent:(indent + 2) let_body
  | Typed_ast.Lambda
      {
        Typed_ast.params;
        Typed_ast.ret_ty;
        Typed_ast.lambda_body;
        Typed_ast.is_recursive;
        Typed_ast.lambda_ty;
        Typed_ast.lambda_span;
      } ->
      let params_str =
        String.concat ", "
          (List.map
             (fun (p : Typed_ast.param) ->
               Printf.sprintf "%s: %s" p.Typed_ast.param_name
                 (Ty_utils.string_of_ty p.Typed_ast.param_ty))
             params)
      in
      let ret_str = Ty_utils.string_of_ty ret_ty in
      let rec_str = if is_recursive then " [recursive]" else "" in
      let _ = lambda_ty in
      (match params with
      | [] ->
          Printf.printf "%sLambda(%s)%s %s\n" pad ret_str rec_str
            (Ty_utils.string_of_span lambda_span)
      | _ ->
          Printf.printf "%sLambda(%s -> %s)%s %s\n" pad params_str ret_str
            rec_str
            (Ty_utils.string_of_span lambda_span));
      dump_expr ~indent:(indent + 2) lambda_body
  | Typed_ast.If
      {
        Typed_ast.cond;
        Typed_ast.then_;
        Typed_ast.else_;
        Typed_ast.if_ty;
        Typed_ast.if_span;
      } -> (
      Printf.printf "%sIf : %s %s\n" pad
        (Ty_utils.string_of_ty if_ty)
        (Ty_utils.string_of_span if_span);
      dump_expr ~indent:(indent + 2) cond;
      Printf.printf "%sThen:\n" (String.make (indent + 2) ' ');
      dump_expr ~indent:(indent + 4) then_;
      match else_ with
      | Some e ->
          Printf.printf "%sElse:\n" (String.make (indent + 2) ' ');
          dump_expr ~indent:(indent + 4) e
      | None -> ())
  | Typed_ast.Match
      {
        Typed_ast.scrutinee;
        Typed_ast.cases;
        Typed_ast.match_ty;
        Typed_ast.match_span;
      } ->
      Printf.printf "%sMatch : %s %s\n" pad
        (Ty_utils.string_of_ty match_ty)
        (Ty_utils.string_of_span match_span);
      dump_expr ~indent:(indent + 2) scrutinee;
      List.iter
        (fun (c : Typed_ast.match_case) ->
          Printf.printf "%s  Case:\n" pad;
          dump_pattern ~indent:(indent + 4) c.Typed_ast.pattern;
          (match c.Typed_ast.guard with
          | Some g -> dump_expr ~indent:(indent + 4) g
          | None -> ());
          dump_expr ~indent:(indent + 4) c.Typed_ast.case_body)
        cases
  | Typed_ast.Block (exprs, ty, span) ->
      Printf.printf "%sBlock : %s %s\n" pad (Ty_utils.string_of_ty ty)
        (Ty_utils.string_of_span span);
      List.iter (dump_expr ~indent:(indent + 2)) exprs
  | Typed_ast.Apply (f, args, ty, span) -> (
      match f with
      | Typed_ast.Variable (name, _, _) ->
          Printf.printf "%sApply(%s) : %s with %d arg(s) %s\n" pad name
            (Ty_utils.string_of_ty ty) (List.length args)
            (Ty_utils.string_of_span span);
          List.iteri
            (fun i arg ->
              Printf.printf "%s  [%d]:\n" pad i;
              dump_expr ~indent:(indent + 4) arg)
            args
      | Typed_ast.Lambda _ | Typed_ast.Apply _ | Typed_ast.Let _
      | Typed_ast.If _ | Typed_ast.Match _ | Typed_ast.Block _
      | Typed_ast.Binary _ | Typed_ast.Unary _ | Typed_ast.Literal _
      | Typed_ast.MemberAccess _ | Typed_ast.Range _ ->
          Printf.printf "%sApply : %s %s\n" pad (Ty_utils.string_of_ty ty)
            (Ty_utils.string_of_span span);
          Printf.printf "%s  Function:\n" pad;
          dump_expr ~indent:(indent + 4) f;
          Printf.printf "%s  Arguments (%d):\n" pad (List.length args);
          List.iteri
            (fun i arg ->
              Printf.printf "%s    [%d]:\n" pad i;
              dump_expr ~indent:(indent + 6) arg)
            args)
  | Typed_ast.MemberAccess (obj, member, ty, span) ->
      Printf.printf "%sMemberAccess(.%s) : %s %s\n" pad member
        (Ty_utils.string_of_ty ty)
        (Ty_utils.string_of_span span);
      Printf.printf "%s  Object:\n" pad;
      dump_expr ~indent:(indent + 4) obj
  | Typed_ast.Range (start_expr, end_expr, span) ->
      Printf.printf "%sRange : List[int] %s\n" pad
        (Ty_utils.string_of_span span);
      Printf.printf "%s  Start:\n" pad;
      dump_expr ~indent:(indent + 4) start_expr;
      Printf.printf "%s  End:\n" pad;
      dump_expr ~indent:(indent + 4) end_expr

let dump_import (imp : Ast.import) =
  Printf.printf "Import(\"%s:%s\") %s\n" imp.Ast.module_ imp.Ast.item
    (Ty_utils.string_of_span imp.Ast.import_span)

let dump_program (prog : Typed_ast.program) =
  List.iter dump_import prog.Typed_ast.imports;
  List.iter (dump_expr ~indent:0) prog.Typed_ast.body

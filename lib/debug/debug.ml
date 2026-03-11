open Lunno_frontend
open Lunno_common

let coq = Util.coq_string_to_string

let show_ty t =
  let rec go = function
    | Lunno.TyInt -> "int"
    | Lunno.TyFloat -> "float"
    | Lunno.TyString -> "string"
    | Lunno.TyBool -> "bool"
    | Lunno.TyUnit -> "unit"
    | Lunno.TyVar nm -> coq nm
    | Lunno.TyList inner -> Printf.sprintf "list %s" (go inner)
    | Lunno.TyFun (params, ret) ->
        let ps = String.concat ", " (List.map go params) in
        Printf.sprintf "(%s) -> %s" ps (go ret)
    | Lunno.TyAdt nm -> coq nm
  in
  go t

let show_binop = function
  | Lunno.OpAdd -> "+"
  | Lunno.OpSub -> "-"
  | Lunno.OpMul -> "*"
  | Lunno.OpDiv -> "/"
  | Lunno.OpEqual -> "="
  | Lunno.OpNotEqual -> "<>"
  | Lunno.OpLess -> "<"
  | Lunno.OpGreater -> ">"
  | Lunno.OpCons -> "::"

let rec dump_pattern ?(indent = 0) pat =
  let pad = String.make indent ' ' in
  match pat with
  | Lunno.PWildcard -> Printf.printf "%s_Pattern Wildcard\n" pad
  | Lunno.PVariable nm -> Printf.printf "%s_Pattern Var(%s)\n" pad (coq nm)
  | Lunno.PIntLit n -> Printf.printf "%s_Pattern Int(%d)\n" pad n
  | Lunno.PFloatLit s -> Printf.printf "%s_Pattern Float(%s)\n" pad (coq s)
  | Lunno.PStringLit s -> Printf.printf "%s_Pattern String(%S)\n" pad (coq s)
  | Lunno.PBoolLit b -> Printf.printf "%s_Pattern Bool(%b)\n" pad b
  | Lunno.PNil -> Printf.printf "%s_Pattern Nil\n" pad
  | Lunno.PCons (h, t) ->
      Printf.printf "%s_Pattern Cons\n" pad;
      dump_pattern ~indent:(indent + 2) h;
      dump_pattern ~indent:(indent + 2) t
  | Lunno.PConstructor (nm, fields) ->
      Printf.printf "%s_Pattern Constructor(%s)\n" pad (coq nm);
      List.iter (dump_pattern ~indent:(indent + 2)) fields

let rec dump_spanned_expr ?(indent = 0) e =
  let pad = String.make indent ' ' in
  let i2 = indent + 2 in
  let i4 = indent + 4 in
  let sp = Spanned_ast.span_of e in
  let loc = Span.string_of_span sp in
  match e with
  | Spanned_ast.Lit (lit, _) -> (
      match lit with
      | Lunno.LInt n -> Printf.printf "%sInt(%d) %s\n" pad n loc
      | Lunno.LFloat s -> Printf.printf "%sFloat(%s) %s\n" pad (coq s) loc
      | Lunno.LString s -> Printf.printf "%sString(%S) %s\n" pad (coq s) loc
      | Lunno.LBool b -> Printf.printf "%sBool(%b) %s\n" pad b loc
      | Lunno.LUnit -> Printf.printf "%sUnit %s\n" pad loc
      | Lunno.LNil -> Printf.printf "%sNil %s\n" pad loc)
  | Spanned_ast.Var (nm, _) -> Printf.printf "%sVar(%s) %s\n" pad nm loc
  | Spanned_ast.Lam (params, ret_ty, body, is_rec, _) ->
      let ps =
        String.concat ", "
          (List.map
             (fun p ->
               let nm = coq p.Lunno.param_name in
               match p.Lunno.param_ty with
               | None -> nm
               | Some t -> Printf.sprintf "%s: %s" nm (show_ty t))
             params)
      in
      let rec_str = if is_rec then " [recursive]" else "" in
      let ret_str =
        match ret_ty with
        | None -> ""
        | Some t -> Printf.sprintf " -> %s" (show_ty t)
      in
      Printf.printf "%sLambda(%s)%s%s %s\n" pad ps rec_str ret_str loc;
      dump_spanned_expr ~indent:i2 body
  | Spanned_ast.App (f, args, _) -> (
      match f with
      | Spanned_ast.Var (nm, _) ->
          Printf.printf "%sApply(%s) with %d arg(s) %s\n" pad nm
            (List.length args) loc;
          List.iteri
            (fun i arg ->
              Printf.printf "%s  [%d]:\n" pad i;
              dump_spanned_expr ~indent:i4 arg)
            args
      | Spanned_ast.Lam _ | Spanned_ast.App _ | Spanned_ast.LetE _
      | Spanned_ast.IfE _ | Spanned_ast.MatchE _ | Spanned_ast.Block _
      | Spanned_ast.BinOp _ | Spanned_ast.UnOp _ | Spanned_ast.Lit _
      | Spanned_ast.MemberAccess _ | Spanned_ast.Range _
      | Spanned_ast.Constructor _ ->
          Printf.printf "%sApply %s\n" pad loc;
          Printf.printf "%s  Function:\n" pad;
          dump_spanned_expr ~indent:i4 f;
          Printf.printf "%s  Arguments (%d):\n" pad (List.length args);
          List.iteri
            (fun i arg ->
              Printf.printf "%s    [%d]:\n" pad i;
              dump_spanned_expr ~indent:(indent + 6) arg)
            args)
  | Spanned_ast.LetE (nm, ty, body, _) ->
      let ty_str =
        match ty with None -> "" | Some t -> Printf.sprintf ": %s" (show_ty t)
      in
      Printf.printf "%sLet(%s%s) %s\n" pad nm ty_str loc;
      dump_spanned_expr ~indent:i2 body
  | Spanned_ast.IfE (cond, then_, else_, _) -> (
      Printf.printf "%sIf %s\n" pad loc;
      dump_spanned_expr ~indent:i2 cond;
      Printf.printf "%s  Then:\n" pad;
      dump_spanned_expr ~indent:i4 then_;
      match else_ with
      | None -> ()
      | Some e ->
          Printf.printf "%s  Else:\n" pad;
          dump_spanned_expr ~indent:i4 e)
  | Spanned_ast.MatchE (scrut, cases, _) ->
      Printf.printf "%sMatch %s\n" pad loc;
      dump_spanned_expr ~indent:i2 scrut;
      List.iter
        (fun c ->
          Printf.printf "%s  Case: %s\n" pad
            (Span.string_of_span c.Spanned_ast.case_span);
          dump_pattern ~indent:i4 c.Spanned_ast.pat;
          (match c.Spanned_ast.guard with
          | None -> ()
          | Some g ->
              Printf.printf "%s    Guard:\n" pad;
              dump_spanned_expr ~indent:(indent + 6) g);
          dump_spanned_expr ~indent:i4 c.Spanned_ast.case_body)
        cases
  | Spanned_ast.Block (exprs, _) ->
      Printf.printf "%sBlock %s\n" pad loc;
      List.iter (dump_spanned_expr ~indent:i2) exprs
  | Spanned_ast.BinOp (op, l, r, _) ->
      Printf.printf "%sBinary(%s) %s\n" pad (show_binop op) loc;
      dump_spanned_expr ~indent:i2 l;
      dump_spanned_expr ~indent:i2 r
  | Spanned_ast.UnOp (Lunno.OpNegate, e, _) ->
      Printf.printf "%sNegate %s\n" pad loc;
      dump_spanned_expr ~indent:i2 e
  | Spanned_ast.MemberAccess (obj, field, _) ->
      Printf.printf "%sMemberAccess(.%s) %s\n" pad field loc;
      Printf.printf "%s  Object:\n" pad;
      dump_spanned_expr ~indent:i4 obj
  | Spanned_ast.Range (lo, hi, _) ->
      Printf.printf "%sRange %s\n" pad loc;
      Printf.printf "%s  lo:\n" pad;
      dump_spanned_expr ~indent:i4 lo;
      Printf.printf "%s  hi:\n" pad;
      dump_spanned_expr ~indent:i4 hi
  | Spanned_ast.Constructor (nm, args, _) ->
      Printf.printf "%sConstructor(%s) %s\n" pad nm loc;
      List.iteri
        (fun i arg ->
          Printf.printf "%s  [%d]:\n" pad i;
          dump_spanned_expr ~indent:i4 arg)
        args

let dump_spanned_import imp =
  Printf.printf "Import(\"%s:%s\") %s\n" imp.Spanned_ast.import_module
    imp.Spanned_ast.import_item
    (Span.string_of_span imp.Spanned_ast.import_span)

let dump_spanned_type_decl td =
  Printf.printf "Data(%s)\n" td.Spanned_ast.type_name;
  List.iter
    (fun (v : Spanned_ast.variant) ->
      match v.Spanned_ast.variant_fields with
      | [] -> Printf.printf "  | %s\n" v.Spanned_ast.variant_name
      | fields ->
          Printf.printf "  | %s(%s)\n" v.Spanned_ast.variant_name
            (String.concat ", " (List.map show_ty fields)))
    td.Spanned_ast.variants

let dump_program prog =
  List.iter dump_spanned_import prog.Spanned_ast.imports;
  List.iter dump_spanned_type_decl prog.Spanned_ast.type_decls;
  List.iter (dump_spanned_expr ~indent:0) prog.Spanned_ast.body

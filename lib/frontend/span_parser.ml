open Lunno_common

type cursor = { mutable toks : (Lunno.token * Span.span) list }

let make_cursor toks = { toks }
let peek cur = match cur.toks with [] -> None | (t, _) :: _ -> Some t

let peek_span cur =
  match cur.toks with [] -> Span.dummy_span | (_, sp) :: _ -> sp

let advance cur = match cur.toks with [] -> () | _ :: tl -> cur.toks <- tl

let consume cur =
  match cur.toks with
  | [] -> Span.dummy_span
  | (_, sp) :: tl ->
      cur.toks <- tl;
      sp

let consume_if cur tok =
  match cur.toks with
  | (t, _) :: tl when Lunno.token_beq t tok -> cur.toks <- tl
  | _ -> ()

let coq = Util.coq_string_to_string

let rec attach_expr cur e =
  match e with
  | Lunno.Lit l ->
      let sp = consume cur in
      Spanned_ast.Lit (l, sp)
  | Lunno.Var nm ->
      let sp = consume cur in
      Spanned_ast.Var (coq nm, sp)
  | Lunno.LetE (nm, ty, body) ->
      let sp_start = consume cur in
      advance cur;
      (match peek cur with
      | Some Lunno.TColon ->
          consume_if cur Lunno.TColon;
          skip_ty cur ty;
          consume_if cur Lunno.TEquals
      | Some Lunno.TEquals -> consume_if cur Lunno.TEquals
      | Some Lunno.TLParen -> (
          consume_if cur Lunno.TLParen;
          skip_params cur body;
          consume_if cur Lunno.TRParen;
          match peek cur with
          | Some Lunno.TArrow ->
              consume_if cur Lunno.TArrow;
              skip_ty cur ty
          | _ -> ())
      | Some Lunno.TLBrace -> ()
      | _ -> ());
      let body' = attach_expr cur body in
      let sp = Span.merge sp_start (Spanned_ast.span_of body') in
      Spanned_ast.LetE (coq nm, ty, body', sp)
  | Lunno.Lam (params, ret_ty, body, is_rec) ->
      let sp_start = peek_span cur in
      let body' = attach_expr cur body in
      let sp = Span.merge sp_start (Spanned_ast.span_of body') in
      Spanned_ast.Lam (params, ret_ty, body', is_rec, sp)
  | Lunno.App (f, args) ->
      let f' = attach_expr cur f in
      consume_if cur Lunno.TLParen;
      let args' = attach_expr_list cur args in
      let sp_end = consume cur in
      let sp = Span.merge (Spanned_ast.span_of f') sp_end in
      Spanned_ast.App (f', args', sp)
  | Lunno.IfE (cond, then_, else_) ->
      let sp_start = consume cur in
      let cond' = attach_expr cur cond in
      consume_if cur Lunno.TThen;
      let then_' = attach_expr cur then_ in
      let else_', sp_end =
        match else_ with
        | None -> (None, Spanned_ast.span_of then_')
        | Some e ->
            consume_if cur Lunno.TElse;
            let e' = attach_expr cur e in
            (Some e', Spanned_ast.span_of e')
      in
      Spanned_ast.IfE (cond', then_', else_', Span.merge sp_start sp_end)
  | Lunno.MatchE (scrut, cases) ->
      let sp_start = consume cur in
      let scrut' = attach_expr cur scrut in
      consume_if cur Lunno.TLBrace;
      let cases' = List.map (attach_case cur) cases in
      let sp_end = consume cur in
      Spanned_ast.MatchE (scrut', cases', Span.merge sp_start sp_end)
  | Lunno.Block exprs ->
      let sp_start = consume cur in
      let exprs' = List.map (attach_expr cur) exprs in
      let sp_end = consume cur in
      let sp =
        match exprs' with
        | [] -> Span.merge sp_start sp_end
        | _ -> Span.merge sp_start sp_end
      in
      Spanned_ast.Block (exprs', sp)
  | Lunno.BinOp (op, l, r) ->
      let l' = attach_expr cur l in
      advance cur;
      let r' = attach_expr cur r in
      let sp = Span.merge (Spanned_ast.span_of l') (Spanned_ast.span_of r') in
      Spanned_ast.BinOp (op, l', r', sp)
  | Lunno.UnOp (op, e) ->
      let sp_start = consume cur in
      let e' = attach_expr cur e in
      Spanned_ast.UnOp (op, e', Span.merge sp_start (Spanned_ast.span_of e'))
  | Lunno.MemberAccess (obj, field) ->
      let obj' = attach_expr cur obj in
      consume_if cur Lunno.TDot;
      let sp_end = consume cur in
      Spanned_ast.MemberAccess
        (obj', coq field, Span.merge (Spanned_ast.span_of obj') sp_end)
  | Lunno.Range (lo, hi) ->
      consume_if cur Lunno.TLBracket;
      let lo' = attach_expr cur lo in
      consume_if cur Lunno.TDotDot;
      let hi' = attach_expr cur hi in
      let sp_end = consume cur in
      Spanned_ast.Range (lo', hi', Span.merge (Spanned_ast.span_of lo') sp_end)
  | Lunno.Constructor (nm, args) ->
      let sp_start = consume cur in
      consume_if cur Lunno.TLParen;
      let args' = attach_expr_list cur args in
      let sp_end = consume cur in
      Spanned_ast.Constructor (coq nm, args', Span.merge sp_start sp_end)

and attach_expr_list cur = function
  | [] -> []
  | [ e ] -> [ attach_expr cur e ]
  | e :: rest ->
      let e' = attach_expr cur e in
      consume_if cur Lunno.TComma;
      e' :: attach_expr_list cur rest

and attach_case cur (Lunno.MkCase (pat, guard, case_body)) =
  let sp_start = consume cur in
  advance cur;
  let guard' =
    match guard with
    | None -> None
    | Some g ->
        consume_if cur Lunno.TIf;
        Some (attach_expr cur g)
  in
  consume_if cur Lunno.TArrow;
  let body' = attach_expr cur case_body in
  let sp = Span.merge sp_start (Spanned_ast.span_of body') in
  { Spanned_ast.pat; guard = guard'; case_body = body'; case_span = sp }

and skip_ty cur _ty =
  let rec go () =
    match peek cur with
    | Some Lunno.TInt
    | Some Lunno.TFloat
    | Some Lunno.TString
    | Some Lunno.TBool
    | Some Lunno.TUnit
    | Some Lunno.TList
    | Some (Lunno.TIdent _) ->
        advance cur;
        go ()
    | Some Lunno.TLParen ->
        advance cur;
        go ();
        consume_if cur Lunno.TRParen
    | _ -> ()
  in
  go ()

and skip_params cur _body =
  let rec go () =
    match peek cur with
    | Some (Lunno.TIdent _) -> (
        advance cur;
        (match peek cur with
        | Some Lunno.TColon ->
            advance cur;
            skip_ty cur None
        | _ -> ());
        match peek cur with
        | Some Lunno.TComma ->
            advance cur;
            go ()
        | _ -> ())
    | _ -> ()
  in
  go ()

let attach_import cur imp =
  let sp_start = consume cur in
  let sp_end = consume cur in
  let path = coq imp.Lunno.import_module in
  let module_, item =
    match String.split_on_char ':' path with
    | [ m; i ] -> (m, i)
    | _ -> (path, "")
  in
  {
    Spanned_ast.import_module = module_;
    import_item = item;
    import_span = Span.merge sp_start sp_end;
  }

let attach_program prog positioned =
  let cur = make_cursor positioned in
  let imports = List.map (attach_import cur) prog.Lunno.imports in
  let type_decls =
    List.map
      (fun td ->
        {
          Spanned_ast.type_name = coq td.Lunno.type_name;
          variants =
            List.map
              (fun v ->
                {
                  Spanned_ast.variant_name = coq v.Lunno.variant_name;
                  variant_fields = v.Lunno.variant_fields;
                })
              td.Lunno.variants;
        })
      prog.Lunno.type_decls
  in
  let body = List.map (attach_expr cur) prog.Lunno.body in
  { Spanned_ast.imports; type_decls; body }

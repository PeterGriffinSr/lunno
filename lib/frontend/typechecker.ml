open Ast
open Lunno_common.Error

let bind_name name ty span env =
  if Env.mem name env then raise (already_defined name span)
  else Env.add name ty env

let check_literal = function
  | LInt _ -> TyInt
  | LFloat _ -> TyFloat
  | LString _ -> TyString
  | LBool _ -> TyBool
  | LUnit -> TyUnit

let rec check_pattern env expected pat =
  match pat with
  | PWildcard _ -> env
  | PVariable (name, span) -> bind_name name expected span env
  | PIntLiteral (_, span) ->
      if expected = TyInt then env
      else
        raise
          (pattern_type_mismatch
             (Debug.string_of_ty expected)
             (Debug.string_of_ty TyInt) span)
  | PFloatLiteral (_, span) ->
      if expected = TyFloat then env
      else
        raise
          (pattern_type_mismatch
             (Debug.string_of_ty expected)
             (Debug.string_of_ty TyFloat)
             span)
  | PStringLiteral (_, span) ->
      if expected = TyString then env
      else
        raise
          (pattern_type_mismatch
             (Debug.string_of_ty expected)
             (Debug.string_of_ty TyString)
             span)
  | PBooleanLiteral (_, span) ->
      if expected = TyBool then env
      else
        raise
          (pattern_type_mismatch
             (Debug.string_of_ty expected)
             (Debug.string_of_ty TyBool)
             span)
  | PNil span -> (
      match expected with
      | TyList _ -> env
      | TyInt | TyFloat | TyString | TyBool | TyUnit | TyVar _ | TyFunction _ ->
          raise (pattern_type_mismatch (Debug.string_of_ty expected) "[_]" span)
      )
  | PCons (hd, tl, span) -> (
      match expected with
      | TyList elem_ty ->
          let env' = check_pattern env elem_ty hd in
          check_pattern env' (TyList elem_ty) tl
      | TyInt | TyFloat | TyString | TyBool | TyUnit | TyVar _
      | TyFunction (_, _) ->
          raise (pattern_type_mismatch (Debug.string_of_ty expected) "[_]" span)
      )

let rec check_expr env expr =
  match expr with
  | Literal (lit, _) -> check_literal lit
  | Variable (name, span) -> (
      match Env.find name env with
      | Some ty -> ty
      | None -> raise (undefined_variable name span))
  | Lambda lam -> check_lambda env lam
  | Apply (func, args, span) -> (
      let func_ty = check_expr env func in
      match func_ty with
      | TyFunction (param_tys, ret_ty) ->
          let expected_arity = List.length param_tys in
          let got_arity = List.length args in
          if expected_arity <> got_arity then
            raise (arity_mismatch expected_arity got_arity span);
          List.iter2
            (fun param_ty arg ->
              let arg_ty = check_expr env arg in
              if arg_ty <> param_ty then
                raise
                  (type_mismatch
                     (Debug.string_of_ty param_ty)
                     (Debug.string_of_ty arg_ty)
                     (span_of_expr arg)))
            param_tys args;
          ret_ty
      | TyInt | TyFloat | TyString | TyBool | TyUnit | TyVar _ | TyList _ ->
          raise (not_a_function (Debug.string_of_ty func_ty) span))
  | Let { name = _; ty; let_body; let_span } ->
      let body_ty = check_expr env let_body in
      (match ty with
      | Some ann_ty when ann_ty <> body_ty ->
          raise
            (type_mismatch
               (Debug.string_of_ty ann_ty)
               (Debug.string_of_ty body_ty)
               let_span)
      | _ -> ());
      body_ty
  | If { cond; then_; else_; if_span } -> (
      let cond_ty = check_expr env cond in
      if cond_ty <> TyBool then
        raise
          (type_mismatch
             (Debug.string_of_ty TyBool)
             (Debug.string_of_ty cond_ty)
             (span_of_expr cond));
      let then_ty = check_expr env then_ in
      match else_ with
      | None -> raise (missing_else_branch if_span)
      | Some else_expr ->
          let else_ty = check_expr env else_expr in
          if then_ty <> else_ty then
            raise
              (if_branch_mismatch
                 (Debug.string_of_ty then_ty)
                 (Debug.string_of_ty else_ty)
                 if_span);
          then_ty)
  | Match { scrutinee; cases; match_span = _ } ->
      let scrutinee_ty = check_expr env scrutinee in
      check_match env scrutinee_ty cases
  | Block (exprs, span) -> check_block env exprs span
  | Binary { binary_op; left; right; binary_span } ->
      check_binary env binary_op left right binary_span
  | Unary { unary_op = OpNegate; expr = e; unary_span } -> (
      let ty = check_expr env e in
      match ty with
      | TyInt | TyFloat -> ty
      | TyString | TyBool | TyUnit | TyVar _ | TyList _ | TyFunction (_, _) ->
          raise
            (type_mismatch (Debug.string_of_ty TyInt) (Debug.string_of_ty ty)
               unary_span))

and check_lambda env lam =
  let param_tys =
    List.map
      (fun param ->
        match param.param_ty with
        | Some ty -> ty
        | None -> raise (missing_annotation param.param_name param.param_span))
      lam.params
  in
  let body_env =
    List.fold_left2
      (fun env' param ty -> bind_name param.param_name ty param.param_span env')
      env lam.params param_tys
  in
  let body_ty = check_expr body_env lam.lambda_body in
  (match lam.ret_ty with
  | Some ann_ret when ann_ret <> body_ty ->
      let body_span =
        match lam.lambda_body with
        | Block ((_ :: _ as exprs), _) ->
            span_of_expr (List.nth exprs (List.length exprs - 1))
        | Block ([], _)
        | Literal _ | Variable _ | Lambda _ | Apply _ | Let _ | If _ | Match _
        | Binary _ | Unary _ ->
            span_of_expr lam.lambda_body
      in
      raise
        (type_mismatch
           (Debug.string_of_ty ann_ret)
           (Debug.string_of_ty body_ty)
           body_span)
  | Some _ -> ()
  | None -> ());
  TyFunction (param_tys, body_ty)

and check_match env scrutinee_ty cases =
  match cases with
  | [] -> TyUnit
  | first :: rest ->
      let env' = check_pattern env scrutinee_ty first.pattern in
      let env'' =
        match first.guard with
        | None -> env'
        | Some g ->
            let g_ty = check_expr env' g in
            if g_ty <> TyBool then
              raise
                (type_mismatch
                   (Debug.string_of_ty TyBool)
                   (Debug.string_of_ty g_ty) (span_of_expr g));
            env'
      in
      let first_ty = check_expr env'' first.case_body in
      List.iter
        (fun case ->
          let env' = check_pattern env scrutinee_ty case.pattern in
          let env'' =
            match case.guard with
            | None -> env'
            | Some g ->
                let g_ty = check_expr env' g in
                if g_ty <> TyBool then
                  raise
                    (type_mismatch
                       (Debug.string_of_ty TyBool)
                       (Debug.string_of_ty g_ty) (span_of_expr g));
                env'
          in
          let case_ty = check_expr env'' case.case_body in
          if case_ty <> first_ty then
            raise
              (match_branch_mismatch
                 (Debug.string_of_ty first_ty)
                 (Debug.string_of_ty case_ty)
                 case.case_span))
        rest;
      first_ty

and check_block env exprs _span =
  let rec go env = function
    | [] -> TyUnit
    | [ e ] -> check_expr env e
    | Let { name; ty; let_body; let_span } :: rest ->
        let body_ty =
          match let_body with
          | Lambda lam ->
              if lam.is_recursive then
                let param_tys =
                  List.map
                    (fun param ->
                      match param.param_ty with
                      | Some t -> t
                      | None ->
                          raise
                            (missing_annotation param.param_name
                               param.param_span))
                    lam.params
                in
                let ret_ty =
                  match lam.ret_ty with
                  | Some t -> t
                  | None -> raise (missing_annotation name let_span)
                in
                let pre_env =
                  bind_name name (TyFunction (param_tys, ret_ty)) let_span env
                in
                check_expr pre_env let_body
              else check_expr env let_body
          | Literal _ | Variable _ | Apply _ | Let _ | If _ | Match _ | Block _
          | Binary _ | Unary _ ->
              check_expr env let_body
        in
        (match ty with
        | Some ann_ty ->
            if ann_ty <> body_ty then
              raise
                (type_mismatch
                   (Debug.string_of_ty ann_ty)
                   (Debug.string_of_ty body_ty)
                   let_span)
        | None -> ());
        let env' = bind_name name body_ty let_span env in
        go env' rest
    | (( Literal _ | Variable _ | Lambda _ | Apply _ | If _ | Match _ | Block _
       | Binary _ | Unary _ ) as e)
      :: rest ->
        let _ = check_expr env e in
        go env rest
  in
  go env exprs

and check_binary env op left right _span =
  let left_ty = check_expr env left in
  let right_ty = check_expr env right in
  let left_span = span_of_expr left in
  let right_span = span_of_expr right in
  match op with
  | OpAdd | OpSub | OpMul | OpDiv -> (
      match (left_ty, right_ty) with
      | TyInt, TyInt -> TyInt
      | TyFloat, TyFloat -> TyFloat
      | TyFloat, got ->
          raise (type_mismatch "float" (Debug.string_of_ty got) right_span)
      | TyInt, got ->
          raise (type_mismatch "int" (Debug.string_of_ty got) right_span)
      | TyString, _
      | TyBool, _
      | TyUnit, _
      | TyVar _, _
      | TyList _, _
      | TyFunction (_, _), _ ->
          raise (type_mismatch "int" (Debug.string_of_ty left_ty) left_span))
  | OpEqual | OpNotEqual ->
      if left_ty <> right_ty then
        raise
          (type_mismatch
             (Debug.string_of_ty left_ty)
             (Debug.string_of_ty right_ty)
             right_span);
      TyBool
  | OpLess | OpGreater -> (
      match (left_ty, right_ty) with
      | TyInt, TyInt | TyFloat, TyFloat -> TyBool
      | TyFloat, got ->
          raise (type_mismatch "float" (Debug.string_of_ty got) right_span)
      | TyInt, got ->
          raise (type_mismatch "int" (Debug.string_of_ty got) right_span)
      | TyString, _
      | TyBool, _
      | TyUnit, _
      | TyVar _, _
      | TyList _, _
      | TyFunction (_, _), _ ->
          raise (type_mismatch "int" (Debug.string_of_ty left_ty) left_span))
  | OpCons -> (
      match right_ty with
      | TyList elem_ty ->
          if left_ty <> elem_ty then
            raise
              (type_mismatch
                 (Debug.string_of_ty elem_ty)
                 (Debug.string_of_ty left_ty)
                 left_span);
          TyList elem_ty
      | TyInt | TyFloat | TyString | TyBool | TyUnit | TyVar _
      | TyFunction (_, _) ->
          raise
            (type_mismatch
               (Debug.string_of_ty (TyList left_ty))
               (Debug.string_of_ty right_ty)
               right_span))

let check_program (program : program) =
  let rec go env = function
    | [] -> ()
    | Let { name; ty; let_body; let_span } :: rest ->
        let body_ty =
          match let_body with
          | Lambda lam ->
              if lam.is_recursive then
                let param_tys =
                  List.map
                    (fun param ->
                      match param.param_ty with
                      | Some t -> t
                      | None ->
                          raise
                            (missing_annotation param.param_name
                               param.param_span))
                    lam.params
                in
                let ret_ty =
                  match lam.ret_ty with
                  | Some t -> t
                  | None -> raise (missing_annotation name let_span)
                in
                let pre_env =
                  bind_name name (TyFunction (param_tys, ret_ty)) let_span env
                in
                check_expr pre_env let_body
              else check_expr env let_body
          | Literal _ | Variable _ | Apply _ | Let _ | If _ | Match _ | Block _
          | Binary _ | Unary _ ->
              check_expr env let_body
        in
        (match ty with
        | Some ann_ty ->
            if ann_ty <> body_ty then
              raise
                (type_mismatch
                   (Debug.string_of_ty ann_ty)
                   (Debug.string_of_ty body_ty)
                   let_span)
        | None -> ());
        let env' = bind_name name body_ty let_span env in
        go env' rest
    | (( Literal _ | Variable _ | Lambda _ | Apply _ | If _ | Match _ | Block _
       | Binary _ | Unary _ ) as e)
      :: rest ->
        let _ = check_expr env e in
        go env rest
  in
  go Env.empty program.body

open Lunno_common
open Lunno_modules

let next_id = ref 0

let fresh_meta () =
  let id = !next_id in
  incr next_id;
  Ast.TyMeta { Ast.id; Ast.contents = ref None }

let rec resolve ty =
  match ty with
  | Ast.TyMeta { Ast.contents; _ } -> (
      match !contents with Some t -> resolve t | None -> ty)
  | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit
  | Ast.TyList _ | Ast.TyFunction _ | Ast.TyVar _ | Ast.TyModule _ ->
      ty

let rec occurs id ty =
  match resolve ty with
  | Ast.TyMeta { Ast.id = id2; _ } -> id = id2
  | Ast.TyList t -> occurs id t
  | Ast.TyFunction (params, ret) ->
      List.exists (occurs id) params || occurs id ret
  | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit
  | Ast.TyVar _ | Ast.TyModule _ ->
      false

let rec unify span t1 t2 =
  let t1 = resolve t1 and t2 = resolve t2 in
  match (t1, t2) with
  | Ast.TyInt, Ast.TyInt
  | Ast.TyFloat, Ast.TyFloat
  | Ast.TyString, Ast.TyString
  | Ast.TyBool, Ast.TyBool
  | Ast.TyUnit, Ast.TyUnit ->
      ()
  | Ast.TyVar a, Ast.TyVar b when a = b -> ()
  | Ast.TyList a, Ast.TyList b -> unify span a b
  | Ast.TyFunction (ps1, r1), Ast.TyFunction (ps2, r2) ->
      if List.length ps1 <> List.length ps2 then
        raise
          (Error.TypeError
             {
               code = Error.E_Type_TypeMismatch;
               msg =
                 Printf.sprintf "type mismatch: expected '%s', got '%s'"
                   (Ty_utils.string_of_ty t1) (Ty_utils.string_of_ty t2);
               span;
             });
      List.iter2 (unify span) ps1 ps2;
      unify span r1 r2
  | Ast.TyMeta m, t | t, Ast.TyMeta m -> (
      match !(m.Ast.contents) with
      | Some _ -> assert false
      | None ->
          if occurs m.Ast.id t then
            match t with
            | Ast.TyMeta m2 when m.Ast.id = m2.Ast.id -> ()
            | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit
            | Ast.TyVar _ | Ast.TyList _ | Ast.TyFunction _ | Ast.TyModule _
            | Ast.TyMeta _ ->
                raise
                  (Error.TypeError
                     {
                       code = Error.E_Type_TypeMismatch;
                       msg =
                         Printf.sprintf "type mismatch: expected '%s', got '%s'"
                           (Ty_utils.string_of_ty t1) (Ty_utils.string_of_ty t2);
                       span;
                     })
          else m.Ast.contents := Some t)
  | Ast.TyInt, _
  | Ast.TyFloat, _
  | Ast.TyString, _
  | Ast.TyBool, _
  | Ast.TyUnit, _
  | Ast.TyVar _, _
  | Ast.TyList _, _
  | Ast.TyFunction _, _
  | Ast.TyModule _, _ ->
      raise
        (Error.TypeError
           {
             code = Error.E_Type_TypeMismatch;
             msg =
               Printf.sprintf "type mismatch: expected '%s', got '%s'"
                 (Ty_utils.string_of_ty t1) (Ty_utils.string_of_ty t2);
             span;
           })

let rec free_metas ty =
  match resolve ty with
  | Ast.TyMeta { Ast.id; Ast.contents } when !contents = None -> [ id ]
  | Ast.TyMeta _ -> []
  | Ast.TyList t -> free_metas t
  | Ast.TyFunction (ps, r) -> List.concat_map free_metas ps @ free_metas r
  | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit
  | Ast.TyVar _ | Ast.TyModule _ ->
      []

let env_free_metas env = Env.fold (fun _ ty acc -> free_metas ty @ acc) env []

let generalize env ty =
  let env_metas = env_free_metas env in
  let to_gen =
    List.filter (fun id -> not (List.mem id env_metas)) (free_metas ty)
  in
  let mapping =
    List.mapi
      (fun i id ->
        let name = String.make 1 (Char.chr (97 + i)) in
        (id, name))
      to_gen
  in
  let rec go ty =
    match resolve ty with
    | Ast.TyMeta { Ast.id; _ } -> (
        match List.assoc_opt id mapping with
        | Some name -> Ast.TyVar name
        | None -> ty)
    | Ast.TyList t -> Ast.TyList (go t)
    | Ast.TyFunction (ps, r) -> Ast.TyFunction (List.map go ps, go r)
    | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit
    | Ast.TyVar _ | Ast.TyModule _ ->
        ty
  in
  go ty

let instantiate ty =
  let mapping : (string, Ast.ty) Hashtbl.t = Hashtbl.create 4 in
  let rec go (ty : Ast.ty) : Ast.ty =
    match ty with
    | Ast.TyVar name -> (
        match Hashtbl.find_opt mapping name with
        | Some m -> m
        | None ->
            let m : Ast.ty = fresh_meta () in
            Hashtbl.add mapping name m;
            m)
    | Ast.TyList t -> Ast.TyList (go t)
    | Ast.TyFunction (ps, r) -> Ast.TyFunction (List.map go ps, go r)
    | Ast.TyMeta { Ast.contents; _ } -> (
        match !contents with Some t -> go t | None -> ty)
    | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit
    | Ast.TyModule _ ->
        ty
  in
  go ty

let bind_name name ty span env =
  if Env.mem name env then
    raise
      (Error.TypeError
         {
           code = Error.E_Type_AlreadyDefined;
           msg =
             Printf.sprintf "name '%s' is already defined in this scope" name;
           span;
         })
  else Env.add name ty env

let process_imports env imports =
  List.fold_left
    (fun env import ->
      match
        Registry.find_module ~namespace:import.Ast.module_ ~name:import.Ast.item
      with
      | Some _ ->
          bind_name import.Ast.item
            (Ast.TyModule (import.Ast.module_, import.Ast.item))
            import.Ast.import_span env
      | None ->
          raise
            (Error.TypeError
               {
                 code = Error.E_Type_UndefinedVariable;
                 msg =
                   Printf.sprintf "Module '%s:%s' not found" import.Ast.module_
                     import.Ast.item;
                 span = import.Ast.import_span;
               }))
    env imports

let infer_literal = function
  | Ast.LInt _ -> Ast.TyInt
  | Ast.LFloat _ -> Ast.TyFloat
  | Ast.LString _ -> Ast.TyString
  | Ast.LBool _ -> Ast.TyBool
  | Ast.LUnit -> Ast.TyUnit
  | Ast.LNil -> Ast.TyList (fresh_meta ())

let rec infer_expr env expr =
  match expr with
  | Ast.Literal (lit, span) ->
      let ty = infer_literal lit in
      Typed_ast.Literal (lit, resolve ty, span)
  | Ast.Variable (name, span) -> (
      match Env.find name env with
      | Some ty -> Typed_ast.Variable (name, instantiate ty, span)
      | None ->
          raise
            (Error.TypeError
               {
                 code = Error.E_Type_UndefinedVariable;
                 msg = Printf.sprintf "undefined variable '%s'" name;
                 span;
               }))
  | Ast.Lambda lam -> Typed_ast.Lambda (infer_lambda env lam)
  | Ast.Apply (func, args, span) ->
      let typed_func = infer_expr env func in
      let func_ty = Typed_ast.ty_of typed_func in
      let typed_args = List.map (infer_expr env) args in
      let arg_tys = List.map Typed_ast.ty_of typed_args in
      let ret_ty = fresh_meta () in
      (match resolve func_ty with
      | Ast.TyFunction (param_tys, _) ->
          let expected = List.length param_tys in
          let got = List.length arg_tys in
          if expected <> got then
            raise
              (Error.TypeError
                 {
                   code = Error.E_Type_ArityMismatch;
                   msg =
                     Printf.sprintf
                       "arity mismatch: expected %d argument(s), got %d"
                       expected got;
                   span;
                 });
          unify span func_ty (Ast.TyFunction (arg_tys, ret_ty))
      | Ast.TyMeta _ -> unify span func_ty (Ast.TyFunction (arg_tys, ret_ty))
      | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit
      | Ast.TyVar _ | Ast.TyList _ | Ast.TyModule _ ->
          raise
            (Error.TypeError
               {
                 code = Error.E_Type_NotAFunction;
                 msg =
                   Printf.sprintf "expected a function, got '%s'"
                     (Ty_utils.string_of_ty (resolve func_ty));
                 span;
               }));
      Typed_ast.Apply
        (typed_func, typed_args, generalize env (resolve ret_ty), span)
  | Ast.Let { Ast.name; Ast.ty = ann; Ast.let_body; Ast.let_span } ->
      let typed_body =
        match let_body with
        | Ast.Lambda lam when lam.Ast.is_recursive ->
            let param_tys =
              List.map
                (fun p ->
                  match p.Ast.param_ty with
                  | Some t -> t
                  | None -> fresh_meta ())
                lam.Ast.params
            in
            let ret_ty =
              match lam.Ast.ret_ty with Some t -> t | None -> fresh_meta ()
            in
            let pre_env =
              bind_name name (Ast.TyFunction (param_tys, ret_ty)) let_span env
            in
            infer_expr pre_env let_body
        | Ast.Lambda _ | Ast.Literal _ | Ast.Variable _ | Ast.Apply _
        | Ast.Let _ | Ast.If _ | Ast.Match _ | Ast.Block _ | Ast.Binary _
        | Ast.Unary _ | Ast.MemberAccess _ | Ast.Range _ ->
            infer_expr env let_body
      in
      let body_ty = Typed_ast.ty_of typed_body in
      (match ann with
      | Some ann_ty -> unify let_span ann_ty body_ty
      | None -> ());
      let gen_ty = generalize env body_ty in
      Typed_ast.Let
        { Typed_ast.name; let_ty = gen_ty; let_body = typed_body; let_span }
  | Ast.If { Ast.cond; Ast.then_; Ast.else_; Ast.if_span } -> (
      let typed_cond = infer_expr env cond in
      unify if_span (Typed_ast.ty_of typed_cond) Ast.TyBool;
      let typed_then = infer_expr env then_ in
      let then_ty = Typed_ast.ty_of typed_then in
      match else_ with
      | None ->
          raise
            (Error.TypeError
               {
                 code = Error.E_Type_MissingElseBranch;
                 msg = "if expression is missing an else branch";
                 span = if_span;
               })
      | Some else_expr ->
          let typed_else = infer_expr env else_expr in
          let else_ty = Typed_ast.ty_of typed_else in
          (try unify if_span then_ty else_ty
           with Error.TypeError _ ->
             raise
               (Error.TypeError
                  {
                    code = Error.E_Type_IfBranchMismatch;
                    msg =
                      Printf.sprintf
                        "if branches have different types: then is '%s', else \
                         is '%s'"
                        (Ty_utils.string_of_ty (resolve then_ty))
                        (Ty_utils.string_of_ty (resolve else_ty));
                    span = if_span;
                  }));
          Typed_ast.If
            {
              Typed_ast.cond = typed_cond;
              Typed_ast.then_ = typed_then;
              Typed_ast.else_ = Some typed_else;
              Typed_ast.if_ty = resolve then_ty;
              Typed_ast.if_span;
            })
  | Ast.Match { Ast.scrutinee; Ast.cases; Ast.match_span } ->
      let typed_scrutinee = infer_expr env scrutinee in
      let scrutinee_ty = Typed_ast.ty_of typed_scrutinee in
      let typed_cases, result_ty =
        infer_match env scrutinee_ty cases match_span
      in
      Typed_ast.Match
        {
          Typed_ast.scrutinee = typed_scrutinee;
          Typed_ast.cases = typed_cases;
          Typed_ast.match_ty = result_ty;
          Typed_ast.match_span;
        }
  | Ast.Block (exprs, span) ->
      let typed_exprs, ty = infer_block env exprs span in
      Typed_ast.Block (typed_exprs, generalize env (resolve ty), span)
  | Ast.Binary { Ast.binary_op; Ast.left; Ast.right; Ast.binary_span } ->
      infer_binary env binary_op left right binary_span
  | Ast.Unary { unary_op = Ast.OpNegate; Ast.expr = e; Ast.unary_span } ->
      let typed_e = infer_expr env e in
      let ty = Typed_ast.ty_of typed_e in
      (try unify unary_span ty Ast.TyInt
       with _ -> (
         try unify unary_span ty Ast.TyFloat
         with _ ->
           raise
             (Error.TypeError
                {
                  code = Error.E_Type_TypeMismatch;
                  msg =
                    Printf.sprintf "type mismatch: expected 'int', got '%s'"
                      (Ty_utils.string_of_ty ty);
                  span = unary_span;
                })));
      Typed_ast.Unary
        {
          Typed_ast.unary_op = Ast.OpNegate;
          Typed_ast.expr = typed_e;
          Typed_ast.unary_ty = generalize env (resolve ty);
          Typed_ast.unary_span;
        }
  | Ast.MemberAccess (obj, member, span) -> (
      let typed_obj = infer_expr env obj in
      let obj_ty = Typed_ast.ty_of typed_obj in
      match resolve obj_ty with
      | Ast.TyModule (namespace, name) -> (
          match Registry.find_module ~namespace ~name with
          | Some module_info -> (
              match List.assoc_opt member module_info.Registry.exports with
              | Some member_ty ->
                  let ty = instantiate member_ty in
                  Typed_ast.MemberAccess (typed_obj, member, ty, span)
              | None ->
                  raise
                    (Error.TypeError
                       {
                         code = Error.E_Type_UndefinedVariable;
                         msg =
                           Printf.sprintf "undefined variable '%s.%s'" name
                             member;
                         span;
                       }))
          | None ->
              raise
                (Error.TypeError
                   {
                     code = Error.E_Type_UndefinedVariable;
                     msg =
                       Printf.sprintf "undefined variable '%s:%s'" namespace
                         name;
                     span;
                   }))
      | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit
      | Ast.TyVar _ | Ast.TyMeta _ | Ast.TyList _ | Ast.TyFunction _ ->
          raise
            (Error.TypeError
               {
                 code = Error.E_Type_TypeMismatch;
                 msg =
                   Printf.sprintf "type mismatch: expected 'module', got '%s'"
                     (Ty_utils.string_of_ty obj_ty);
                 span = Ast.span_of_expr obj;
               }))
  | Ast.Range (start, stop, span) ->
      let typed_start = infer_expr env start in
      let typed_stop = infer_expr env stop in
      unify span (Typed_ast.ty_of typed_start) Ast.TyInt;
      unify span (Typed_ast.ty_of typed_stop) Ast.TyInt;
      Typed_ast.Range (typed_start, typed_stop, span)

and infer_lambda env lam =
  let param_tys =
    List.map
      (fun param ->
        match param.Ast.param_ty with Some ty -> ty | None -> fresh_meta ())
      lam.Ast.params
  in
  let body_env =
    List.fold_left2
      (fun env' param ty ->
        bind_name param.Ast.param_name ty param.Ast.param_span env')
      env lam.Ast.params param_tys
  in
  let typed_body = infer_expr body_env lam.Ast.lambda_body in
  let body_ty = Typed_ast.ty_of typed_body in
  (match lam.Ast.ret_ty with
  | Some ann_ret ->
      let body_span =
        match lam.Ast.lambda_body with
        | Ast.Block ((_ :: _ as exprs), _) ->
            Ast.span_of_expr (List.nth exprs (List.length exprs - 1))
        | Ast.Block ([], _)
        | Ast.Literal _ | Ast.Variable _ | Ast.Lambda _ | Ast.Apply _
        | Ast.Let _ | Ast.If _ | Ast.Match _ | Ast.Binary _ | Ast.Unary _
        | Ast.MemberAccess _ | Ast.Range _ ->
            Ast.span_of_expr lam.Ast.lambda_body
      in
      unify body_span ann_ret body_ty
  | None -> ());
  let gen = generalize env in
  let resolved_param_tys = List.map (fun t -> gen (resolve t)) param_tys in
  let typed_params =
    List.map2
      (fun (p : Ast.param) ty ->
        {
          Typed_ast.param_name = p.Ast.param_name;
          param_ty = ty;
          param_span = p.Ast.param_span;
        })
      lam.Ast.params resolved_param_tys
  in
  let ret_ty = gen (resolve body_ty) in
  let lambda_ty =
    gen (Ast.TyFunction (List.map resolve param_tys, resolve body_ty))
  in
  {
    Typed_ast.params = typed_params;
    Typed_ast.ret_ty;
    Typed_ast.lambda_body = typed_body;
    Typed_ast.is_recursive = lam.Ast.is_recursive;
    Typed_ast.lambda_ty;
    Typed_ast.lambda_span = lam.Ast.lambda_span;
  }

and infer_match env scrutinee_ty cases _span =
  match cases with
  | [] -> ([], Ast.TyUnit)
  | first :: rest ->
      let env' = infer_pattern env scrutinee_ty first.Ast.pattern in
      let env'' =
        match first.Ast.guard with
        | None -> env'
        | Some g ->
            let typed_g = infer_expr env' g in
            unify
              (Typed_ast.span_of typed_g)
              (Typed_ast.ty_of typed_g) Ast.TyBool;
            env'
      in
      let typed_first_guard =
        match first.Ast.guard with
        | None -> None
        | Some g -> Some (infer_expr env' g)
      in
      let typed_first_body = infer_expr env'' first.Ast.case_body in
      let first_ty = Typed_ast.ty_of typed_first_body in
      let typed_first =
        {
          Typed_ast.pattern = first.Ast.pattern;
          guard = typed_first_guard;
          case_body = typed_first_body;
          case_span = first.Ast.case_span;
        }
      in
      let typed_rest =
        List.map
          (fun case ->
            let env' = infer_pattern env scrutinee_ty case.Ast.pattern in
            let typed_guard =
              match case.Ast.guard with
              | None -> None
              | Some g ->
                  let typed_g = infer_expr env' g in
                  unify
                    (Typed_ast.span_of typed_g)
                    (Typed_ast.ty_of typed_g) Ast.TyBool;
                  Some typed_g
            in
            let env'' =
              match case.Ast.guard with
              | None -> env'
              | Some g ->
                  let _ = infer_expr env' g in
                  env'
            in
            let typed_body = infer_expr env'' case.Ast.case_body in
            let case_ty = Typed_ast.ty_of typed_body in
            (try unify case.Ast.case_span first_ty case_ty
             with Error.TypeError _ ->
               raise
                 (Error.TypeError
                    {
                      code = Error.E_Type_MatchBranchMismatch;
                      msg =
                        Printf.sprintf
                          "match branch type mismatch: expected '%s', got '%s'"
                          (Ty_utils.string_of_ty (resolve first_ty))
                          (Ty_utils.string_of_ty (resolve case_ty));
                      span = case.Ast.case_span;
                    }));
            {
              Typed_ast.pattern = case.Ast.pattern;
              guard = typed_guard;
              case_body = typed_body;
              case_span = case.Ast.case_span;
            })
          rest
      in
      (typed_first :: typed_rest, resolve first_ty)

and infer_pattern env expected pat =
  match pat with
  | Ast.PWildcard _ -> env
  | Ast.PVariable (name, span) -> bind_name name expected span env
  | Ast.PIntLiteral (_, span) -> (
      match resolve expected with
      | Ast.TyInt -> env
      | Ast.TyMeta _ ->
          unify span expected Ast.TyInt;
          env
      | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit | Ast.TyVar _
      | Ast.TyModule _ | Ast.TyList _ | Ast.TyFunction _ ->
          raise
            (Error.TypeError
               {
                 code = Error.E_Type_PatternTypeMismatch;
                 msg =
                   Printf.sprintf
                     "pattern type mismatch: expected '%s', got 'int'"
                     (Ty_utils.string_of_ty expected);
                 span;
               }))
  | Ast.PFloatLiteral (_, span) -> (
      match resolve expected with
      | Ast.TyFloat -> env
      | Ast.TyMeta _ ->
          unify span expected Ast.TyFloat;
          env
      | Ast.TyInt | Ast.TyString | Ast.TyBool | Ast.TyUnit | Ast.TyVar _
      | Ast.TyModule _ | Ast.TyList _ | Ast.TyFunction _ ->
          raise
            (Error.TypeError
               {
                 code = Error.E_Type_PatternTypeMismatch;
                 msg =
                   Printf.sprintf
                     "pattern type mismatch: expected '%s', got 'float'"
                     (Ty_utils.string_of_ty expected);
                 span;
               }))
  | Ast.PStringLiteral (_, span) -> (
      match resolve expected with
      | Ast.TyString -> env
      | Ast.TyMeta _ ->
          unify span expected Ast.TyString;
          env
      | Ast.TyInt | Ast.TyFloat | Ast.TyBool | Ast.TyUnit | Ast.TyVar _
      | Ast.TyModule _ | Ast.TyList _ | Ast.TyFunction _ ->
          raise
            (Error.TypeError
               {
                 code = Error.E_Type_PatternTypeMismatch;
                 msg =
                   Printf.sprintf
                     "pattern type mismatch: expected '%s', got 'string'"
                     (Ty_utils.string_of_ty expected);
                 span;
               }))
  | Ast.PBooleanLiteral (_, span) -> (
      match resolve expected with
      | Ast.TyBool -> env
      | Ast.TyMeta _ ->
          unify span expected Ast.TyBool;
          env
      | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyUnit | Ast.TyVar _
      | Ast.TyModule _ | Ast.TyList _ | Ast.TyFunction _ ->
          raise
            (Error.TypeError
               {
                 code = Error.E_Type_PatternTypeMismatch;
                 msg =
                   Printf.sprintf
                     "pattern type mismatch: expected '%s', got 'bool'"
                     (Ty_utils.string_of_ty expected);
                 span;
               }))
  | Ast.PNil span -> (
      match resolve expected with
      | Ast.TyList _ -> env
      | Ast.TyMeta _ ->
          let elem = fresh_meta () in
          unify span expected (Ast.TyList elem);
          env
      | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit
      | Ast.TyVar _ | Ast.TyModule _ | Ast.TyFunction _ ->
          raise
            (Error.TypeError
               {
                 code = Error.E_Type_PatternTypeMismatch;
                 msg =
                   Printf.sprintf
                     "pattern type mismatch: expected '%s', got '_ list'"
                     (Ty_utils.string_of_ty expected);
                 span;
               }))
  | Ast.PCons (hd, tl, span) -> (
      match resolve expected with
      | Ast.TyList elem_ty ->
          let env' = infer_pattern env elem_ty hd in
          infer_pattern env' (Ast.TyList elem_ty) tl
      | Ast.TyMeta _ ->
          let elem = fresh_meta () in
          unify span expected (Ast.TyList elem);
          let env' = infer_pattern env elem hd in
          infer_pattern env' (Ast.TyList elem) tl
      | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit
      | Ast.TyVar _ | Ast.TyModule _ | Ast.TyFunction _ ->
          raise
            (Error.TypeError
               {
                 code = Error.E_Type_PatternTypeMismatch;
                 msg =
                   Printf.sprintf
                     "pattern type mismatch: expected '%s', got '_ list'"
                     (Ty_utils.string_of_ty expected);
                 span;
               }))

and infer_block env exprs _span =
  let rec go env acc = function
    | [] -> (List.rev acc, Ast.TyUnit)
    | [ e ] ->
        let typed_e = infer_expr env e in
        let ty = generalize env (resolve (Typed_ast.ty_of typed_e)) in
        (List.rev (typed_e :: acc), ty)
    | Ast.Let { Ast.name; Ast.ty = ann; Ast.let_body; Ast.let_span } :: rest ->
        let pre_ty = fresh_meta () in
        let pre_env = bind_name name pre_ty let_span env in
        let typed_body = infer_expr pre_env let_body in
        let body_ty = Typed_ast.ty_of typed_body in
        unify let_span pre_ty body_ty;
        (match ann with
        | Some ann_ty -> unify let_span ann_ty body_ty
        | None -> ());
        let gen_ty = generalize env body_ty in
        let env' = bind_name name gen_ty let_span env in
        let typed_let =
          Typed_ast.Let
            {
              Typed_ast.name;
              Typed_ast.let_ty = gen_ty;
              Typed_ast.let_body = typed_body;
              Typed_ast.let_span;
            }
        in
        go env' (typed_let :: acc) rest
    | (( Ast.Literal _ | Ast.Variable _ | Ast.Lambda _ | Ast.Apply _ | Ast.If _
       | Ast.Match _ | Ast.Block _ | Ast.Binary _ | Ast.Unary _
       | Ast.MemberAccess _ | Ast.Range _ ) as e)
      :: rest ->
        let typed_e = infer_expr env e in
        go env (typed_e :: acc) rest
  in
  go env [] exprs

and infer_binary env op left right span : Typed_ast.expr =
  let typed_left = infer_expr env left in
  let typed_right = infer_expr env right in
  let left_ty = Typed_ast.ty_of typed_left in
  let right_ty = Typed_ast.ty_of typed_right in
  let ty =
    match op with
    | Ast.OpAdd | Ast.OpSub | Ast.OpMul | Ast.OpDiv -> (
        match (resolve left_ty, resolve right_ty) with
        | Ast.TyInt, Ast.TyInt -> Ast.TyInt
        | Ast.TyFloat, Ast.TyFloat -> Ast.TyFloat
        | Ast.TyInt, Ast.TyMeta _ ->
            unify (Typed_ast.span_of typed_right) right_ty Ast.TyInt;
            Ast.TyInt
        | Ast.TyFloat, Ast.TyMeta _ ->
            unify (Typed_ast.span_of typed_right) right_ty Ast.TyFloat;
            Ast.TyFloat
        | Ast.TyMeta _, Ast.TyInt ->
            unify (Typed_ast.span_of typed_left) left_ty Ast.TyInt;
            Ast.TyInt
        | Ast.TyMeta _, Ast.TyFloat ->
            unify (Typed_ast.span_of typed_left) left_ty Ast.TyFloat;
            Ast.TyFloat
        | Ast.TyMeta _, Ast.TyMeta _ ->
            unify (Typed_ast.span_of typed_left) left_ty Ast.TyInt;
            unify (Typed_ast.span_of typed_right) right_ty Ast.TyInt;
            Ast.TyInt
        | Ast.TyInt, got ->
            raise
              (Error.TypeError
                 {
                   code = Error.E_Type_TypeMismatch;
                   msg =
                     Printf.sprintf "type mismatch: expected 'int', got '%s'"
                       (Ty_utils.string_of_ty got);
                   span = Typed_ast.span_of typed_right;
                 })
        | Ast.TyFloat, got ->
            raise
              (Error.TypeError
                 {
                   code = Error.E_Type_TypeMismatch;
                   msg =
                     Printf.sprintf "type mismatch: expected 'float', got '%s'"
                       (Ty_utils.string_of_ty got);
                   span = Typed_ast.span_of typed_right;
                 })
        | Ast.TyMeta _, got ->
            raise
              (Error.TypeError
                 {
                   code = Error.E_Type_TypeMismatch;
                   msg =
                     Printf.sprintf
                       "type mismatch: expected 'int or float', got '%s'"
                       (Ty_utils.string_of_ty got);
                   span = Typed_ast.span_of typed_right;
                 })
        | Ast.TyString, _
        | Ast.TyBool, _
        | Ast.TyUnit, _
        | Ast.TyVar _, _
        | Ast.TyModule _, _
        | Ast.TyList _, _
        | Ast.TyFunction _, _ ->
            raise
              (Error.TypeError
                 {
                   code = Error.E_Type_TypeMismatch;
                   msg =
                     Printf.sprintf
                       "type mismatch: expected 'int or float', got '%s'"
                       (Ty_utils.string_of_ty (resolve left_ty));
                   span = Typed_ast.span_of typed_left;
                 }))
    | Ast.OpEqual | Ast.OpNotEqual ->
        (try unify (Typed_ast.span_of typed_right) left_ty right_ty
         with Error.TypeError _ ->
           raise
             (Error.TypeError
                {
                  code = Error.E_Type_TypeMismatch;
                  msg =
                    Printf.sprintf "type mismatch: expected '%s', got '%s'"
                      (Ty_utils.string_of_ty (resolve left_ty))
                      (Ty_utils.string_of_ty (resolve right_ty));
                  span = Typed_ast.span_of typed_right;
                }));
        Ast.TyBool
    | Ast.OpLess | Ast.OpGreater -> (
        match (resolve left_ty, resolve right_ty) with
        | Ast.TyInt, Ast.TyInt -> Ast.TyBool
        | Ast.TyFloat, Ast.TyFloat -> Ast.TyBool
        | Ast.TyInt, Ast.TyMeta _ ->
            unify (Typed_ast.span_of typed_right) right_ty Ast.TyInt;
            Ast.TyBool
        | Ast.TyFloat, Ast.TyMeta _ ->
            unify (Typed_ast.span_of typed_right) right_ty Ast.TyFloat;
            Ast.TyBool
        | Ast.TyMeta _, Ast.TyInt ->
            unify (Typed_ast.span_of typed_left) left_ty Ast.TyInt;
            Ast.TyBool
        | Ast.TyMeta _, Ast.TyFloat ->
            unify (Typed_ast.span_of typed_left) left_ty Ast.TyFloat;
            Ast.TyBool
        | Ast.TyMeta _, Ast.TyMeta _ ->
            unify (Typed_ast.span_of typed_left) left_ty Ast.TyInt;
            unify (Typed_ast.span_of typed_right) right_ty Ast.TyInt;
            Ast.TyBool
        | Ast.TyInt, got ->
            raise
              (Error.TypeError
                 {
                   code = Error.E_Type_TypeMismatch;
                   msg =
                     Printf.sprintf "type mismatch: expected 'int', got '%s'"
                       (Ty_utils.string_of_ty got);
                   span = Typed_ast.span_of typed_right;
                 })
        | Ast.TyFloat, got ->
            raise
              (Error.TypeError
                 {
                   code = Error.E_Type_TypeMismatch;
                   msg =
                     Printf.sprintf "type mismatch: expected 'float', got '%s'"
                       (Ty_utils.string_of_ty got);
                   span = Typed_ast.span_of typed_right;
                 })
        | Ast.TyMeta _, got ->
            raise
              (Error.TypeError
                 {
                   code = Error.E_Type_TypeMismatch;
                   msg =
                     Printf.sprintf
                       "type mismatch: expected 'int or float', got '%s'"
                       (Ty_utils.string_of_ty got);
                   span = Typed_ast.span_of typed_right;
                 })
        | Ast.TyString, _
        | Ast.TyBool, _
        | Ast.TyUnit, _
        | Ast.TyVar _, _
        | Ast.TyModule _, _
        | Ast.TyList _, _
        | Ast.TyFunction _, _ ->
            raise
              (Error.TypeError
                 {
                   code = Error.E_Type_TypeMismatch;
                   msg =
                     Printf.sprintf
                       "type mismatch: expected 'int or float', got '%s'"
                       (Ty_utils.string_of_ty (resolve left_ty));
                   span = Typed_ast.span_of typed_left;
                 }))
    | Ast.OpCons ->
        let elem = fresh_meta () in
        unify (Typed_ast.span_of typed_left) left_ty elem;
        unify (Typed_ast.span_of typed_right) right_ty (Ast.TyList elem);
        Ast.TyList elem
  in
  Typed_ast.Binary
    {
      Typed_ast.binary_op = op;
      Typed_ast.left = typed_left;
      Typed_ast.right = typed_right;
      Typed_ast.binary_ty = generalize env (resolve ty);
      Typed_ast.binary_span = span;
    }

let infer_program (program : Ast.program) : Typed_ast.program =
  let env = process_imports Env.empty program.Ast.imports in
  let rec go env acc = function
    | [] -> List.rev acc
    | Ast.Let { Ast.name; Ast.ty = ann; Ast.let_body; Ast.let_span } :: rest ->
        let pre_ty = fresh_meta () in
        let pre_env = bind_name name pre_ty let_span env in
        let typed_body = infer_expr pre_env let_body in
        let body_ty = Typed_ast.ty_of typed_body in
        unify let_span pre_ty body_ty;
        (match ann with
        | Some ann_ty -> unify let_span ann_ty body_ty
        | None -> ());
        let gen_ty = generalize env body_ty in
        let env' = bind_name name gen_ty let_span env in
        let typed_let =
          Typed_ast.Let
            {
              Typed_ast.name;
              Typed_ast.let_ty = gen_ty;
              Typed_ast.let_body = typed_body;
              Typed_ast.let_span;
            }
        in
        go env' (typed_let :: acc) rest
    | (( Ast.Literal _ | Ast.Variable _ | Ast.Lambda _ | Ast.Apply _ | Ast.If _
       | Ast.Match _ | Ast.Block _ | Ast.Binary _ | Ast.Unary _
       | Ast.MemberAccess _ | Ast.Range _ ) as e)
      :: rest ->
        let typed_e = infer_expr env e in
        go env (typed_e :: acc) rest
  in
  { Typed_ast.imports = program.Ast.imports; body = go env [] program.Ast.body }

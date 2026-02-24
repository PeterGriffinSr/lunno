[@@@warning "-45"]

open Lunno_common.Ast
open Lunno_common.Error
open Lunno_modules

let next_id = ref 0

let fresh_meta () =
  let id = !next_id in
  incr next_id;
  TyMeta { id; contents = ref None }

let rec resolve ty =
  match ty with
  | TyMeta { contents; _ } -> (
      match !contents with Some t -> resolve t | None -> ty)
  | TyInt | TyFloat | TyString | TyBool | TyUnit | TyList _ | TyFunction _
  | TyVar _ | TyModule _ ->
      ty

let rec occurs id ty =
  match resolve ty with
  | TyMeta { id = id2; _ } -> id = id2
  | TyList t -> occurs id t
  | TyFunction (params, ret) -> List.exists (occurs id) params || occurs id ret
  | TyInt | TyFloat | TyString | TyBool | TyUnit | TyVar _ | TyModule _ -> false

let rec unify span t1 t2 =
  let t1 = resolve t1 and t2 = resolve t2 in
  match (t1, t2) with
  | TyInt, TyInt
  | TyFloat, TyFloat
  | TyString, TyString
  | TyBool, TyBool
  | TyUnit, TyUnit ->
      ()
  | TyVar a, TyVar b when a = b -> ()
  | TyList a, TyList b -> unify span a b
  | TyFunction (ps1, r1), TyFunction (ps2, r2) ->
      if List.length ps1 <> List.length ps2 then
        raise
          (type_mismatch (Debug.string_of_ty t1) (Debug.string_of_ty t2) span);
      List.iter2 (unify span) ps1 ps2;
      unify span r1 r2
  | TyMeta m, t | t, TyMeta m -> (
      match !(m.contents) with
      | Some _ -> assert false
      | None ->
          if occurs m.id t then
            match t with
            | TyMeta m2 when m.id = m2.id -> ()
            | TyInt | TyFloat | TyString | TyBool | TyUnit | TyVar _ | TyList _
            | TyFunction _ | TyModule _ | TyMeta _ ->
                raise
                  (type_mismatch (Debug.string_of_ty t1) (Debug.string_of_ty t2)
                     span)
          else m.contents := Some t)
  | TyInt, _
  | TyFloat, _
  | TyString, _
  | TyBool, _
  | TyUnit, _
  | TyVar _, _
  | TyList _, _
  | TyFunction _, _
  | TyModule _, _ ->
      raise (type_mismatch (Debug.string_of_ty t1) (Debug.string_of_ty t2) span)

let rec free_metas ty =
  match resolve ty with
  | TyMeta { id; contents } when !contents = None -> [ id ]
  | TyMeta _ -> []
  | TyList t -> free_metas t
  | TyFunction (ps, r) -> List.concat_map free_metas ps @ free_metas r
  | TyInt | TyFloat | TyString | TyBool | TyUnit | TyVar _ | TyModule _ -> []

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
    | TyMeta { id; _ } -> (
        match List.assoc_opt id mapping with
        | Some name -> TyVar name
        | None -> ty)
    | TyList t -> TyList (go t)
    | TyFunction (ps, r) -> TyFunction (List.map go ps, go r)
    | TyInt | TyFloat | TyString | TyBool | TyUnit | TyVar _ | TyModule _ -> ty
  in
  go ty

let instantiate (ty : ty) : ty =
  let mapping : (string, ty) Hashtbl.t = Hashtbl.create 4 in
  let rec go (ty : ty) : ty =
    match ty with
    | TyVar name -> (
        match Hashtbl.find_opt mapping name with
        | Some m -> m
        | None ->
            let m : ty = fresh_meta () in
            Hashtbl.add mapping name m;
            m)
    | TyList t -> TyList (go t)
    | TyFunction (ps, r) -> TyFunction (List.map go ps, go r)
    | TyMeta { contents; _ } -> (
        match !contents with Some t -> go t | None -> ty)
    | TyInt | TyFloat | TyString | TyBool | TyUnit | TyModule _ -> ty
  in
  go ty

let bind_name name ty span env =
  if Env.mem name env then raise (already_defined name span)
  else Env.add name ty env

let process_imports env imports =
  List.fold_left
    (fun env import ->
      match
        Registry.find_module ~namespace:import.module_ ~name:import.item
      with
      | Some _ ->
          bind_name import.item
            (TyModule (import.module_, import.item))
            import.import_span env
      | None ->
          raise
            (undefined_variable
               (Printf.sprintf "Module '%s:%s' not found" import.module_
                  import.item)
               import.import_span))
    env imports

let infer_literal = function
  | LInt _ -> TyInt
  | LFloat _ -> TyFloat
  | LString _ -> TyString
  | LBool _ -> TyBool
  | LUnit -> TyUnit
  | LNil -> TyList (fresh_meta ())

let rec infer_expr env expr =
  match expr with
  | Literal (lit, _) -> infer_literal lit
  | Variable (name, span) -> (
      match Env.find name env with
      | Some ty -> instantiate ty
      | None -> raise (undefined_variable name span))
  | Lambda lam -> infer_lambda env lam
  | Apply (func, args, span) ->
      let func_ty = infer_expr env func in
      let arg_tys = List.map (infer_expr env) args in
      let ret_ty = fresh_meta () in
      (match resolve func_ty with
      | TyFunction (param_tys, _) ->
          let expected = List.length param_tys in
          let got = List.length arg_tys in
          if expected <> got then raise (arity_mismatch expected got span);
          unify span func_ty (TyFunction (arg_tys, ret_ty))
      | TyMeta _ -> unify span func_ty (TyFunction (arg_tys, ret_ty))
      | TyInt | TyFloat | TyString | TyBool | TyUnit | TyVar _ | TyList _
      | TyModule _ ->
          raise (not_a_function (Debug.string_of_ty (resolve func_ty)) span));
      resolve ret_ty
  | Let { name; ty = ann; let_body; let_span } ->
      let body_ty =
        match let_body with
        | Lambda lam when lam.is_recursive ->
            let param_tys =
              List.map
                (fun p ->
                  match p.param_ty with Some t -> t | None -> fresh_meta ())
                lam.params
            in
            let ret_ty =
              match lam.ret_ty with Some t -> t | None -> fresh_meta ()
            in
            let pre_env =
              bind_name name (TyFunction (param_tys, ret_ty)) let_span env
            in
            infer_expr pre_env let_body
        | Lambda _ | Literal _ | Variable _ | Apply _ | Let _ | If _ | Match _
        | Block _ | Binary _ | Unary _ | MemberAccess _ ->
            infer_expr env let_body
      in
      (match ann with
      | Some ann_ty -> unify let_span ann_ty body_ty
      | None -> ());
      body_ty
  | If { cond; then_; else_; if_span } -> (
      let cond_ty = infer_expr env cond in
      unify if_span cond_ty TyBool;
      let then_ty = infer_expr env then_ in
      match else_ with
      | None -> raise (missing_else_branch if_span)
      | Some else_expr ->
          let else_ty = infer_expr env else_expr in
          (try unify if_span then_ty else_ty
           with TypeError _ ->
             raise
               (if_branch_mismatch
                  (Debug.string_of_ty (resolve then_ty))
                  (Debug.string_of_ty (resolve else_ty))
                  if_span));
          resolve then_ty)
  | Match { scrutinee; cases; match_span } ->
      let scrutinee_ty = infer_expr env scrutinee in
      infer_match env scrutinee_ty cases match_span
  | Block (exprs, span) -> infer_block env exprs span
  | Binary { binary_op; left; right; binary_span } ->
      infer_binary env binary_op left right binary_span
  | Unary { unary_op = OpNegate; expr = e; unary_span } ->
      let ty = infer_expr env e in
      (try unify unary_span ty TyInt
       with _ -> (
         try unify unary_span ty TyFloat
         with _ ->
           raise (type_mismatch "int" (Debug.string_of_ty ty) unary_span)));
      resolve ty
  | MemberAccess (obj, member, span) -> (
      let obj_ty = infer_expr env obj in
      match resolve obj_ty with
      | TyModule (namespace, name) -> (
          match Registry.find_module ~namespace ~name with
          | Some module_info -> (
              match List.assoc_opt member module_info.Registry.exports with
              | Some member_ty -> instantiate member_ty
              | None -> raise (undefined_variable (name ^ "." ^ member) span))
          | None -> raise (undefined_variable (namespace ^ ":" ^ name) span))
      | TyInt | TyFloat | TyString | TyBool | TyUnit | TyVar _ | TyMeta _
      | TyList _ | TyFunction _ ->
          raise
            (type_mismatch "module"
               (Debug.string_of_ty obj_ty)
               (span_of_expr obj)))

and infer_lambda env lam =
  let param_tys =
    List.map
      (fun param ->
        match param.param_ty with Some ty -> ty | None -> fresh_meta ())
      lam.params
  in
  let body_env =
    List.fold_left2
      (fun env' param ty -> bind_name param.param_name ty param.param_span env')
      env lam.params param_tys
  in
  let body_ty = infer_expr body_env lam.lambda_body in
  (match lam.ret_ty with
  | Some ann_ret ->
      let body_span =
        match lam.lambda_body with
        | Block ((_ :: _ as exprs), _) ->
            span_of_expr (List.nth exprs (List.length exprs - 1))
        | Block ([], _)
        | Literal _ | Variable _ | Lambda _ | Apply _ | Let _ | If _ | Match _
        | Binary _ | Unary _ | MemberAccess _ ->
            span_of_expr lam.lambda_body
      in
      unify body_span ann_ret body_ty
  | None -> ());
  TyFunction (param_tys, resolve body_ty)

and infer_match env scrutinee_ty cases _span =
  match cases with
  | [] -> TyUnit
  | first :: rest ->
      let env' = infer_pattern env scrutinee_ty first.pattern in
      let env'' =
        match first.guard with
        | None -> env'
        | Some g ->
            let g_ty = infer_expr env' g in
            unify (span_of_expr g) g_ty TyBool;
            env'
      in
      let first_ty = infer_expr env'' first.case_body in
      List.iter
        (fun case ->
          let env' = infer_pattern env scrutinee_ty case.pattern in
          let env'' =
            match case.guard with
            | None -> env'
            | Some g ->
                let g_ty = infer_expr env' g in
                unify (span_of_expr g) g_ty TyBool;
                env'
          in
          let case_ty = infer_expr env'' case.case_body in
          try unify case.case_span first_ty case_ty
          with TypeError _ ->
            raise
              (match_branch_mismatch
                 (Debug.string_of_ty (resolve first_ty))
                 (Debug.string_of_ty (resolve case_ty))
                 case.case_span))
        rest;
      resolve first_ty

and infer_pattern env expected pat =
  match pat with
  | PWildcard _ -> env
  | PVariable (name, span) -> bind_name name expected span env
  | PIntLiteral (_, span) -> (
      match resolve expected with
      | TyInt -> env
      | TyMeta _ ->
          unify span expected TyInt;
          env
      | TyFloat | TyString | TyBool | TyUnit | TyVar _ | TyModule _ | TyList _
      | TyFunction _ ->
          raise
            (pattern_type_mismatch
               (Debug.string_of_ty expected)
               (Debug.string_of_ty TyInt) span))
  | PFloatLiteral (_, span) -> (
      match resolve expected with
      | TyFloat -> env
      | TyMeta _ ->
          unify span expected TyFloat;
          env
      | TyInt | TyString | TyBool | TyUnit | TyVar _ | TyModule _ | TyList _
      | TyFunction _ ->
          raise
            (pattern_type_mismatch
               (Debug.string_of_ty expected)
               (Debug.string_of_ty TyFloat)
               span))
  | PStringLiteral (_, span) -> (
      match resolve expected with
      | TyString -> env
      | TyMeta _ ->
          unify span expected TyString;
          env
      | TyInt | TyFloat | TyBool | TyUnit | TyVar _ | TyModule _ | TyList _
      | TyFunction _ ->
          raise
            (pattern_type_mismatch
               (Debug.string_of_ty expected)
               (Debug.string_of_ty TyString)
               span))
  | PBooleanLiteral (_, span) -> (
      match resolve expected with
      | TyBool -> env
      | TyMeta _ ->
          unify span expected TyBool;
          env
      | TyInt | TyFloat | TyString | TyUnit | TyVar _ | TyModule _ | TyList _
      | TyFunction _ ->
          raise
            (pattern_type_mismatch
               (Debug.string_of_ty expected)
               (Debug.string_of_ty TyBool)
               span))
  | PNil span -> (
      match resolve expected with
      | TyList _ -> env
      | TyMeta _ ->
          let elem = fresh_meta () in
          unify span expected (TyList elem);
          env
      | TyInt | TyFloat | TyString | TyBool | TyUnit | TyVar _ | TyModule _
      | TyFunction _ ->
          raise (pattern_type_mismatch (Debug.string_of_ty expected) "[_]" span)
      )
  | PCons (hd, tl, span) -> (
      match resolve expected with
      | TyList elem_ty ->
          let env' = infer_pattern env elem_ty hd in
          infer_pattern env' (TyList elem_ty) tl
      | TyMeta _ ->
          let elem = fresh_meta () in
          unify span expected (TyList elem);
          let env' = infer_pattern env elem hd in
          infer_pattern env' (TyList elem) tl
      | TyInt | TyFloat | TyString | TyBool | TyUnit | TyVar _ | TyModule _
      | TyFunction _ ->
          raise (pattern_type_mismatch (Debug.string_of_ty expected) "[_]" span)
      )

and infer_block env exprs _span =
  let rec go env = function
    | [] -> TyUnit
    | [ e ] -> infer_expr env e
    | Let { name; ty = ann; let_body; let_span } :: rest ->
        let pre_ty = fresh_meta () in
        let pre_env = bind_name name pre_ty let_span env in
        let body_ty = infer_expr pre_env let_body in
        unify let_span pre_ty body_ty;
        (match ann with
        | Some ann_ty -> unify let_span ann_ty body_ty
        | None -> ());
        let gen_ty = generalize env body_ty in
        let env' = bind_name name gen_ty let_span env in
        go env' rest
    | (( Literal _ | Variable _ | Lambda _ | Apply _ | If _ | Match _ | Block _
       | Binary _ | Unary _ | MemberAccess _ ) as e)
      :: rest ->
        let _ = infer_expr env e in
        go env rest
  in
  go env exprs

and infer_binary env op left right _span =
  let left_ty = infer_expr env left in
  let right_ty = infer_expr env right in
  match op with
  | OpAdd | OpSub | OpMul | OpDiv -> (
      match (resolve left_ty, resolve right_ty) with
      | TyInt, TyInt -> TyInt
      | TyFloat, TyFloat -> TyFloat
      | TyInt, TyMeta _ ->
          unify (span_of_expr right) right_ty TyInt;
          TyInt
      | TyFloat, TyMeta _ ->
          unify (span_of_expr right) right_ty TyFloat;
          TyFloat
      | TyMeta _, TyInt ->
          unify (span_of_expr left) left_ty TyInt;
          TyInt
      | TyMeta _, TyFloat ->
          unify (span_of_expr left) left_ty TyFloat;
          TyFloat
      | TyMeta _, TyMeta _ ->
          unify (span_of_expr left) left_ty TyInt;
          unify (span_of_expr right) right_ty TyInt;
          TyInt
      | TyInt, got ->
          raise
            (type_mismatch "int" (Debug.string_of_ty got) (span_of_expr right))
      | TyFloat, got ->
          raise
            (type_mismatch "float" (Debug.string_of_ty got) (span_of_expr right))
      | TyMeta _, got ->
          raise
            (type_mismatch "int or float" (Debug.string_of_ty got)
               (span_of_expr right))
      | TyString, _
      | TyBool, _
      | TyUnit, _
      | TyVar _, _
      | TyModule _, _
      | TyList _, _
      | TyFunction _, _ ->
          raise
            (type_mismatch "int or float"
               (Debug.string_of_ty (resolve left_ty))
               (span_of_expr left)))
  | OpEqual | OpNotEqual ->
      (try unify (span_of_expr right) left_ty right_ty
       with TypeError _ ->
         raise
           (type_mismatch
              (Debug.string_of_ty (resolve left_ty))
              (Debug.string_of_ty (resolve right_ty))
              (span_of_expr right)));
      TyBool
  | OpLess | OpGreater -> (
      match (resolve left_ty, resolve right_ty) with
      | TyInt, TyInt -> TyBool
      | TyFloat, TyFloat -> TyBool
      | TyInt, TyMeta _ ->
          unify (span_of_expr right) right_ty TyInt;
          TyBool
      | TyFloat, TyMeta _ ->
          unify (span_of_expr right) right_ty TyFloat;
          TyBool
      | TyMeta _, TyInt ->
          unify (span_of_expr left) left_ty TyInt;
          TyBool
      | TyMeta _, TyFloat ->
          unify (span_of_expr left) left_ty TyFloat;
          TyBool
      | TyMeta _, TyMeta _ ->
          unify (span_of_expr left) left_ty TyInt;
          unify (span_of_expr right) right_ty TyInt;
          TyBool
      | TyInt, got ->
          raise
            (type_mismatch "int" (Debug.string_of_ty got) (span_of_expr right))
      | TyFloat, got ->
          raise
            (type_mismatch "float" (Debug.string_of_ty got) (span_of_expr right))
      | TyMeta _, got ->
          raise
            (type_mismatch "int or float" (Debug.string_of_ty got)
               (span_of_expr right))
      | TyString, _
      | TyBool, _
      | TyUnit, _
      | TyVar _, _
      | TyModule _, _
      | TyList _, _
      | TyFunction _, _ ->
          raise
            (type_mismatch "int or float"
               (Debug.string_of_ty (resolve left_ty))
               (span_of_expr left)))
  | OpCons ->
      let elem = fresh_meta () in
      unify (span_of_expr left) left_ty elem;
      unify (span_of_expr right) right_ty (TyList elem);
      TyList elem

let infer_program (program : program) =
  let env = process_imports Env.empty program.imports in
  let rec go env = function
    | [] -> ()
    | Let { name; ty = ann; let_body; let_span } :: rest ->
        let pre_ty = fresh_meta () in
        let pre_env = bind_name name pre_ty let_span env in
        let body_ty = infer_expr pre_env let_body in
        unify let_span pre_ty body_ty;
        (match ann with
        | Some ann_ty -> unify let_span ann_ty body_ty
        | None -> ());
        let gen_ty = generalize env body_ty in
        let env' = bind_name name gen_ty let_span env in
        go env' rest
    | (( Literal _ | Variable _ | Lambda _ | Apply _ | If _ | Match _ | Block _
       | Binary _ | Unary _ | MemberAccess _ ) as e)
      :: rest ->
        let _ = infer_expr env e in
        go env rest
  in
  go env program.body

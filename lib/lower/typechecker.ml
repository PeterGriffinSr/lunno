open Lunno_common
open Lunno_modules
open Error

module Env = struct
  module StringMap = Map.Make (String)

  let empty = StringMap.empty
  let mem = StringMap.mem
  let add = StringMap.add
  let find = StringMap.find_opt
  let fold = StringMap.fold
end

let next_id = ref 0

let fresh_meta () =
  let id = !next_id in
  incr next_id;
  Ast.TyMeta { Ast.id; Ast.contents = ref None }

let fresh_family_meta family =
  let id = !next_id in
  incr next_id;
  Ast.TyFamilyMeta { Ast.fid = id; Ast.family; Ast.fcontents = ref None }

let rec resolve ty =
  match ty with
  | Ast.TyMeta { Ast.contents; _ } -> (
      match !contents with Some t -> resolve t | None -> ty)
  | Ast.TyFamilyMeta { Ast.fcontents; _ } -> (
      match !fcontents with Some t -> resolve t | None -> ty)
  | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
  | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit
  | Ast.TyList _ | Ast.TyFunction _ | Ast.TyVar _ | Ast.TyBoundVar _
  | Ast.TyModule _ | Ast.TyAdt _ ->
      ty

let rec occurs id ty =
  match resolve ty with
  | Ast.TyMeta { Ast.id = id2; _ } -> id = id2
  | Ast.TyFamilyMeta { Ast.fcontents; _ } -> (
      match !fcontents with Some t -> occurs id t | None -> false)
  | Ast.TyList t -> occurs id t
  | Ast.TyFunction (params, ret) ->
      List.exists (occurs id) params || occurs id ret
  | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
  | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit | Ast.TyVar _
  | Ast.TyBoundVar _ | Ast.TyModule _ | Ast.TyAdt _ ->
      false

let family_of_ty = function
  | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 -> Some Ast.FInt
  | Ast.TyF32 | Ast.TyF64 -> Some Ast.FFloat
  | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit
  | Ast.TyList _ | Ast.TyFunction _ | Ast.TyVar _ | Ast.TyBoundVar _
  | Ast.TyModule _ | Ast.TyAdt _ | Ast.TyMeta _ | Ast.TyFamilyMeta _ ->
      None

let member_of_family ty fam = family_of_ty ty = Some fam
let family_name = function Ast.FInt -> Ast.TyInt | Ast.FFloat -> Ast.TyFloat

let unify_family_meta span fm concrete =
  match !(fm.Ast.fcontents) with
  | Some _ -> assert false
  | None -> (
      match concrete with
      | Ast.TyMeta m -> (
          match !(m.Ast.contents) with
          | Some _ -> assert false
          | None ->
              m.Ast.contents := Some (Ast.TyFamilyMeta fm);
              Ok ())
      | Ast.TyVar _ | Ast.TyBoundVar _ ->
          fm.Ast.fcontents := Some concrete;
          Ok ()
      | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32 | Ast.TyF64
      | Ast.TyInt | Ast.TyFloat | Ast.TyString | Ast.TyBool | Ast.TyUnit
      | Ast.TyList _ | Ast.TyFunction _ | Ast.TyModule _ | Ast.TyAdt _
      | Ast.TyFamilyMeta _ ->
          if not (member_of_family concrete fm.Ast.family) then
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type '%s' is not a member of family '%s'"
                 (Ty_utils.string_of_ty concrete)
                 (Ty_utils.string_of_ty (family_name fm.Ast.family)))
              span
          else (
            fm.Ast.fcontents := Some concrete;
            Ok ()))

let rec unify span t1 t2 =
  let t1 = resolve t1 and t2 = resolve t2 in
  match (t1, t2) with
  | Ast.TyInt, Ast.TyInt
  | Ast.TyFloat, Ast.TyFloat
  | Ast.TyI8, Ast.TyI8
  | Ast.TyI16, Ast.TyI16
  | Ast.TyI32, Ast.TyI32
  | Ast.TyI64, Ast.TyI64
  | Ast.TyF32, Ast.TyF32
  | Ast.TyF64, Ast.TyF64
  | Ast.TyString, Ast.TyString
  | Ast.TyBool, Ast.TyBool
  | Ast.TyUnit, Ast.TyUnit ->
      Ok ()
  | Ast.TyVar a, Ast.TyVar b when a = b -> Ok ()
  | Ast.TyAdt a, Ast.TyAdt b when a = b -> Ok ()
  | Ast.TyBoundVar (a, fa), Ast.TyBoundVar (b, fb) when a = b && fa = fb ->
      Ok ()
  | Ast.TyList a, Ast.TyList b -> unify span a b
  | Ast.TyFunction (ps1, r1), Ast.TyFunction (ps2, r2) ->
      if List.length ps1 <> List.length ps2 then
        type_error E_Type_TypeMismatch
          (Printf.sprintf "type mismatch: expected '%s', got '%s'"
             (Ty_utils.string_of_ty t1) (Ty_utils.string_of_ty t2))
          span
      else
        let* () =
          List.fold_left
            (fun acc (a, b) ->
              let* () = acc in
              unify span a b)
            (Ok ()) (List.combine ps1 ps2)
        in
        unify span r1 r2
  | Ast.TyFamilyMeta fm1, Ast.TyFamilyMeta fm2 when fm1.Ast.fid = fm2.Ast.fid ->
      Ok ()
  | Ast.TyFamilyMeta fm1, Ast.TyFamilyMeta fm2 ->
      if fm1.Ast.family <> fm2.Ast.family then
        type_error E_Type_TypeMismatch
          (Printf.sprintf "type mismatch: expected '%s', got '%s'"
             (Ty_utils.string_of_ty t1) (Ty_utils.string_of_ty t2))
          span
      else
        let shared = fresh_family_meta fm1.Ast.family in
        fm1.Ast.fcontents := Some shared;
        fm2.Ast.fcontents := Some shared;
        Ok ()
  | Ast.TyFamilyMeta fm, concrete -> unify_family_meta span fm concrete
  | concrete, Ast.TyFamilyMeta fm -> unify_family_meta span fm concrete
  | Ast.TyMeta m, t | t, Ast.TyMeta m -> (
      match !(m.Ast.contents) with
      | Some _ -> assert false
      | None ->
          if occurs m.Ast.id t then
            match t with
            | Ast.TyMeta m2 when m.Ast.id = m2.Ast.id -> Ok ()
            | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64
            | Ast.TyFloat | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool
            | Ast.TyUnit | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyList _
            | Ast.TyFunction _ | Ast.TyModule _ | Ast.TyMeta _
            | Ast.TyFamilyMeta _ | Ast.TyAdt _ ->
                type_error E_Type_TypeMismatch
                  (Printf.sprintf "type mismatch: expected '%s', got '%s'"
                     (Ty_utils.string_of_ty t1) (Ty_utils.string_of_ty t2))
                  span
          else (
            m.Ast.contents := Some t;
            Ok ()))
  | Ast.TyInt, _
  | Ast.TyI8, _
  | Ast.TyI16, _
  | Ast.TyI32, _
  | Ast.TyI64, _
  | Ast.TyFloat, _
  | Ast.TyF32, _
  | Ast.TyF64, _
  | Ast.TyString, _
  | Ast.TyBool, _
  | Ast.TyUnit, _
  | Ast.TyVar _, _
  | Ast.TyBoundVar _, _
  | Ast.TyList _, _
  | Ast.TyFunction _, _
  | Ast.TyModule _, _
  | Ast.TyAdt _, _ ->
      type_error E_Type_TypeMismatch
        (Printf.sprintf "type mismatch: expected '%s', got '%s'"
           (Ty_utils.string_of_ty t1) (Ty_utils.string_of_ty t2))
        span

let rec free_metas ty =
  match resolve ty with
  | Ast.TyMeta { Ast.id; Ast.contents } when !contents = None -> [ id ]
  | Ast.TyMeta _ -> []
  | Ast.TyFamilyMeta { Ast.fcontents; _ } when !fcontents = None -> []
  | Ast.TyFamilyMeta { Ast.fcontents; _ } -> free_metas (Option.get !fcontents)
  | Ast.TyList t -> free_metas t
  | Ast.TyFunction (ps, r) -> List.concat_map free_metas ps @ free_metas r
  | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
  | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit | Ast.TyVar _
  | Ast.TyBoundVar _ | Ast.TyModule _ | Ast.TyAdt _ ->
      []

let rec free_family_metas ty =
  match resolve ty with
  | Ast.TyFamilyMeta { Ast.fid; Ast.family; Ast.fcontents; _ }
    when !fcontents = None ->
      [ (fid, family) ]
  | Ast.TyFamilyMeta { Ast.fcontents; _ } ->
      free_family_metas (Option.get !fcontents)
  | Ast.TyList t -> free_family_metas t
  | Ast.TyFunction (ps, r) ->
      List.concat_map free_family_metas ps @ free_family_metas r
  | Ast.TyMeta { Ast.contents; _ } -> (
      match !contents with Some t -> free_family_metas t | None -> [])
  | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
  | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit | Ast.TyVar _
  | Ast.TyBoundVar _ | Ast.TyModule _ | Ast.TyAdt _ ->
      []

let env_free_metas env = Env.fold (fun _ ty acc -> free_metas ty @ acc) env []

let env_free_family_metas env =
  Env.fold (fun _ ty acc -> List.map fst (free_family_metas ty) @ acc) env []

let default_of_family = function
  | Ast.FInt -> Ast.TyI64
  | Ast.FFloat -> Ast.TyF64

let rec monomorphize_ty ty =
  match resolve ty with
  | Ast.TyFamilyMeta { Ast.family; Ast.fcontents; _ } -> (
      match !fcontents with
      | Some t -> monomorphize_ty t
      | None ->
          let def = default_of_family family in
          fcontents := Some def;
          def)
  | Ast.TyMeta { Ast.contents; _ } -> (
      match !contents with
      | Some t -> monomorphize_ty t
      | None ->
          contents := Some Ast.TyUnit;
          Ast.TyUnit)
  | Ast.TyBoundVar (_, Ast.FInt) -> Ast.TyI64
  | Ast.TyBoundVar (_, Ast.FFloat) -> Ast.TyF64
  | Ast.TyVar _ -> ty
  | Ast.TyList t -> Ast.TyList (monomorphize_ty t)
  | Ast.TyFunction (ps, r) ->
      Ast.TyFunction (List.map monomorphize_ty ps, monomorphize_ty r)
  | Ast.TyInt -> Ast.TyI64
  | Ast.TyFloat -> Ast.TyF64
  | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32 | Ast.TyF64
  | Ast.TyString | Ast.TyBool | Ast.TyUnit | Ast.TyModule _ | Ast.TyAdt _ ->
      ty

let generalize env ty =
  let env_metas = env_free_metas env in
  let to_gen =
    List.filter (fun id -> not (List.mem id env_metas)) (free_metas ty)
  in
  let env_fms = env_free_family_metas env in
  let free_fms = free_family_metas ty in
  let to_gen_fm =
    List.filter (fun (id, _) -> not (List.mem id env_fms)) free_fms
  in
  let to_gen_fm =
    List.fold_left
      (fun acc (id, fam) ->
        if List.exists (fun (id2, _) -> id = id2) acc then acc
        else (id, fam) :: acc)
      [] to_gen_fm
    |> List.rev
  in
  let meta_mapping =
    List.mapi
      (fun i id ->
        let name = String.make 1 (Char.chr (97 + i)) in
        (id, name))
      to_gen
  in
  let fm_mapping =
    let family_names = ref [] in
    let name_counter = ref (List.length to_gen) in
    List.map
      (fun (id, family) ->
        let name =
          match List.assoc_opt family !family_names with
          | Some n -> n
          | None ->
              let n = String.make 1 (Char.chr (97 + !name_counter)) in
              incr name_counter;
              family_names := (family, n) :: !family_names;
              n
        in
        (id, (name, family)))
      to_gen_fm
  in
  let rec go ty =
    match resolve ty with
    | Ast.TyMeta { Ast.id; _ } -> (
        match List.assoc_opt id meta_mapping with
        | Some name -> Ast.TyVar name
        | None -> ty)
    | Ast.TyFamilyMeta { Ast.fid; Ast.family; Ast.fcontents; _ } -> (
        match !fcontents with
        | Some t -> go t
        | None -> (
            match List.assoc_opt fid fm_mapping with
            | Some (name, fam) -> Ast.TyBoundVar (name, fam)
            | None ->
                let def = default_of_family family in
                fcontents := Some def;
                def))
    | Ast.TyList t -> Ast.TyList (go t)
    | Ast.TyFunction (ps, r) -> Ast.TyFunction (List.map go ps, go r)
    | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
    | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit
    | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyModule _ | Ast.TyAdt _ ->
        ty
  in
  go ty

let instantiate ty =
  let mapping = Hashtbl.create 4 in
  let rec go ty =
    match ty with
    | Ast.TyVar name -> (
        match Hashtbl.find_opt mapping name with
        | Some m -> m
        | None ->
            let m : Ast.ty = fresh_meta () in
            Hashtbl.add mapping name m;
            m)
    | Ast.TyBoundVar (name, family) -> (
        match Hashtbl.find_opt mapping name with
        | Some m -> m
        | None ->
            let m = fresh_family_meta family in
            Hashtbl.add mapping name m;
            m)
    | Ast.TyList t -> Ast.TyList (go t)
    | Ast.TyFunction (ps, r) -> Ast.TyFunction (List.map go ps, go r)
    | Ast.TyMeta { Ast.contents; _ } -> (
        match !contents with Some t -> go t | None -> ty)
    | Ast.TyFamilyMeta { Ast.fcontents; _ } -> (
        match !fcontents with Some t -> go t | None -> ty)
    | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
    | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit
    | Ast.TyModule _ | Ast.TyAdt _ ->
        ty
  in
  go ty

let bind_name name ty span env =
  if Env.mem name env then
    type_error E_Type_AlreadyDefined
      (Printf.sprintf "name '%s' is already defined in this scope" name)
      span
  else Ok (Env.add name ty env)

let process_imports env imports =
  List.fold_left
    (fun acc import ->
      let* env = acc in
      match
        Registry.find_module ~namespace:import.Ast.module_ ~name:import.Ast.item
      with
      | Some _ ->
          bind_name import.Ast.item
            (Ast.TyModule (import.Ast.module_, import.Ast.item))
            import.Ast.import_span env
      | None ->
          type_error E_Type_UndefinedVariable
            (Printf.sprintf "Module '%s:%s' not found" import.Ast.module_
               import.Ast.item)
            import.Ast.import_span)
    (Ok env) imports

let infer_literal = function
  | Ast.LInt _ -> fresh_family_meta Ast.FInt
  | Ast.LFloat _ -> fresh_family_meta Ast.FFloat
  | Ast.LString _ -> Ast.TyString
  | Ast.LBool _ -> Ast.TyBool
  | Ast.LUnit -> Ast.TyUnit
  | Ast.LNil -> Ast.TyList (fresh_meta ())

let rec infer_expr env expr =
  match expr with
  | Ast.Literal (lit, span) ->
      let ty = infer_literal lit in
      Ok (Typed_ast.Literal (lit, resolve ty, span))
  | Ast.Variable (name, span) -> (
      match Env.find name env with
      | Some ty -> Ok (Typed_ast.Variable (name, instantiate ty, span))
      | None ->
          type_error E_Type_UndefinedVariable
            (Printf.sprintf "undefined variable '%s'" name)
            span)
  | Ast.Lambda lam ->
      let* l = infer_lambda env lam in
      Ok (Typed_ast.Lambda l)
  | Ast.Apply (func, args, span) ->
      let* typed_func = infer_expr env func in
      let func_ty = Typed_ast.ty_of typed_func in
      let* typed_args =
        List.fold_left
          (fun acc arg ->
            let* acc = acc in
            let* t = infer_expr env arg in
            Ok (acc @ [ t ]))
          (Ok []) args
      in
      let arg_tys = List.map Typed_ast.ty_of typed_args in
      let ret_ty = fresh_meta () in
      let* () =
        match resolve func_ty with
        | Ast.TyFunction (param_tys, _) ->
            let expected = List.length param_tys in
            let got = List.length arg_tys in
            if expected <> got then
              type_error E_Type_ArityMismatch
                (Printf.sprintf
                   "arity mismatch: expected %d argument(s), got %d" expected
                   got)
                span
            else unify span func_ty (Ast.TyFunction (arg_tys, ret_ty))
        | Ast.TyMeta _ -> unify span func_ty (Ast.TyFunction (arg_tys, ret_ty))
        | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
        | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit
        | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyList _ | Ast.TyModule _
        | Ast.TyFamilyMeta _ | Ast.TyAdt _ ->
            type_error E_Type_NotAFunction
              (Printf.sprintf "expected a function, got '%s'"
                 (Ty_utils.string_of_ty (resolve func_ty)))
              span
      in
      Ok (Typed_ast.Apply (typed_func, typed_args, resolve ret_ty, span))
  | Ast.Let { Ast.name; Ast.ty = ann; Ast.let_body; Ast.let_span } ->
      let* typed_body =
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
            let* pre_env =
              bind_name name (Ast.TyFunction (param_tys, ret_ty)) let_span env
            in
            let* typed =
              infer_expr pre_env
                (Ast.Lambda
                   {
                     lam with
                     Ast.params =
                       List.map2
                         (fun p ty -> { p with Ast.param_ty = Some ty })
                         lam.Ast.params param_tys;
                   })
            in
            let* () =
              match typed with
              | Typed_ast.Lambda l -> unify let_span ret_ty l.Typed_ast.ret_ty
              | Typed_ast.Literal _ | Typed_ast.Variable _ | Typed_ast.Apply _
              | Typed_ast.Let _ | Typed_ast.If _ | Typed_ast.Match _
              | Typed_ast.Block _ | Typed_ast.Binary _ | Typed_ast.Unary _
              | Typed_ast.MemberAccess _ | Typed_ast.Range _
              | Typed_ast.Constructor _ ->
                  Ok ()
            in
            Ok typed
        | Ast.Lambda _ | Ast.Literal _ | Ast.Variable _ | Ast.Apply _
        | Ast.Let _ | Ast.If _ | Ast.Match _ | Ast.Block _ | Ast.Binary _
        | Ast.Unary _ | Ast.MemberAccess _ | Ast.Range _ | Ast.Constructor _ ->
            infer_expr env let_body
      in
      let body_ty = Typed_ast.ty_of typed_body in
      let* () =
        match ann with
        | Some ann_ty -> unify let_span ann_ty body_ty
        | None -> Ok ()
      in
      let gen_ty = generalize env body_ty in
      Ok
        (Typed_ast.Let
           { Typed_ast.name; let_ty = gen_ty; let_body = typed_body; let_span })
  | Ast.If { Ast.cond; Ast.then_; Ast.else_; Ast.if_span } -> (
      let* typed_cond = infer_expr env cond in
      let* () = unify if_span (Typed_ast.ty_of typed_cond) Ast.TyBool in
      let* typed_then = infer_expr env then_ in
      let then_ty = Typed_ast.ty_of typed_then in
      match else_ with
      | None ->
          type_error E_Type_MissingElseBranch
            "if expression is missing an else branch" if_span
      | Some else_expr ->
          let* typed_else = infer_expr env else_expr in
          let else_ty = Typed_ast.ty_of typed_else in
          let* () =
            match unify if_span then_ty else_ty with
            | Ok () -> Ok ()
            | Error _ ->
                type_error E_Type_IfBranchMismatch
                  (Printf.sprintf
                     "if branches have different types: then is '%s', else is \
                      '%s'"
                     (Ty_utils.string_of_ty (resolve then_ty))
                     (Ty_utils.string_of_ty (resolve else_ty)))
                  if_span
          in
          Ok
            (Typed_ast.If
               {
                 Typed_ast.cond = typed_cond;
                 Typed_ast.then_ = typed_then;
                 Typed_ast.else_ = Some typed_else;
                 Typed_ast.if_ty = resolve then_ty;
                 Typed_ast.if_span;
               }))
  | Ast.Match { Ast.scrutinee; Ast.cases; Ast.match_span } ->
      let* typed_scrutinee = infer_expr env scrutinee in
      let scrutinee_ty = Typed_ast.ty_of typed_scrutinee in
      let* typed_cases, result_ty =
        infer_match env scrutinee_ty cases match_span
      in
      Ok
        (Typed_ast.Match
           {
             Typed_ast.scrutinee = typed_scrutinee;
             Typed_ast.cases = typed_cases;
             Typed_ast.match_ty = result_ty;
             Typed_ast.match_span;
           })
  | Ast.Block (exprs, span) ->
      let* typed_exprs, ty = infer_block env exprs span in
      Ok (Typed_ast.Block (typed_exprs, resolve ty, span))
  | Ast.Binary { Ast.binary_op; Ast.left; Ast.right; Ast.binary_span } ->
      infer_binary env binary_op left right binary_span
  | Ast.Unary { unary_op = Ast.OpNegate; Ast.expr = e; Ast.unary_span } ->
      let* typed_e = infer_expr env e in
      let ty = Typed_ast.ty_of typed_e in
      let* () =
        match unify unary_span ty Ast.TyInt with
        | Ok () -> Ok ()
        | Error _ -> (
            match unify unary_span ty Ast.TyFloat with
            | Ok () -> Ok ()
            | Error _ ->
                type_error E_Type_TypeMismatch
                  (Printf.sprintf "type mismatch: expected 'int', got '%s'"
                     (Ty_utils.string_of_ty ty))
                  unary_span)
      in
      Ok
        (Typed_ast.Unary
           {
             Typed_ast.unary_op = Ast.OpNegate;
             Typed_ast.expr = typed_e;
             Typed_ast.unary_ty = resolve ty;
             Typed_ast.unary_span;
           })
  | Ast.MemberAccess (obj, member, span) -> (
      let* typed_obj = infer_expr env obj in
      let obj_ty = Typed_ast.ty_of typed_obj in
      match resolve obj_ty with
      | Ast.TyModule (namespace, name) -> (
          match Registry.find_module ~namespace ~name with
          | Some module_info -> (
              match List.assoc_opt member module_info.Registry.exports with
              | Some member_ty ->
                  let ty = instantiate member_ty in
                  Ok (Typed_ast.MemberAccess (typed_obj, member, ty, span))
              | None ->
                  type_error E_Type_UndefinedVariable
                    (Printf.sprintf "undefined variable '%s.%s'" name member)
                    span)
          | None ->
              type_error E_Type_UndefinedVariable
                (Printf.sprintf "undefined variable '%s:%s'" namespace name)
                span)
      | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
      | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit
      | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyMeta _ | Ast.TyFamilyMeta _
      | Ast.TyList _ | Ast.TyFunction _ | Ast.TyAdt _ ->
          type_error E_Type_TypeMismatch
            (Printf.sprintf "type mismatch: expected 'module', got '%s'"
               (Ty_utils.string_of_ty obj_ty))
            (Ast.span_of_expr obj))
  | Ast.Range (start, stop, span) ->
      let* typed_start = infer_expr env start in
      let* typed_stop = infer_expr env stop in
      let* () = unify span (Typed_ast.ty_of typed_start) Ast.TyInt in
      let* () = unify span (Typed_ast.ty_of typed_stop) Ast.TyInt in
      Ok (Typed_ast.Range (typed_start, typed_stop, span))
  | Ast.Constructor (name, args, span) -> (
      match Env.find name env with
      | Some ty ->
          let* typed_args =
            List.fold_left
              (fun acc arg ->
                let* acc = acc in
                let* t = infer_expr env arg in
                Ok (acc @ [ t ]))
              (Ok []) args
          in
          let arg_tys = List.map Typed_ast.ty_of typed_args in
          let ret_ty = fresh_meta () in
          let* () = unify span ty (Ast.TyFunction (arg_tys, ret_ty)) in
          Ok (Typed_ast.Constructor (name, typed_args, resolve ret_ty, span))
      | None ->
          type_error E_Type_UndefinedVariable
            (Printf.sprintf "undefined constructor '%s'" name)
            span)

and infer_lambda env lam =
  let param_tys =
    List.map
      (fun param ->
        match param.Ast.param_ty with Some ty -> ty | None -> fresh_meta ())
      lam.Ast.params
  in
  let* body_env =
    List.fold_left2
      (fun acc param ty ->
        let* env' = acc in
        bind_name param.Ast.param_name ty param.Ast.param_span env')
      (Ok env) lam.Ast.params param_tys
  in
  let* typed_body = infer_expr body_env lam.Ast.lambda_body in
  let body_ty = Typed_ast.ty_of typed_body in
  let* () =
    match lam.Ast.ret_ty with
    | Some ann_ret ->
        let body_span =
          match lam.Ast.lambda_body with
          | Ast.Block ((_ :: _ as exprs), _) ->
              Ast.span_of_expr (List.nth exprs (List.length exprs - 1))
          | Ast.Block ([], _)
          | Ast.Literal _ | Ast.Variable _ | Ast.Lambda _ | Ast.Apply _
          | Ast.Let _ | Ast.If _ | Ast.Match _ | Ast.Binary _ | Ast.Unary _
          | Ast.MemberAccess _ | Ast.Range _ | Ast.Constructor _ ->
              Ast.span_of_expr lam.Ast.lambda_body
        in
        unify body_span ann_ret body_ty
    | None -> Ok ()
  in
  let resolved_param_tys = List.map resolve param_tys in
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
  let ret_ty = resolve body_ty in
  let lambda_ty =
    Ast.TyFunction (List.map resolve param_tys, resolve body_ty)
  in
  Ok
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
  | [] -> Ok ([], Ast.TyUnit)
  | first :: rest ->
      let* env' = infer_pattern env scrutinee_ty first.Ast.pattern in
      let* env'' =
        match first.Ast.guard with
        | None -> Ok env'
        | Some g ->
            let* typed_g = infer_expr env' g in
            let* () =
              unify
                (Typed_ast.span_of typed_g)
                (Typed_ast.ty_of typed_g) Ast.TyBool
            in
            Ok env'
      in
      let* typed_first_guard =
        match first.Ast.guard with
        | None -> Ok None
        | Some g ->
            let* tg = infer_expr env' g in
            Ok (Some tg)
      in
      let* typed_first_body = infer_expr env'' first.Ast.case_body in
      let first_ty = Typed_ast.ty_of typed_first_body in
      let typed_first =
        {
          Typed_ast.pattern = first.Ast.pattern;
          guard = typed_first_guard;
          case_body = typed_first_body;
          case_span = first.Ast.case_span;
        }
      in
      let* typed_rest =
        List.fold_left
          (fun acc case ->
            let* acc = acc in
            let* env' = infer_pattern env scrutinee_ty case.Ast.pattern in
            let* typed_guard =
              match case.Ast.guard with
              | None -> Ok None
              | Some g ->
                  let* typed_g = infer_expr env' g in
                  let* () =
                    unify
                      (Typed_ast.span_of typed_g)
                      (Typed_ast.ty_of typed_g) Ast.TyBool
                  in
                  Ok (Some typed_g)
            in
            let* env'' =
              match case.Ast.guard with None -> Ok env' | Some _ -> Ok env'
            in
            let* typed_body = infer_expr env'' case.Ast.case_body in
            let case_ty = Typed_ast.ty_of typed_body in
            let* () =
              match unify case.Ast.case_span first_ty case_ty with
              | Ok () -> Ok ()
              | Error _ ->
                  type_error E_Type_MatchBranchMismatch
                    (Printf.sprintf
                       "match branch type mismatch: expected '%s', got '%s'"
                       (Ty_utils.string_of_ty (resolve first_ty))
                       (Ty_utils.string_of_ty (resolve case_ty)))
                    case.Ast.case_span
            in
            Ok
              (acc
              @ [
                  {
                    Typed_ast.pattern = case.Ast.pattern;
                    guard = typed_guard;
                    case_body = typed_body;
                    case_span = case.Ast.case_span;
                  };
                ]))
          (Ok []) rest
      in
      Ok (typed_first :: typed_rest, resolve first_ty)

and infer_pattern env expected pat =
  match pat with
  | Ast.PWildcard _ -> Ok env
  | Ast.PVariable (name, span) -> (
      match Env.find name env with
      | Some (Ast.TyAdt _) ->
          let* () = unify span expected (Option.get (Env.find name env)) in
          Ok env
      | Some
          ( Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64
          | Ast.TyFloat | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool
          | Ast.TyUnit | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyModule _
          | Ast.TyFunction _ | Ast.TyFamilyMeta _ | Ast.TyMeta _ | Ast.TyList _
            ) ->
          bind_name name expected span env
      | None -> bind_name name expected span env)
  | Ast.PIntLiteral (_, span) -> (
      match resolve expected with
      | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 -> Ok env
      | Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ } -> Ok env
      | Ast.TyMeta _ ->
          let* () = unify span expected (fresh_family_meta Ast.FInt) in
          Ok env
      | Ast.TyFamilyMeta { Ast.family = Ast.FFloat; _ }
      | Ast.TyFloat | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool
      | Ast.TyUnit | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyModule _
      | Ast.TyList _ | Ast.TyFunction _ | Ast.TyInt | Ast.TyAdt _ ->
          type_error E_Type_PatternTypeMismatch
            (Printf.sprintf "pattern type mismatch: expected '%s', got 'int'"
               (Ty_utils.string_of_ty expected))
            span)
  | Ast.PFloatLiteral (_, span) -> (
      match resolve expected with
      | Ast.TyF32 | Ast.TyF64 -> Ok env
      | Ast.TyFamilyMeta { Ast.family = Ast.FFloat; _ } -> Ok env
      | Ast.TyMeta _ ->
          let* () = unify span expected (fresh_family_meta Ast.FFloat) in
          Ok env
      | Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ }
      | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyString
      | Ast.TyBool | Ast.TyUnit | Ast.TyVar _ | Ast.TyBoundVar _
      | Ast.TyModule _ | Ast.TyList _ | Ast.TyFunction _ | Ast.TyFloat
      | Ast.TyAdt _ ->
          type_error E_Type_PatternTypeMismatch
            (Printf.sprintf "pattern type mismatch: expected '%s', got 'float'"
               (Ty_utils.string_of_ty expected))
            span)
  | Ast.PStringLiteral (_, span) -> (
      match resolve expected with
      | Ast.TyString -> Ok env
      | Ast.TyMeta _ ->
          let* () = unify span expected Ast.TyString in
          Ok env
      | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
      | Ast.TyF32 | Ast.TyF64 | Ast.TyBool | Ast.TyUnit | Ast.TyVar _
      | Ast.TyBoundVar _ | Ast.TyModule _ | Ast.TyList _ | Ast.TyFunction _
      | Ast.TyFamilyMeta _ | Ast.TyAdt _ ->
          type_error E_Type_PatternTypeMismatch
            (Printf.sprintf "pattern type mismatch: expected '%s', got 'string'"
               (Ty_utils.string_of_ty expected))
            span)
  | Ast.PBooleanLiteral (_, span) -> (
      match resolve expected with
      | Ast.TyBool -> Ok env
      | Ast.TyMeta _ ->
          let* () = unify span expected Ast.TyBool in
          Ok env
      | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
      | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyUnit | Ast.TyVar _
      | Ast.TyBoundVar _ | Ast.TyModule _ | Ast.TyList _ | Ast.TyFunction _
      | Ast.TyFamilyMeta _ | Ast.TyAdt _ ->
          type_error E_Type_PatternTypeMismatch
            (Printf.sprintf "pattern type mismatch: expected '%s', got 'bool'"
               (Ty_utils.string_of_ty expected))
            span)
  | Ast.PNil span -> (
      match resolve expected with
      | Ast.TyList _ -> Ok env
      | Ast.TyMeta _ ->
          let elem = fresh_meta () in
          let* () = unify span expected (Ast.TyList elem) in
          Ok env
      | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
      | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit
      | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyModule _ | Ast.TyFunction _
      | Ast.TyFamilyMeta _ | Ast.TyAdt _ ->
          type_error E_Type_PatternTypeMismatch
            (Printf.sprintf "pattern type mismatch: expected '%s', got '_ list'"
               (Ty_utils.string_of_ty expected))
            span)
  | Ast.PCons (hd, tl, span) -> (
      match resolve expected with
      | Ast.TyList elem_ty ->
          let* env' = infer_pattern env elem_ty hd in
          infer_pattern env' (Ast.TyList elem_ty) tl
      | Ast.TyMeta _ ->
          let elem = fresh_meta () in
          let* () = unify span expected (Ast.TyList elem) in
          let* env' = infer_pattern env elem hd in
          infer_pattern env' (Ast.TyList elem) tl
      | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
      | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit
      | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyModule _ | Ast.TyFunction _
      | Ast.TyFamilyMeta _ | Ast.TyAdt _ ->
          type_error E_Type_PatternTypeMismatch
            (Printf.sprintf "pattern type mismatch: expected '%s', got '_ list'"
               (Ty_utils.string_of_ty expected))
            span)
  | Ast.PConstructor (name, fields, span) -> (
      match resolve expected with
      | Ast.TyAdt adt_name -> (
          match Env.find name env with
          | Some (Ast.TyFunction (field_tys, Ast.TyAdt ret_name)) ->
              if ret_name <> adt_name then
                type_error E_Type_PatternTypeMismatch
                  (Printf.sprintf
                     "pattern type mismatch: constructor '%s' belongs to '%s', \
                      expected '%s'"
                     name ret_name adt_name)
                  span
              else if List.length fields <> List.length field_tys then
                type_error E_Type_ArityMismatch
                  (Printf.sprintf "constructor '%s' expects %d field(s), got %d"
                     name (List.length field_tys) (List.length fields))
                  span
              else
                List.fold_left2
                  (fun acc pat ty ->
                    let* env' = acc in
                    infer_pattern env' ty pat)
                  (Ok env) fields field_tys
          | Some (Ast.TyAdt ret_name) ->
              if ret_name <> adt_name then
                type_error E_Type_PatternTypeMismatch
                  (Printf.sprintf
                     "pattern type mismatch: constructor '%s' belongs to '%s', \
                      expected '%s'"
                     name ret_name adt_name)
                  span
              else if fields <> [] then
                type_error E_Type_ArityMismatch
                  (Printf.sprintf "constructor '%s' expects 0 field(s), got %d"
                     name (List.length fields))
                  span
              else Ok env
          | None
          | Some
              ( Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64
              | Ast.TyFloat | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool
              | Ast.TyUnit | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyMeta _
              | Ast.TyFamilyMeta _ | Ast.TyList _ | Ast.TyFunction _
              | Ast.TyModule _ ) ->
              type_error E_Type_UndefinedVariable
                (Printf.sprintf "undefined constructor '%s'" name)
                span)
      | Ast.TyMeta _ -> (
          match Env.find name env with
          | Some (Ast.TyFunction (field_tys, (Ast.TyAdt _ as adt_ty))) ->
              let* () = unify span expected adt_ty in
              if List.length fields <> List.length field_tys then
                type_error E_Type_ArityMismatch
                  (Printf.sprintf "constructor '%s' expects %d field(s), got %d"
                     name (List.length field_tys) (List.length fields))
                  span
              else
                List.fold_left2
                  (fun acc pat ty ->
                    let* env' = acc in
                    infer_pattern env' ty pat)
                  (Ok env) fields field_tys
          | Some (Ast.TyAdt _ as adt_ty) ->
              let* () = unify span expected adt_ty in
              if fields <> [] then
                type_error E_Type_ArityMismatch
                  (Printf.sprintf "constructor '%s' expects 0 field(s), got %d"
                     name (List.length fields))
                  span
              else Ok env
          | None
          | Some
              ( Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64
              | Ast.TyFloat | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool
              | Ast.TyUnit | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyMeta _
              | Ast.TyFamilyMeta _ | Ast.TyList _ | Ast.TyFunction _
              | Ast.TyModule _ ) ->
              type_error E_Type_UndefinedVariable
                (Printf.sprintf "undefined constructor '%s'" name)
                span)
      | ( Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
        | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit
        | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyFamilyMeta _ | Ast.TyList _
        | Ast.TyFunction _ | Ast.TyModule _ ) as other ->
          type_error E_Type_PatternTypeMismatch
            (Printf.sprintf
               "pattern type mismatch: expected '%s', got constructor '%s'"
               (Ty_utils.string_of_ty other)
               name)
            span)

and infer_block env exprs _span =
  let rec go env acc = function
    | [] -> Ok (List.rev acc, Ast.TyUnit)
    | [ e ] ->
        let* typed_e = infer_expr env e in
        let ty = resolve (Typed_ast.ty_of typed_e) in
        Ok (List.rev (typed_e :: acc), ty)
    | Ast.Let { Ast.name; Ast.ty = ann; Ast.let_body; Ast.let_span } :: rest ->
        let* typed_body =
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
              let* pre_env =
                bind_name name (Ast.TyFunction (param_tys, ret_ty)) let_span env
              in
              let* typed =
                infer_expr pre_env
                  (Ast.Lambda
                     {
                       lam with
                       Ast.params =
                         List.map2
                           (fun p ty -> { p with Ast.param_ty = Some ty })
                           lam.Ast.params param_tys;
                     })
              in
              let* () =
                match typed with
                | Typed_ast.Lambda l -> unify let_span ret_ty l.Typed_ast.ret_ty
                | Typed_ast.Literal _ | Typed_ast.Variable _ | Typed_ast.Apply _
                | Typed_ast.Let _ | Typed_ast.If _ | Typed_ast.Match _
                | Typed_ast.Block _ | Typed_ast.Binary _ | Typed_ast.Unary _
                | Typed_ast.MemberAccess _ | Typed_ast.Range _
                | Typed_ast.Constructor _ ->
                    Ok ()
              in
              Ok typed
          | Ast.Lambda _ | Ast.Literal _ | Ast.Variable _ | Ast.Apply _
          | Ast.Let _ | Ast.If _ | Ast.Match _ | Ast.Block _ | Ast.Binary _
          | Ast.Unary _ | Ast.MemberAccess _ | Ast.Range _ | Ast.Constructor _
            ->
              infer_expr env let_body
        in
        let body_ty = Typed_ast.ty_of typed_body in
        let* () =
          match ann with
          | Some ann_ty -> unify let_span ann_ty body_ty
          | None -> Ok ()
        in
        let gen_ty = generalize env body_ty in
        let* env' = bind_name name gen_ty let_span env in
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
       | Ast.MemberAccess _ | Ast.Range _ | Ast.Constructor _ ) as e)
      :: rest ->
        let* typed_e = infer_expr env e in
        go env (typed_e :: acc) rest
  in
  go env [] exprs

and infer_binary env op left right span =
  let* typed_left = infer_expr env left in
  let* typed_right = infer_expr env right in
  let left_ty = Typed_ast.ty_of typed_left in
  let right_ty = Typed_ast.ty_of typed_right in
  let* ty =
    match op with
    | Ast.OpAdd | Ast.OpSub | Ast.OpMul | Ast.OpDiv -> (
        match (resolve left_ty, resolve right_ty) with
        | Ast.TyI8, Ast.TyI8 -> Ok Ast.TyI8
        | Ast.TyI16, Ast.TyI16 -> Ok Ast.TyI16
        | Ast.TyI32, Ast.TyI32 -> Ok Ast.TyI32
        | Ast.TyI64, Ast.TyI64 -> Ok Ast.TyI64
        | Ast.TyF32, Ast.TyF32 -> Ok Ast.TyF32
        | Ast.TyF64, Ast.TyF64 -> Ok Ast.TyF64
        | ( ((Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64) as concrete),
            Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ } ) ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty concrete in
            Ok concrete
        | ( ((Ast.TyF32 | Ast.TyF64) as concrete),
            Ast.TyFamilyMeta { Ast.family = Ast.FFloat; _ } ) ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty concrete in
            Ok concrete
        | ( (( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
             | Ast.TyF64 ) as concrete),
            Ast.TyFamilyMeta _ ) ->
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type mismatch: numeric family mismatch with '%s'"
                 (Ty_utils.string_of_ty concrete))
              (Typed_ast.span_of typed_right)
        | ( Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ },
            ((Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64) as concrete) ) ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty concrete in
            Ok concrete
        | ( Ast.TyFamilyMeta { Ast.family = Ast.FFloat; _ },
            ((Ast.TyF32 | Ast.TyF64) as concrete) ) ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty concrete in
            Ok concrete
        | ( Ast.TyFamilyMeta _,
            (( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
             | Ast.TyF64 ) as concrete) ) ->
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type mismatch: numeric family mismatch with '%s'"
                 (Ty_utils.string_of_ty concrete))
              (Typed_ast.span_of typed_left)
        | (Ast.TyFamilyMeta _ as fm1), (Ast.TyFamilyMeta _ as fm2) ->
            let* () = unify (Typed_ast.span_of typed_left) fm1 fm2 in
            Ok (resolve left_ty)
        | Ast.TyMeta _, Ast.TyMeta _ ->
            let fm = fresh_family_meta Ast.FInt in
            let* () = unify (Typed_ast.span_of typed_left) left_ty fm in
            let* () = unify (Typed_ast.span_of typed_right) right_ty fm in
            Ok (resolve left_ty)
        | ( Ast.TyMeta _,
            (Ast.TyFamilyMeta { Ast.family = Ast.FInt | Ast.FFloat; _ } as fm) )
          ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty fm in
            Ok (resolve left_ty)
        | ( (Ast.TyFamilyMeta { Ast.family = Ast.FInt | Ast.FFloat; _ } as fm),
            Ast.TyMeta _ ) ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty fm in
            Ok (resolve right_ty)
        | ( Ast.TyMeta _,
            (( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
             | Ast.TyF64 ) as concrete) ) ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty concrete in
            Ok concrete
        | ( (( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
             | Ast.TyF64 ) as concrete),
            Ast.TyMeta _ ) ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty concrete in
            Ok concrete
        | ( Ast.TyVar _,
            (Ast.TyFamilyMeta { Ast.family = Ast.FInt | Ast.FFloat; _ } as fm) )
          ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty fm in
            Ok fm
        | ( (Ast.TyFamilyMeta { Ast.family = Ast.FInt | Ast.FFloat; _ } as fm),
            Ast.TyVar _ ) ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty fm in
            Ok fm
        | ( Ast.TyVar _,
            (( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
             | Ast.TyF64 ) as concrete) ) ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty concrete in
            Ok concrete
        | ( (( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
             | Ast.TyF64 ) as concrete),
            Ast.TyVar _ ) ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty concrete in
            Ok concrete
        | Ast.TyVar _, Ast.TyVar _ ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty right_ty in
            Ok (resolve left_ty)
        | Ast.TyVar _, Ast.TyMeta _ ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty left_ty in
            Ok (resolve left_ty)
        | Ast.TyMeta _, Ast.TyVar _ ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty right_ty in
            Ok (resolve right_ty)
        | Ast.TyMeta _, got ->
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type mismatch: expected 'int or float', got '%s'"
                 (Ty_utils.string_of_ty got))
              (Typed_ast.span_of typed_right)
        | got, Ast.TyMeta _ ->
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type mismatch: expected 'int or float', got '%s'"
                 (Ty_utils.string_of_ty got))
              (Typed_ast.span_of typed_left)
        | ( ( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
            | Ast.TyF64
            | Ast.TyFamilyMeta { Ast.family = Ast.FInt | Ast.FFloat; _ } ),
            got ) ->
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type mismatch: expected '%s', got '%s'"
                 (Ty_utils.string_of_ty (resolve left_ty))
                 (Ty_utils.string_of_ty got))
              (Typed_ast.span_of typed_right)
        | Ast.TyString, _
        | Ast.TyBool, _
        | Ast.TyUnit, _
        | Ast.TyVar _, _
        | Ast.TyBoundVar _, _
        | Ast.TyModule _, _
        | Ast.TyList _, _
        | Ast.TyFunction _, _
        | Ast.TyInt, _
        | Ast.TyFloat, _
        | Ast.TyAdt _, _ ->
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type mismatch: expected 'int or float', got '%s'"
                 (Ty_utils.string_of_ty (resolve left_ty)))
              (Typed_ast.span_of typed_left))
    | Ast.OpEqual | Ast.OpNotEqual ->
        let* () =
          match unify (Typed_ast.span_of typed_right) left_ty right_ty with
          | Ok () -> Ok ()
          | Error _ ->
              type_error E_Type_TypeMismatch
                (Printf.sprintf "type mismatch: expected '%s', got '%s'"
                   (Ty_utils.string_of_ty (resolve left_ty))
                   (Ty_utils.string_of_ty (resolve right_ty)))
                (Typed_ast.span_of typed_right)
        in
        Ok Ast.TyBool
    | Ast.OpLess | Ast.OpGreater -> (
        match (resolve left_ty, resolve right_ty) with
        | Ast.TyI8, Ast.TyI8 -> Ok Ast.TyBool
        | Ast.TyI16, Ast.TyI16 -> Ok Ast.TyBool
        | Ast.TyI32, Ast.TyI32 -> Ok Ast.TyBool
        | Ast.TyI64, Ast.TyI64 -> Ok Ast.TyBool
        | Ast.TyF32, Ast.TyF32 -> Ok Ast.TyBool
        | Ast.TyF64, Ast.TyF64 -> Ok Ast.TyBool
        | ( ((Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64) as concrete),
            Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ } ) ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty concrete in
            Ok Ast.TyBool
        | ( ((Ast.TyF32 | Ast.TyF64) as concrete),
            Ast.TyFamilyMeta { Ast.family = Ast.FFloat; _ } ) ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty concrete in
            Ok Ast.TyBool
        | ( (( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
             | Ast.TyF64 ) as concrete),
            Ast.TyFamilyMeta _ ) ->
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type mismatch: numeric family mismatch with '%s'"
                 (Ty_utils.string_of_ty concrete))
              (Typed_ast.span_of typed_right)
        | ( Ast.TyFamilyMeta { Ast.family = Ast.FInt; _ },
            ((Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64) as concrete) ) ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty concrete in
            Ok Ast.TyBool
        | ( Ast.TyFamilyMeta { Ast.family = Ast.FFloat; _ },
            ((Ast.TyF32 | Ast.TyF64) as concrete) ) ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty concrete in
            Ok Ast.TyBool
        | ( Ast.TyFamilyMeta _,
            (( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
             | Ast.TyF64 ) as concrete) ) ->
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type mismatch: numeric family mismatch with '%s'"
                 (Ty_utils.string_of_ty concrete))
              (Typed_ast.span_of typed_left)
        | (Ast.TyFamilyMeta _ as fm1), (Ast.TyFamilyMeta _ as fm2) ->
            let* () = unify (Typed_ast.span_of typed_left) fm1 fm2 in
            Ok Ast.TyBool
        | Ast.TyMeta _, Ast.TyMeta _ ->
            let fm = fresh_family_meta Ast.FInt in
            let* () = unify (Typed_ast.span_of typed_left) left_ty fm in
            let* () = unify (Typed_ast.span_of typed_right) right_ty fm in
            Ok Ast.TyBool
        | ( Ast.TyMeta _,
            (Ast.TyFamilyMeta { Ast.family = Ast.FInt | Ast.FFloat; _ } as fm) )
          ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty fm in
            Ok Ast.TyBool
        | ( (Ast.TyFamilyMeta { Ast.family = Ast.FInt | Ast.FFloat; _ } as fm),
            Ast.TyMeta _ ) ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty fm in
            Ok Ast.TyBool
        | ( Ast.TyMeta _,
            (( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
             | Ast.TyF64 ) as concrete) ) ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty concrete in
            Ok Ast.TyBool
        | ( (( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
             | Ast.TyF64 ) as concrete),
            Ast.TyMeta _ ) ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty concrete in
            Ok Ast.TyBool
        | ( Ast.TyVar _,
            (Ast.TyFamilyMeta { Ast.family = Ast.FInt | Ast.FFloat; _ } as fm) )
          ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty fm in
            Ok Ast.TyBool
        | ( (Ast.TyFamilyMeta { Ast.family = Ast.FInt | Ast.FFloat; _ } as fm),
            Ast.TyVar _ ) ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty fm in
            Ok Ast.TyBool
        | ( Ast.TyVar _,
            (( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
             | Ast.TyF64 ) as concrete) ) ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty concrete in
            Ok Ast.TyBool
        | ( (( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
             | Ast.TyF64 ) as concrete),
            Ast.TyVar _ ) ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty concrete in
            Ok Ast.TyBool
        | Ast.TyVar _, Ast.TyVar _ ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty right_ty in
            Ok Ast.TyBool
        | Ast.TyVar _, Ast.TyMeta _ ->
            let* () = unify (Typed_ast.span_of typed_right) right_ty left_ty in
            Ok Ast.TyBool
        | Ast.TyMeta _, Ast.TyVar _ ->
            let* () = unify (Typed_ast.span_of typed_left) left_ty right_ty in
            Ok Ast.TyBool
        | Ast.TyMeta _, got ->
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type mismatch: expected 'int or float', got '%s'"
                 (Ty_utils.string_of_ty got))
              (Typed_ast.span_of typed_right)
        | got, Ast.TyMeta _ ->
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type mismatch: expected 'int or float', got '%s'"
                 (Ty_utils.string_of_ty got))
              (Typed_ast.span_of typed_left)
        | ( ( Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyF32
            | Ast.TyF64
            | Ast.TyFamilyMeta { Ast.family = Ast.FInt | Ast.FFloat; _ } ),
            got ) ->
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type mismatch: expected '%s', got '%s'"
                 (Ty_utils.string_of_ty (resolve left_ty))
                 (Ty_utils.string_of_ty got))
              (Typed_ast.span_of typed_right)
        | Ast.TyString, _
        | Ast.TyBool, _
        | Ast.TyUnit, _
        | Ast.TyVar _, _
        | Ast.TyBoundVar _, _
        | Ast.TyModule _, _
        | Ast.TyList _, _
        | Ast.TyFunction _, _
        | Ast.TyInt, _
        | Ast.TyFloat, _
        | Ast.TyAdt _, _ ->
            type_error E_Type_TypeMismatch
              (Printf.sprintf "type mismatch: expected 'int or float', got '%s'"
                 (Ty_utils.string_of_ty (resolve left_ty)))
              (Typed_ast.span_of typed_left))
    | Ast.OpCons ->
        let elem = fresh_meta () in
        let* () = unify (Typed_ast.span_of typed_left) left_ty elem in
        let* () =
          unify (Typed_ast.span_of typed_right) right_ty (Ast.TyList elem)
        in
        Ok (Ast.TyList elem)
  in
  Ok
    (Typed_ast.Binary
       {
         Typed_ast.binary_op = op;
         Typed_ast.left = typed_left;
         Typed_ast.right = typed_right;
         Typed_ast.binary_ty = resolve ty;
         Typed_ast.binary_span = span;
       })

let rec monomorphize_expr e =
  match e with
  | Typed_ast.Literal (lit, ty, span) ->
      Typed_ast.Literal (lit, monomorphize_ty ty, span)
  | Typed_ast.Variable (name, ty, span) ->
      Typed_ast.Variable (name, monomorphize_ty ty, span)
  | Typed_ast.Lambda lam ->
      Typed_ast.Lambda
        {
          lam with
          Typed_ast.params =
            List.map
              (fun p ->
                {
                  p with
                  Typed_ast.param_ty = monomorphize_ty p.Typed_ast.param_ty;
                })
              lam.Typed_ast.params;
          Typed_ast.ret_ty = monomorphize_ty lam.Typed_ast.ret_ty;
          Typed_ast.lambda_ty = monomorphize_ty lam.Typed_ast.lambda_ty;
          Typed_ast.lambda_body = monomorphize_expr lam.Typed_ast.lambda_body;
        }
  | Typed_ast.Apply (f, args, ty, span) ->
      Typed_ast.Apply
        ( monomorphize_expr f,
          List.map monomorphize_expr args,
          monomorphize_ty ty,
          span )
  | Typed_ast.Let { Typed_ast.name; let_ty; let_body; let_span } ->
      Typed_ast.Let
        {
          Typed_ast.name;
          let_ty = monomorphize_ty let_ty;
          let_body = monomorphize_expr let_body;
          let_span;
        }
  | Typed_ast.If { Typed_ast.cond; then_; else_; if_ty; if_span } ->
      Typed_ast.If
        {
          Typed_ast.cond = monomorphize_expr cond;
          then_ = monomorphize_expr then_;
          else_ = Option.map monomorphize_expr else_;
          if_ty = monomorphize_ty if_ty;
          if_span;
        }
  | Typed_ast.Match { Typed_ast.scrutinee; cases; match_ty; match_span } ->
      Typed_ast.Match
        {
          Typed_ast.scrutinee = monomorphize_expr scrutinee;
          cases =
            List.map
              (fun c ->
                {
                  c with
                  Typed_ast.case_body = monomorphize_expr c.Typed_ast.case_body;
                  guard = Option.map monomorphize_expr c.Typed_ast.guard;
                })
              cases;
          match_ty = monomorphize_ty match_ty;
          match_span;
        }
  | Typed_ast.Block (exprs, ty, span) ->
      Typed_ast.Block
        (List.map monomorphize_expr exprs, monomorphize_ty ty, span)
  | Typed_ast.Binary
      { Typed_ast.binary_op; left; right; binary_ty; binary_span } ->
      Typed_ast.Binary
        {
          Typed_ast.binary_op;
          left = monomorphize_expr left;
          right = monomorphize_expr right;
          binary_ty = monomorphize_ty binary_ty;
          binary_span;
        }
  | Typed_ast.Unary { Typed_ast.unary_op; expr; unary_ty; unary_span } ->
      Typed_ast.Unary
        {
          Typed_ast.unary_op;
          expr = monomorphize_expr expr;
          unary_ty = monomorphize_ty unary_ty;
          unary_span;
        }
  | Typed_ast.MemberAccess (obj, name, ty, span) ->
      Typed_ast.MemberAccess
        (monomorphize_expr obj, name, monomorphize_ty ty, span)
  | Typed_ast.Range (a, b, span) ->
      Typed_ast.Range (monomorphize_expr a, monomorphize_expr b, span)
  | Typed_ast.Constructor (name, args, ty, span) ->
      Typed_ast.Constructor
        (name, List.map monomorphize_expr args, monomorphize_ty ty, span)

let monomorphize_program prog =
  { prog with Typed_ast.body = List.map monomorphize_expr prog.Typed_ast.body }

let check_literal_ranges prog =
  let rec check_expr e =
    match e with
    | Typed_ast.Literal (Ast.LInt v, ty, span) ->
        let ty = resolve ty in
        let in_range lo hi =
          Int64.compare v lo >= 0 && Int64.compare v hi <= 0
        in
        let ok =
          match ty with
          | Ast.TyI8 -> in_range (-128L) 127L
          | Ast.TyI16 -> in_range (-32768L) 32767L
          | Ast.TyI32 -> in_range (-2147483648L) 2147483647L
          | Ast.TyI64 | Ast.TyFamilyMeta _ | Ast.TyVar _ | Ast.TyBoundVar _ ->
              true
          | Ast.TyInt | Ast.TyString | Ast.TyBool | Ast.TyUnit | Ast.TyModule _
          | Ast.TyList _ | Ast.TyFunction _ | Ast.TyFloat | Ast.TyAdt _
          | Ast.TyF32 | Ast.TyF64 | Ast.TyMeta _ ->
              true
        in
        if not ok then
          type_error E_Type_TypeMismatch
            (Printf.sprintf "integer literal %Ld overflows type '%s'" v
               (Ty_utils.string_of_ty ty))
            span
        else Ok ()
    | Typed_ast.Literal (Ast.LFloat _, _, _)
    | Typed_ast.Literal (Ast.LString _, _, _)
    | Typed_ast.Literal (Ast.LBool _, _, _)
    | Typed_ast.Literal (Ast.LUnit, _, _)
    | Typed_ast.Literal (Ast.LNil, _, _)
    | Typed_ast.Variable _ ->
        Ok ()
    | Typed_ast.Lambda lam -> check_expr lam.Typed_ast.lambda_body
    | Typed_ast.Apply (f, args, _, _) ->
        let* () = check_expr f in
        List.fold_left
          (fun acc e ->
            let* () = acc in
            check_expr e)
          (Ok ()) args
    | Typed_ast.Let { Typed_ast.let_body; _ } -> check_expr let_body
    | Typed_ast.If { Typed_ast.cond; then_; else_; _ } -> (
        let* () = check_expr cond in
        let* () = check_expr then_ in
        match else_ with Some e -> check_expr e | None -> Ok ())
    | Typed_ast.Match { Typed_ast.scrutinee; cases; _ } ->
        let* () = check_expr scrutinee in
        List.fold_left
          (fun acc c ->
            let* () = acc in
            let* () = check_expr c.Typed_ast.case_body in
            match c.Typed_ast.guard with
            | Some g -> check_expr g
            | None -> Ok ())
          (Ok ()) cases
    | Typed_ast.Block (exprs, _, _) ->
        List.fold_left
          (fun acc e ->
            let* () = acc in
            check_expr e)
          (Ok ()) exprs
    | Typed_ast.Binary { Typed_ast.left; right; _ } ->
        let* () = check_expr left in
        check_expr right
    | Typed_ast.Unary { Typed_ast.expr; _ } -> check_expr expr
    | Typed_ast.MemberAccess (obj, _, _, _) -> check_expr obj
    | Typed_ast.Range (a, b, _) ->
        let* () = check_expr a in
        check_expr b
    | Typed_ast.Constructor (_, args, _, _) ->
        List.fold_left
          (fun acc e ->
            let* () = acc in
            check_expr e)
          (Ok ()) args
  in
  List.fold_left
    (fun acc e ->
      let* () = acc in
      check_expr e)
    (Ok ()) prog.Typed_ast.body

let infer_program program =
  let* env = process_imports Env.empty program.Ast.imports in
  let* env =
    List.fold_left
      (fun acc type_decl ->
        let* env = acc in
        List.fold_left
          (fun acc variant ->
            let* env = acc in
            let ty =
              match variant.Ast.variant_fields with
              | [] -> Ast.TyAdt type_decl.Ast.type_name
              | fields ->
                  Ast.TyFunction (fields, Ast.TyAdt type_decl.Ast.type_name)
            in
            bind_name variant.Ast.variant_name ty variant.Ast.variant_span env)
          (Ok env) type_decl.Ast.variants)
      (Ok env) program.Ast.type_decls
  in
  let rec go env acc = function
    | [] -> Ok (List.rev acc)
    | Ast.Let { Ast.name; Ast.ty = ann; Ast.let_body; Ast.let_span } :: rest ->
        let pre_ty = fresh_meta () in
        let* pre_env = bind_name name pre_ty let_span env in
        let* typed_body = infer_expr pre_env let_body in
        let body_ty = Typed_ast.ty_of typed_body in
        let* () = unify let_span pre_ty body_ty in
        let* () =
          match ann with
          | Some ann_ty -> unify let_span ann_ty body_ty
          | None -> Ok ()
        in
        let gen_ty = generalize env body_ty in
        let* env' = bind_name name gen_ty let_span env in
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
       | Ast.MemberAccess _ | Ast.Range _ | Ast.Constructor _ ) as e)
      :: rest ->
        let* typed_e = infer_expr env e in
        go env (typed_e :: acc) rest
  in
  let* body = go env [] program.Ast.body in
  let typed_prog =
    {
      Typed_ast.imports = program.Ast.imports;
      Typed_ast.type_decls = program.Ast.type_decls;
      Typed_ast.body;
    }
  in
  let* () = check_literal_ranges typed_prog in
  Ok (monomorphize_program typed_prog)

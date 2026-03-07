let string_of_binop = function
  | Ast.OpAdd -> "+"
  | Ast.OpSub -> "-"
  | Ast.OpMul -> "*"
  | Ast.OpDiv -> "/"
  | Ast.OpEqual -> "=="
  | Ast.OpNotEqual -> "<>"
  | Ast.OpLess -> "<"
  | Ast.OpGreater -> ">"
  | Ast.OpCons -> "::"

let string_of_unop = function Ast.OpNegate -> "-"

let string_of_span (start_pos, end_pos) =
  Printf.sprintf "[%d:%d-%d:%d]" start_pos.Lexing.pos_lnum
    (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
    end_pos.Lexing.pos_lnum
    (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol)

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

let generalize_display ty =
  let rec free_metas acc t =
    match resolve t with
    | Ast.TyMeta { Ast.id; _ } -> if List.mem id acc then acc else acc @ [ id ]
    | Ast.TyFamilyMeta { Ast.fid; _ } ->
        if List.mem fid acc then acc else acc @ [ fid ]
    | Ast.TyList t -> free_metas acc t
    | Ast.TyFunction (ps, r) -> free_metas (List.fold_left free_metas acc ps) r
    | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
    | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit
    | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyModule _ | Ast.TyAdt _ ->
        acc
  in
  let metas = free_metas [] ty in
  let mapping =
    List.mapi (fun i id -> (id, String.make 1 (Char.chr (97 + i)))) metas
  in
  let rec go t =
    match resolve t with
    | Ast.TyMeta { Ast.id; _ } -> (
        match List.assoc_opt id mapping with
        | Some name -> Ast.TyVar name
        | None -> t)
    | Ast.TyFamilyMeta { Ast.fid; Ast.family; _ } -> (
        match List.assoc_opt fid mapping with
        | Some name -> Ast.TyBoundVar (name, family)
        | None -> (
            match family with Ast.FInt -> Ast.TyI64 | Ast.FFloat -> Ast.TyF64))
    | Ast.TyList t -> Ast.TyList (go t)
    | Ast.TyFunction (ps, r) -> Ast.TyFunction (List.map go ps, go r)
    | Ast.TyInt | Ast.TyI8 | Ast.TyI16 | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat
    | Ast.TyF32 | Ast.TyF64 | Ast.TyString | Ast.TyBool | Ast.TyUnit
    | Ast.TyVar _ | Ast.TyBoundVar _ | Ast.TyModule _ | Ast.TyAdt _ ->
        t
  in
  go ty

let rec string_of_ty_inner = function
  | Ast.TyInt -> "int"
  | Ast.TyI8 -> "i8"
  | Ast.TyI16 -> "i16"
  | Ast.TyI32 -> "i32"
  | Ast.TyI64 -> "i64"
  | Ast.TyFloat -> "float"
  | Ast.TyF32 -> "f32"
  | Ast.TyF64 -> "f64"
  | Ast.TyString -> "string"
  | Ast.TyBool -> "bool"
  | Ast.TyUnit -> "unit"
  | Ast.TyVar v -> "'" ^ v
  | Ast.TyBoundVar (name, family) ->
      Printf.sprintf "'%s:%s" name
        (match family with Ast.FInt -> "int" | Ast.FFloat -> "float")
  | Ast.TyMeta mv -> (
      match !(mv.Ast.contents) with
      | None -> Printf.sprintf "?%d" mv.Ast.id
      | Some t -> string_of_ty_inner t)
  | Ast.TyFamilyMeta fm -> (
      match !(fm.Ast.fcontents) with
      | None -> (
          match fm.Ast.family with Ast.FInt -> "int" | Ast.FFloat -> "float")
      | Some t -> string_of_ty_inner t)
  | Ast.TyModule (namespace, name) -> namespace ^ ":" ^ name
  | Ast.TyList t -> Printf.sprintf "%s list" (string_of_ty_atom t)
  | Ast.TyFunction (args, ret) -> (
      match args with
      | [] -> string_of_ty_inner ret
      | _ ->
          Printf.sprintf "%s -> %s"
            (String.concat " -> " (List.map string_of_ty_atom args))
            (string_of_ty_inner ret))
  | Ast.TyAdt name -> name

and string_of_ty_atom ty =
  match ty with
  | Ast.TyFunction _ -> Printf.sprintf "(%s)" (string_of_ty_inner ty)
  | Ast.TyFamilyMeta _ | Ast.TyMeta _ | Ast.TyInt | Ast.TyI8 | Ast.TyI16
  | Ast.TyI32 | Ast.TyI64 | Ast.TyFloat | Ast.TyF32 | Ast.TyF64 | Ast.TyString
  | Ast.TyBool | Ast.TyUnit | Ast.TyList _ | Ast.TyVar _ | Ast.TyBoundVar _
  | Ast.TyModule _ | Ast.TyAdt _ ->
      string_of_ty_inner ty

let string_of_ty ty = string_of_ty_inner (generalize_display ty)

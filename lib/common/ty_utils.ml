let string_of_binop = function
  | Ast.OpAdd -> "+"
  | Ast.OpSub -> "-"
  | Ast.OpMul -> "*"
  | Ast.OpDiv -> "/"
  | Ast.OpEqual -> "=="
  | Ast.OpNotEqual -> "!="
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
  | _ -> ty

let generalize_display ty =
  let rec free_metas acc t =
    match resolve t with
    | Ast.TyMeta { Ast.id; _ } -> if List.mem id acc then acc else acc @ [ id ]
    | Ast.TyList t -> free_metas acc t
    | Ast.TyFunction (ps, r) -> free_metas (List.fold_left free_metas acc ps) r
    | _ -> acc
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
    | Ast.TyList t -> Ast.TyList (go t)
    | Ast.TyFunction (ps, r) -> Ast.TyFunction (List.map go ps, go r)
    | _ -> t
  in
  go ty

let rec string_of_ty_inner = function
  | Ast.TyInt -> "int"
  | Ast.TyFloat -> "float"
  | Ast.TyString -> "string"
  | Ast.TyBool -> "bool"
  | Ast.TyUnit -> "unit"
  | Ast.TyVar v -> "'" ^ v
  | Ast.TyMeta mv -> (
      match !(mv.Ast.contents) with
      | None -> Printf.sprintf "?%d" mv.Ast.id
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

and string_of_ty_atom ty =
  match ty with
  | Ast.TyFunction _ -> Printf.sprintf "(%s)" (string_of_ty_inner ty)
  | _ -> string_of_ty_inner ty

let string_of_ty ty = string_of_ty_inner (generalize_display ty)

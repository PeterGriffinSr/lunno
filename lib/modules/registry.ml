open Lunno_common

type module_info = { exports : (string * Ast.ty) list }

let table : (string, (string, module_info) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 8

let register ~namespace ~name info =
  let ns =
    match Hashtbl.find_opt table namespace with
    | Some ns -> ns
    | None ->
        let ns = Hashtbl.create 8 in
        Hashtbl.add table namespace ns;
        ns
  in
  Hashtbl.replace ns name info

let find_module ~namespace ~name =
  match Hashtbl.find_opt table namespace with
  | None -> None
  | Some ns -> Hashtbl.find_opt ns name

open Lunno_common.Ast
module StringMap = Map.Make (String)

type t = ty StringMap.t

let empty = StringMap.empty
let mem name env = StringMap.mem name env
let add name ty env = StringMap.add name ty env
let find name env = StringMap.find_opt name env
let fold f env acc = StringMap.fold f env acc

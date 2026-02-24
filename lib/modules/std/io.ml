open Lunno_common.Ast

let exports =
  [
    ("print", TyFunction ([ TyVar "a" ], TyUnit));
    ("println", TyFunction ([ TyVar "a" ], TyUnit));
  ]

let () = Registry.register ~namespace:"std" ~name:"io" { Registry.exports }

open Lunno_common

let exports =
  [
    ("print", Ast.TyFunction ([ Ast.TyVar "a" ], Ast.TyUnit));
    ("println", Ast.TyFunction ([ Ast.TyVar "a" ], Ast.TyUnit));
  ]

let () = Registry.register ~namespace:"core" ~name:"io" { Registry.exports }

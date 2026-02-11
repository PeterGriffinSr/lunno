(lang dune 3.20)

(using menhir 3.0)

(name lunno)

(version 0.4.8)

(generate_opam_files true)

(source
 (github PeterGriffinSr/lunno))

(authors "Codezz-Ops")

(maintainers "Codezz-Ops")

(license MIT)

(documentation https://github.com/PeterGriffinSr/lunno)

(package
 (name lunno)
 (synopsis "A purely functional programming language")
 (description
  "Lunno is a purely functional, statically typed programming language.")
 (depends ocaml dune ounit2 menhir)
 (tags
  ("functional" "language" "typed" "static-typing")))

(lang dune 3.21)

(using menhir 3.0)

(name lunno)

(version 0.12.0)

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
 (depends
  (ocaml
   (and
    (>= 5.0.0)
    (< 5.5.0)))
  (dune
   (>= 3.21))
  (coq
   (>= 9.1.0)))
 (depopts
  (merlin :dev))
 (tags
  (lunno functional language compiler typed static-typing)))

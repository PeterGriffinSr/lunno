(lang dune 3.21)

(using menhir 3.0)

(name lunno)

(version 0.5.5)

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
   (>= 5.0.0))
  (dune
   (>= 3.21))
  (menhir
   (>= 20260209))
  (ounit2
   (and
    :with-test
    (>= 2.2.2.7))))
 (tags
  (functional language compiler typed static-typing)))

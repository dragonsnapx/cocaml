(lang dune 3.16)
(using menhir 3.0)
(generate_opam_files true)
(authors "Jake Lee" "Chase Feng")
(package
    (name runner)
    (allow_empty)
    (synopsis "A Ocaml-based compiler for C")
    (description "A Ocaml compiler for a subset of C")
    (depends
        (core (>= 0.17.1))
        (base (>= 0.17.1))
        (ppx_jane (>= 0.17.0))
        (ppx_deriving (>= 6.0.3))
        (ouint2 (>= 2.2.7))
        (menhir (>= 20240715))
        (llvm (>= 5.9.0))
    )
)
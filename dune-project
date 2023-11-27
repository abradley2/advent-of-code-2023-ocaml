(lang dune 3.11)

(name aoc_2023_ocaml)

(generate_opam_files true)

(source
 (github abradley2/advent-of-code-2023-ocaml))

(authors "Anthony Bradley")

(maintainers "Anthony Bradley")

(license MIT)

(package
 (name aoc_2023_ocaml)
 (synopsis "Advent of Code 2023 in OCaml")
 (depends
  (ocaml
   (= 5.1.0))
  (dune
   (= 3.11))
  ocamlformat
  ocaml-lsp-server
  (ocamlgraph
   (>= 2.1.0))
  (bark
   (>= 0.1.4))
  (base
   (>= 0.16.3))
  (stdio
   (>= 0.16.0))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

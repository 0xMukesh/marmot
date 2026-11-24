(lang dune 3.24)

(name marmot)

(generate_opam_files true)

(source
 (github 0xmukesh/marmot))

(authors "Mukesh <mukesh@0xc84.fyi>")

(maintainers "Mukesh <mukesh@0xc84.fyi>")

(license MIT)

(documentation https://github.com/0xmukesh/marmot)

(package
 (name marmot)
 (synopsis "An emulator for Uxn, written in OCaml")
 (description "An emulator for Uxn, written in OCaml")
 (depends
  ocaml
  ocaml-lsp-server
  ocamlformat
  ppx_deriving
  core-unix
  raylib)
 (tags
  ("uxn", "varvara")))

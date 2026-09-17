(lang dune 3.16)

(name kast)

(version 0.3.0)

(package
 (name kast)
 (depends
  (ocaml
   (>= 5.4))
  (ppx_deriving
   (>= 6))
  (ppx_getenv
   (>= 2))
  (linol
   (>= 0.10))
  (linol-eio
   (>= 0.10))
  (eio_main
   (>= 1))
  (js_of_ocaml
   (>= 6))
  (js_of_ocaml-compiler
   (>= 6))
  (js_of_ocaml-ppx
   (>= 6))
  (promise_jsoo
   (>= 0.4))
  (memtrace
   (>= 0.2))
  (uri
   (>= 4))))

(generate_opam_files)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

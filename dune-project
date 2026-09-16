(lang dune 3.16)

(name kast)

(version 0.3.0)

(package
 (name kast)
 (depends
  (ocaml
   (= 5.4.0))
  (ppx_deriving
   (= 6.1.1))
  (ppx_getenv
   (= 2.1))
  (linol
   (= 0.10))
  (linol-eio
   (= 0.10))
  (eio_main
   (= 1.3))
  (js_of_ocaml
   (= 6.2.0))
  (js_of_ocaml-compiler
   (= 6.2.0))
  (js_of_ocaml-ppx
   (= 6.2.0))
  (promise_jsoo
   (= "0.4.3"))
  (memtrace
   (= "0.2.3"))
  (uri
   (= "4.4.0"))))

(generate_opam_files)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

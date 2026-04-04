# Kast

An experimental programming language

This is a rewrite #4. You can find previous versions in

1. [`old-ocaml-version`](https://github.com/kast-lang/kast/tree/old-ocaml-version) branch
2. [`old-rust-version`](https://github.com/kast-lang/kast/tree/old-rust-version) branch
3. [`bootstrap-ocaml`](https://github.com/kast-lang/kast/tree/bootstrap-ocaml) branch
4. self-hosted - current version

The language is not **NOT READY YET**

[See more on the website](https://kast-lang.org)

## TODO

Self-hosted progress

- [x] Lexer
  - [x] Error resilience
- [x] Parser
  - [x] Error resilience
- [ ] Interperter & Compiler to IR
- [ ] LSP
  - [x] syntax highlighting
  - [x] formatting
  - [ ] inlay hints
  - [ ] hover info
  - [ ] goto def
  - [ ] autocomplete
  - [ ] find references
- [ ] Compilation targets:
  - [ ] Minikast as intermediate simpler language
  - [ ] JavaScript - easiest to implement
  - [ ] Go
  - [ ] C
  - [ ] LLVM - probably not until much later
  - [ ] Other alternatives to consider:
    - C--
    - QBE

# Kast

An experimental programming language

This is a rewrite #4. You can find previous versions in

1. [`old-ocaml-version`](https://github.com/kast-lang/kast/tree/old-ocaml-version) branch
2. [`old-rust-version`](https://github.com/kast-lang/kast/tree/old-rust-version) branch
3. bootstrap on [`main`](https://github.com/kast-lang/kast/tree/main) branch
4. self-hosted - current version

The language is not **NOT READY YET**

[See more on the website](https://kast-lang.org)

## TODO

Self-hosted progress

- [x] Lexer
  - [x] Error resilience
- [x] Parser
  - [ ] Error resilience
- [ ] Interperter & Compiler to IR
- [ ] LSP
  - [x] syntax highlighting
  - [ ] formatting
  - [ ] inlay hints
  - [ ] hover info
  - [ ] goto def
  - [ ] find references
- [ ] Compilation targets:
  - [ ] C - This is probably what we do
  - [ ] JavaScript??? - This is what bootstrap does
  - [ ] LLVM - probably not until much later
  - [ ] Other alternatives to consider:
    - C--
    - QBE
    - Go

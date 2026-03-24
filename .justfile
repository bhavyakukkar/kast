default:
    echo Hello there!
    just --list

build *args:
    mkdir -p target
    flock --exclusive target kast {{args}} compile \
        --js-ref-vars false \
        --async always \
        --use-numbers-instead-of-symbols false \
        --target js \
        --output target/main.mjs \
        src/main.ks

watch:
    #!/usr/bin/env bash
    trap 'kill $(jobs -p); exit' INT
    inotifywait -m -r -e modify,create,delete,moved_to src |
    while read -r directory events filename; do
        echo "Change detected: $filename ($events). Rebuilding..."
        just build
    done &
    just build
    wait

test:
    just build
    fd --type file --extension ks --exec-batch self-kast tokenize
    self-kast parse_syntax_rules tests/syntax/*.ks
    self-kast parse-json tests/test.json
    self-kast parse-json tests/lsp-init.json
    fd --type file --extension ks --exec-batch self-kast parse

default:
    echo Hello there!
    just --list

build *args:
    mkdir -p target
    # flock --exclusive target 
    time kast-bootstrap {{args}} compile \
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
    fd --type file --extension ks --exec-batch kast tokenize
    kast parse_syntax_rules tests/syntax/*.ks
    kast parse-json tests/json/*.json
    kast parse-json --use-kast-parser tests/json/*.json
    fd --type file --extension ks --exec-batch kast parse

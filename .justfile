default:
    echo Hello there!
    just --list

[arg("continuous", long="continuous", value="--continuous")]
build continuous="":
    mkdir -p target
    # flock --exclusive target 
    time ${KAST_BIN:-kast-bootstrap} compile \
        {{continuous}} \
        --js-ref-vars false \
        --async always \
        --use-numbers-instead-of-symbols false \
        --target js \
        --output target/kast.mjs \
        src/cli/_main.ks

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
    fd --type file --extension ks --exec-batch kast tokenize > /dev/null
    kast parse-syntax-rules tests/syntax/*.ks > /dev/null
    kast parse-json tests/json/*.json > /dev/null
    kast parse-json --use-kast-parser tests/json/*.json > /dev/null
    fd --type file --extension ks --exec-batch kast parse > /dev/null

lsp-stress-test:
    kast-bootstrap --target js lsp-stress-test/main.ks | kast lsp
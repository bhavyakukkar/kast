use (import "./source.ks").*;
use (import "./source_path.ks").*;

const is_set = (var :: String) => (
    match std.sys.get_env(var) with (
        | :Some c => String.length(c) > 0
        | :None => false
    )
);

const SyntaxSource = newtype (
    # Source that will be read at runtime from a path
    | :Path String
    # Source that has already been read at compile-time
    | :Static Source
);

impl SyntaxSource as module = (
    module:

    const new = (path :: String, at_comptime :: Bool) -> SyntaxSource => (
        if at_comptime then (
            :Static Source.read(SourcePath.file(path))
        ) else (
            :Path path
        )
    );

    const to_source = (self :: SyntaxSource) -> Source => (
        match self with (
            | :Path path => Source.read(SourcePath.file(path))
            | :Static source => source
        )
    );

    const path = (self :: &SyntaxSource) -> String => (
        match self with (
            | &(:Path path) => path
            | &(:Static ref source) => source^.path |> to_string
        )
    );
);

module:

const SyntaxSource = SyntaxSource;
const kast_syntax = SyntaxSource.new("std/syntax.ks", is_set("READONLY_KAST_SYNTAX"));
const minikast_syntax = SyntaxSource.new("src/mini/syntax.ks", is_set("READONLY_MINIKAST_SYNTAX"));
const json_syntax = SyntaxSource.new("src/json/syntax.ks", is_set("READONLY_JSON_SYNTAX"));

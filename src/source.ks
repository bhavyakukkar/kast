use (import "../deps/uri/src/lib.ks").*;
use (import "./position.ks").*;
use (import "./span.ks").*;
use (import "./common.ks").*;
use (import "./source_path.ks").*;

module:

const Source = newtype {
    .contents :: String,
    .path :: SourcePath,
};

impl Source as module = (
    module:

    const read = (path :: SourcePath) -> Source => (
        let contents = match path with (
            | :Stdin => std.io.stdin.read_to_end()
            | :Uri uri => if uri.scheme == "file" then (
                std.fs.read_file(uri.path)
            ) else (
                panic("unsupported uri scheme " + String.escape(uri.scheme))
            )
        );
        {
            .contents,
            .path,
        }
    );

    const entire_span = (self :: &Source) -> Span => {
        .start = Position.beginning(),
        .end = (
            let mut position = Position.beginning();
            for c in String.iter(self^.contents) do (
                &mut position |> Position.advance(c);
            );
            position
        ),
        .path = self^.path,
    };
);
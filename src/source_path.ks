use (import "../deps/uri/src/lib.ks").*;

module:

const SourcePath = newtype (
    | :Stdin
    | :Uri Uri
    | :Special String
);

impl SourcePath as module = (
    module:

    const file = (path :: String) -> SourcePath => (
        :Uri (
            # TODO resolve to absolute
            Uri.spec_path("file", path)
        )
    );
);

impl SourcePath as ToString = {
    .to_string = (self :: SourcePath) => match self with (
        | :Stdin => "<stdin>"
        | :Uri uri => Uri.to_string(uri)
        | :Special name => name
    ),
};
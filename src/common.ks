use (import "../deps/uri/src/lib.ks").*;

module:

const Path = String;

const FileOrStdin = newtype (
    | :Stdin
    | :File Path
);

impl FileOrStdin as module = (
    module:

    const read = (self :: FileOrStdin) -> String => (
        match self with (
            | :Stdin => std.io.stdin.read_to_end()
            | :File path => std.fs.read_file(path)
        )
    );
    const uri = (self :: FileOrStdin) -> Uri => (
        match self with (
            | :Stdin => parse("stdin://")
            | :File path => (
                # TODO resolve to full path? and file scheme
                Uri.new_path(path)
            )
        )
    );
);

impl FileOrStdin as ToString = {
    .to_string = (self :: FileOrStdin) => match self with (
        | :Stdin => "<stdin>"
        | :File path => path
    ),
}
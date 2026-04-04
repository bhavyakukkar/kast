use (import "../diagnostic.ks").*;

module:

const Common = (
    module:

    const Syntax = newtype {
        .ruleset :: String,
        .ext :: Option.t[String],
    };

    const path_arg_for_syntax = (path :: String, .@"syntax" :: Option.t[Syntax]) -> String => (
        let ext = match @"syntax" with (
            | :None => :Some "ks"
            | :Some s => s.ext
        );
        path_arg(path, .ext)
    );

    const path_arg = (path :: String, .ext :: Option.t[String]) -> String => (
        let fail = () => (
            let mut message = "Expected a path";
            if ext is :Some ext then (
                message += " with ." + ext + " extension";
            );
            message += ", got " + String.escape(path);
            Diagnostic.abort(message)
        );
        if String.length(path) == 0 then fail();
        if path |> String.at(0) == '-' then fail();
        let last_dot = path |> String.last_index_of('.');
        if last_dot < 0 then fail();
        let actual_ext = path |> String.substring_from(last_dot + 1);
        if ext is :Some ext then (
            if actual_ext != ext then fail();
        );
        path
    );

    const Args = (
        module:

        const t = newtype {
            .output_mode :: (
                | :Human
                | :Json
            ),
            .stop_on_error :: Bool,
            .color :: Bool,
        };

        const default = () -> Args.t => {
            .output_mode = :Human,
            .stop_on_error = true,
            .color = true,
        };

        const parse_arg = (
            args :: &mut Args.t,
            arg_idx :: &mut Int32,
        ) => with_return (
            let arg = std.sys.argv_at(arg_idx^);
            if arg == "--output-mode" then (
                let mode = std.sys.argv_at(arg_idx^ + 1);
                let mode = if mode == "human" then (
                    :Human
                ) else if mode == "json" then (
                    :Json
                ) else (
                    Diagnostic.abort("Unknown output mode " + String.escape(mode))
                );
                args^.output_mode = mode;
                arg_idx^ += 2;
                return;
            );
            if arg == "--continue-on-error" then (
                args^.stop_on_error = false;
                arg_idx^ += 1;
                return;
            );
            if arg == "--color" then (
                args^.color = String.parse(std.sys.argv_at(arg_idx^ + 1));
                arg_idx^ += 2;
                return;
            );
            Diagnostic.abort("Unexpected arg " + arg);
        );
    );
);

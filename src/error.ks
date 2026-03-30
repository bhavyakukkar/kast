use (import "./output.ks").*;
use (import "./span.ks").*;

module:

const Error = (
    module:

    const Kind = newtype (
        | :Lexer
        | :Parser
        ## Internal error is a bug in the implementation of kast
        | :Internal
        | :Other
    );

    const Handler = newtype {
        .stop_on_error :: Bool,
        .handle :: (Kind, Span, () -> ()) -> (),
    };

    const HandlerContext = @context Handler;

    const UnwindableHandler = @context newtype {
        .unwind_on_error :: [T] () -> T,
    };

    const init_handler = (.stop_on_error :: Bool) -> Handler => {
        .stop_on_error,
        .handle = (kind, span, message) => (
            let output = @current Output;
            ansi.with_mode(
                :Red,
                () => (
                    let kind_name = match kind with (
                        | :Internal => :Some "Internal"
                        | :Lexer => :Some "Lexer"
                        | :Parser => :Some "Parser"
                        | :Other => :None
                    );
                    match kind_name with (
                        | :Some kind_name => (
                            output.write(kind_name);
                            output.write(" error at ");
                        )
                        | :None => (
                            output.write("Error at ");
                        )
                    );
                    span |> Span.print;
                    output.write(":\n");
                    message();
                    output.write("\n\n");
                ),
            );
            if stop_on_error then (
                std.sys.exit(-1);
            );
        )
    };

    const report_msg = (kind :: Kind, span :: Span, message :: String) => (
        report(kind, span, () => (@current Output).write(message))
    );

    const report = (kind :: Kind, span :: Span, message :: () -> ()) => (
        (@current HandlerContext).handle(kind, span, message);
    );

    const report_and_unwind = [T] (kind :: Kind, span :: Span, message :: () -> ()) -> T => (
        (@current HandlerContext).handle(kind, span, message);
        (@current UnwindableHandler).unwind_on_error()
    );
);

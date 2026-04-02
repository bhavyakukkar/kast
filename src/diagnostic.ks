use (import "./output.ks").*;
use (import "./span.ks").*;

module:

const Diagnostic = (
    module:

    const t = newtype {
        .severity :: Severity,
        .source :: Source,
        .span :: Span,
        .message :: () -> (),
        .related :: ArrayList.t[RelatedInfo]
    };

    const RelatedInfo = newtype {
        .span :: Span,
        .message :: () -> (),
    };

    const Severity = newtype (
        | :Error
        | :Warning
        | :Info
        | :Hint
    );

    const Source = newtype (
        | :Lexer
        | :Parser
        | :Compiler
        ## Internal error is a bug in the implementation of kast
        | :Internal
        | :Other
    );

    const Handler = newtype {
        .stop_on_error :: Bool,
        .handle :: Diagnostic.t -> (),
    };

    const HandlerContext = @context Handler;

    const UnwindableHandler = @context newtype {
        .unwind_on_error :: [T] () -> T,
    };

    const default_handler = (.stop_on_error :: Bool) -> Handler => {
        .stop_on_error,
        .handle = (diagnostic :: Diagnostic.t) => (
            let output = @current Output;
            ansi.with_mode(
                :Red,
                () => (
                    let source_name = match diagnostic.source with (
                        | :Internal => :Some "Internal"
                        | :Lexer => :Some "Lexer"
                        | :Parser => :Some "Parser"
                        | :Compiler => :Some "Compiler"
                        | :Other => :None
                    );
                    match source_name with (
                        | :Some source_name => (
                            output.write(source_name);
                            output.write(" error at ");
                        )
                        | :None => (
                            output.write("Error at ");
                        )
                    );
                    diagnostic.span |> Span.print;
                    output.write(":\n");
                    diagnostic.message();
                    for info in diagnostic.related |> ArrayList.into_iter do (
                        output.write("\n");
                        ansi.with_mode(
                            :Dim,
                            () => (
                                output.write("Note: at ");
                                Span.print(info.span);
                                output.write("\n");
                            ),
                        );
                        info.message();
                    );
                    output.write("\n\n");
                ),
            );
            if stop_on_error then (
                std.sys.exit(-1);
            );
        ),
    };

    const report = (diagnostic :: Diagnostic.t) => (
        (@current HandlerContext).handle(diagnostic);
    );

    const report_and_unwind = [T] (diagnostic :: Diagnostic.t) -> T => (
        (@current HandlerContext).handle(diagnostic);
        (@current UnwindableHandler).unwind_on_error()
    );
);

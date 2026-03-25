use (import "./common.ks").*;
use (import "./output.ks").*;
use (import "./span.ks").*;

module:

const Token = (
    module:
    
    const t = newtype {
        .shape :: Token.Shape.t,
        .span :: Span,
    };
    
    const print_impl = (
        self :: Token.t,
        .verbose :: Bool,
    ) => (
        let output = @current Output;
        self.shape |> Token.Shape.print_impl(.verbose);
        ansi.with_mode(
            :Dim,
            () => (
                output.write(" at ");
                self.span |> Span.print;
            )
        );
    );

    const print = token => Token.print_impl(token, .verbose = false);
    
    const StringContent = newtype {
        .raw :: String,
        .contents :: String,
    };
    
    const InterpolatedStringPart = newtype (
        | :Content StringContent
        | :Interpolated {
            .tokens :: ArrayList.t[Token.t],
            .span :: Span,
        }
    );
    
    const InterpolatedStringShape = newtype {
        .delimiter :: String,
        .raw :: String,
        .parts :: ArrayList.t[InterpolatedStringPart],
    };
    
    const Shape = (
        module:
        
        const t = newtype (
            | :Comment {
                .raw :: String,
                .ty :: (:Line | :Block),
            }
            | :Punct {
                .raw :: String,
            }
            | :Ident {
                .raw :: String,
                .name :: String,
            }
            | :String {
                .raw :: String,
                .contents :: String,
            }
            | :InterpolatedString InterpolatedStringShape
            | :Number {
                .raw :: String,
            }
            | :Eof
            | :Error {
                .raw :: String,
            }
        );
        
        const raw = (self :: Token.Shape.t) -> String => match self with (
            | :Punct { .raw, ... } => raw
            | :Comment { .raw, ... } => raw
            | :Ident { .raw, ... } => raw
            | :String { .raw, ... } => raw
            | :InterpolatedString { .raw, ... } => raw
            | :Number { .raw, ... } => raw
            | :Error { .raw, ... } => raw
            | :Eof => ""
        );
        
        const print_impl = (
            self :: Token.Shape.t,
            .verbose :: Bool,
        ) => (
            let output = @current Output;
            match self with (
                | :Comment { .raw, ... } => (
                    for c in String.iter(raw) do (
                        if c == '\n' then (
                            ansi.with_mode(
                                :Cyan,
                                () => output.write("\\n"),
                            );
                        ) else (
                            ansi.with_mode(
                                :Gray,
                                () => output.write(to_string(c)),
                            );
                        );
                    );
                )
                | :Punct { .raw, ... } => (
                    ansi.with_mode(
                        :Magenta,
                        () => output.write(raw),
                    );
                )
                | :Ident { .raw, .name, ... } => (
                    if not verbose or raw == name then (
                        ansi.with_mode(
                            :Under,
                            () => output.write(raw),
                        );
                    ) else (
                        ansi.with_mode(
                            :Under,
                            () => output.write(raw),
                        );
                        output.write(" = ");
                        ansi.with_mode(
                            :Green,
                            () => output.write(escape_string(name)),
                        );
                    );
                )
                | :Number { .raw, ... } => (
                    ansi.with_mode(
                        :Italic,
                        () => output.write(raw),
                    );
                )
                | :String { .raw, .contents, ... } => (
                    ansi.with_mode(
                        :Green,
                        () => output.write(raw),
                    );
                    if verbose then (
                        output.write(" {\n");
                        output.inc_indentation();
                        output.write(".contents = ");
                        ansi.with_mode(
                            :Green,
                            () => output.write(escape_string(contents)),
                        );
                        output.write("\n");
                        output.dec_indentation();
                        output.write("}");
                    );
                )
                | :InterpolatedString { .delimiter, .parts = ref parts, ... } => (
                    if verbose then (
                        ansi.with_mode(
                            :Green,
                            () => (
                                output.write(delimiter);
                                output.write("interpolated");
                                output.write(delimiter);
                            ),
                        );
                        output.write(" {\n");
                        output.inc_indentation();
                    ) else (
                        ansi.with_mode(
                            :Green,
                            () => output.write(delimiter),
                        );
                    );
                    for part in parts |> ArrayList.iter do (
                        match part^ with (
                            | :Content { .raw, .contents, ... } => (
                                if verbose then (
                                    ansi.with_mode(
                                        :Green,
                                        () => (
                                            output.write(delimiter);
                                            output.write(raw);
                                            output.write(delimiter);
                                        ),
                                    );
                                    output.write(" {\n");
                                    output.inc_indentation();
                                    output.write(".contents = ");
                                    ansi.with_mode(
                                        :Green,
                                        () => output.write(escape_string(contents)),
                                    );
                                    output.write("\n");
                                    output.dec_indentation();
                                    output.write("}");
                                ) else (
                                    ansi.with_mode(
                                        :Green,
                                        () => output.write(raw),
                                    );
                                );
                            )
                            | :Interpolated { .tokens = ref tokens, ... } => (
                                if verbose then (
                                    ansi.with_mode(
                                        :Yellow,
                                        () => output.write("\\"),
                                    );
                                    output.write(" {\n");
                                    output.inc_indentation();
                                    for token in tokens |> ArrayList.iter do (
                                        Token.Shape.print_impl(token^.shape, .verbose);
                                        output.write("\n");
                                    );
                                    output.dec_indentation();
                                    output.write("}");
                                ) else (
                                    ansi.with_mode(
                                        :Yellow,
                                        () => output.write("\\("),
                                    );
                                    let mut first = true;
                                    for token in tokens |> ArrayList.iter do (
                                        if first then (
                                            first = false;
                                        ) else (
                                            output.write(" ");
                                        );
                                        Token.Shape.print_impl(token^.shape, .verbose);
                                    );
                                    ansi.with_mode(
                                        :Yellow,
                                        () => output.write(")"),
                                    );
                                );
                            )
                        );
                        if verbose then (
                            output.write("\n");
                        );
                    );
                    if verbose then (
                        output.dec_indentation();
                        output.write("}");
                    ) else (
                        ansi.with_mode(
                            :Green,
                            () => output.write(delimiter),
                        );
                    );
                )
                | :Error { .raw, ... } => (
                    ansi.with_mode(
                        :Red,
                        () => output.write(raw),
                    );
                )
                | :Eof => (
                    ansi.with_mode(
                        :Italic,
                        () => output.write("<eof>"),
                    );
                )
            );
        );

        const print = shape => print_impl(shape, .verbose = false);
    );
);

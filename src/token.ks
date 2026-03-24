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
    
    const print = (self :: Token.t) => (
        let output = @current Output;
        self.shape |> Token.Shape.print;
        ansi.with_mode(
            :Dim,
            () => (
                output.write(" at ");
                self.span |> Span.print;
            )
        );
    );
    
    const StringContentPart = newtype {
        .raw :: String,
        .contents :: String,
    };
    
    const StringPart = newtype (
        | :Content StringContentPart
        | :Interpolated {
            .tokens :: ArrayList.t[Token.t],
            .span :: Span,
        }
    );
    
    const StringShape = newtype {
        .delimiter :: String,
        .raw :: String,
        .parts :: ArrayList.t[StringPart],
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
            | :String StringShape
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
            | :Number { .raw, ... } => raw
            | :Error { .raw, ... } => raw
            | :Eof => ""
        );
        
        const print = (self :: Token.Shape.t) => (
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
                        :Italic,
                        () => output.write(raw),
                    );
                )
                | :Ident { .raw, ... } => (
                    ansi.with_mode(
                        :Under,
                        () => output.write(raw),
                    );
                )
                | :Number { .raw, ... } => (
                    ansi.with_mode(
                        :Italic,
                        () => output.write(raw),
                    );
                )
                | :String { .delimiter, .parts = ref parts, ... } => (
                    ansi.with_mode(
                        :Green,
                        () => output.write(delimiter),
                    );
                    for part in parts |> ArrayList.iter do (
                        match part^ with (
                            | :Content { .raw, ... } => (
                                ansi.with_mode(
                                    :Green,
                                    () => output.write(raw),
                                );
                            )
                            | :Interpolated { .tokens = ref tokens, ... } => (
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
                                    Token.Shape.print(token^.shape)
                                );
                                ansi.with_mode(
                                    :Yellow,
                                    () => output.write(")"),
                                );
                            )
                        );
                    );
                    ansi.with_mode(
                        :Green,
                        () => output.write(delimiter),
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
    );
    
    ## get non-interpolated string contents
    const get_string_contents = ({ .parts, ... } :: StringShape) -> Option.t[String] => with_return (
        if &parts |> ArrayList.length == 1 then (
            let part = &parts |> ArrayList.at(0);
            if part^ is :Content { .contents, ... } then (
                return :Some contents;
            );
        );
        :None
    );
);

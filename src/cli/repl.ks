use (import "./common.ks").*;
use (import "../output.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../lexer.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../ast.ks").*;
use (import "../highlight.ks").*;
use (import "../diagnostic.ks").*;
use (import "../readline.ks").*;

module:

const Repl = (
    module:

    const Args = (
        module:

        const t = newtype {  };

        const default = () -> t => {} ;
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let ruleset_path = "tests/syntax/kast.ks";
        let mut lexer = Lexer.new(Source.read(SourcePath.file(ruleset_path)));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);
        let tokenize = contents => (
            with Diagnostic.HandlerContext = {
                .stop_on_error = false,
                .handle = diagnostic => (
                    # TODO show diagnostics under the repl line
                    # &mut diagnostics |> ArrayList.push_back(diagnostic);
                    let () = ();
                ),
            };
            let source = {
                .contents,
                .path = :Special "repl"
            };
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let mut ranges = ArrayList.new();
            loop (
                let token = &token_stream |> TokenStream.peek;
                if token.shape is :Eof then (
                    break;
                );
                &mut token_stream |> TokenStream.advance;
                let range = {
                    .start = token.span.start.string_encoding_index,
                    .end = token.span.end.string_encoding_index,
                };
                &mut ranges |> ArrayList.push_back(range);
            );
            ranges
        );
        let highlight = contents => (
            with Diagnostic.HandlerContext = {
                .stop_on_error = false,
                .handle = diagnostic => (
                    # TODO show diagnostics under the repl line
                    # &mut diagnostics |> ArrayList.push_back(diagnostic);
                    let () = ();
                ),
            };
            let source = {
                .contents,
                .path = :Special "repl"
            };
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let parsed = Parser.parse(
                .ruleset,
                .entire_source_span = Source.entire_span(&source),
                .path = source.path,
                .token_stream = &mut token_stream,
            );
            let mut result = "";
            with Output = new_output(
                .write = s => (
                    result += s;
                ),
                .indentation_string = "    ",
                .color = true,
            );
            Highlight.highlight(&parsed, Highlight.new_output(:Terminal));
            result
        );
        let prompt = output_to_string(
            () => (
                ansi.with_mode(
                    :Dim,
                    () => (@current Output).write("> "),
                )
            )
        );
        Readline.run(.tokenize, .highlight, .prompt);
    );
);

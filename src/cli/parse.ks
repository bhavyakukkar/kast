use (import "./common.ks").*;
use (import "../output.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../lexer.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../ast.ks").*;

module:

const Parse = (
    module:

    const Args = (
        module:

        const t = newtype {
            .ruleset_path :: Option.t[String],
            .paths :: ArrayList.t[String],
        };

        const parse = (
            start_index :: Int32,
            .fix_ruleset :: Option.t[String],
        ) -> t => (
            let mut paths = ArrayList.new();
            let mut i = start_index;
            let mut ruleset_path = fix_ruleset;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if arg == "--ruleset" and &fix_ruleset |> Option.is_none then (
                    if i + 1 >= std.sys.argc() then (
                        panic("Expected ruleset path");
                    );
                    ruleset_path = :Some std.sys.argv_at(i + 1);
                    i += 2;
                    continue;
                );
                &mut paths |> ArrayList.push_back(Common.ks_path_arg(arg));
                i += 1;
            );
            {
                .ruleset_path,
                .paths,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let ruleset_path = args.ruleset_path |> Option.unwrap_or("tests/syntax/kast.ks");
        ansi.with_mode(
            :Bold,
            () => (@current Output).write("Parsing syntax rules from " + ruleset_path + "\n\n"),
        );
        let mut lexer = Lexer.new(Source.read(SourcePath.file(ruleset_path)));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);
        let process = (path :: SourcePath) => (
            ansi.with_mode(
                :Bold,
                () => (@current Output).write("Parsing " + to_string(path) + "\n\n"),
            );
            let source = Source.read(path);
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let parsed = Parser.parse(
                .ruleset,
                .entire_source_span = Source.entire_span(&source),
                .path = source.path,
                .token_stream = &mut token_stream,
            );

            Ast.print(&parsed.ast);
            (@current Output).write("\n");
        );
        if &args.paths |> ArrayList.length == 0 then (
            process(:Stdin);
        );
        for path in args.paths |> ArrayList.into_iter do (
            process(SourcePath.file(path));
        );
    );
);

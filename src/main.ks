use (import "./common.ks").*;
use (import "./serialize.ks").*;
use (import "./diagnostic.ks").*;
use (import "./log.ks").*;
use (import "./output.ks").*;
use (import "./position.ks").*;
use (import "./source.ks").*;
use (import "./source_path.ks").*;
use (import "./lexer.ks").*;
use (import "./token.ks").*;
use (import "./token_stream.ks").*;
use (import "./syntax_parser.ks").*;
use (import "./syntax_ruleset.ks").*;
use (import "./ast.ks").*;
use (import "./parser.ks").*;
use (import "./json.ks").*;
use (import "./highlight.ks").*;
use (import "./lsp/lsp.ks").*;
use (import "./format.ks").*;
use (import "./structural_find_and_replace.ks").*;
use (import "./readline.ks").*;
const dep_json = import "../deps/json/lib.ks";
# @eval Serialize.do_impl();
with Stdout = new_std_output(std.io.stdout.write);
with Stderr = new_std_output(std.io.stderr.write);
with Output = (@current Stdout);

const Args = (
    module:

    const LexerArgs = (
        module:

        const t = newtype {
            .paths :: ArrayList.t[String],
        };

        const parse = start_index -> t => (
            let mut paths = ArrayList.new();
            for i in start_index..std.sys.argc() do (
                &mut paths |> ArrayList.push_back(std.sys.argv_at(i));
            );
            {
                .paths,
            }
        );
    );

    const ParseSyntaxRulesArgs = (
        module:

        const t = newtype {
            .paths :: ArrayList.t[String],
        };

        const parse = start_index -> t => (
            let mut paths = ArrayList.new();
            for i in start_index..std.sys.argc() do (
                &mut paths |> ArrayList.push_back(std.sys.argv_at(i));
            );
            {
                .paths,
            }
        );
    );

    const ParseArgs = (
        module:

        const t = newtype {
            .ruleset_path :: Option.t[String],
            .paths :: ArrayList.t[String],
        };

        const parse = start_index -> t => (
            let mut paths = ArrayList.new();
            let mut i = start_index;
            let mut ruleset_path = :None;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if arg == "--ruleset" then (
                    if i + 1 >= std.sys.argc() then (
                        panic("Expected ruleset path");
                    );
                    ruleset_path = :Some std.sys.argv_at(i + 1);
                    i += 2;
                    continue;
                );
                &mut paths |> ArrayList.push_back(arg);
                i += 1;
            );
            {
                .ruleset_path,
                .paths,
            }
        );
    );

    const ParseJsonArgs = (
        module:

        const t = newtype {
            .use_kast_parser :: Bool,
            .paths :: ArrayList.t[String],
        };

        const parse = start_index -> t => (
            let mut use_kast_parser = false;
            let mut paths = ArrayList.new();
            let mut i = start_index;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if arg == "--use-kast-parser" then (
                    use_kast_parser = true;
                    i += 1;
                    continue;
                );
                &mut paths |> ArrayList.push_back(arg);
                i += 1;
            );
            {
                .use_kast_parser,
                .paths,
            }
        );
    );

    const Subcommand = newtype (
        | :Tokenize LexerArgs.t
        | :ParseSyntaxRules ParseSyntaxRulesArgs.t
        | :ParseSyntaxRuleset ParseSyntaxRulesArgs.t
        | :Parse ParseArgs.t
        | :ParseJson ParseJsonArgs.t
        | :Highlight Highlight.Cli.Args.t
        | :Format Format.Cli.Args.t
        | :Lsp Lsp.CliArgs.t
        | :StructuralFindAndReplace StructuralFindAndReplace.Cli.Args.t
        | :Repl
    );

    const t = newtype {
        .subcommand :: Subcommand,
        .output_mode :: (
            | :Human
            | :Json
        ),
        .stop_on_error :: Bool,
        .color :: Bool,
    };

    const parse = () -> t => (
        let mut output_mode = :Human;
        let mut stop_on_error = true;
        let mut color = true;
        let subcommand = unwindable subcommand (
            let mut i = 1;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if arg == "lex" or arg == "tokenize" then (
                    unwind subcommand (:Tokenize LexerArgs.parse(i + 1));
                );
                if arg == "parse-syntax-rules" then (
                    unwind subcommand (:ParseSyntaxRules ParseSyntaxRulesArgs.parse(i + 1));
                );
                if arg == "parse-syntax-ruleset" then (
                    unwind subcommand (:ParseSyntaxRuleset ParseSyntaxRulesArgs.parse(i + 1));
                );
                if arg == "parse" then (
                    unwind subcommand (:Parse ParseArgs.parse(i + 1));
                );
                if arg == "parse-json" then (
                    unwind subcommand (:ParseJson ParseJsonArgs.parse(i + 1));
                );
                if arg == "highlight" then (
                    unwind subcommand (:Highlight Highlight.Cli.Args.parse(i + 1));
                );
                if arg == "fmt" or arg == "format" then (
                    unwind subcommand (:Format Format.Cli.Args.parse(i + 1));
                );
                if arg == "lsp" then (
                    unwind subcommand (:Lsp Lsp.CliArgs.parse(i + 1));
                );
                if arg == "find-ast" then (
                    unwind subcommand (:StructuralFindAndReplace StructuralFindAndReplace.Cli.Args.parse(i + 1));
                );
                if arg == "--output-mode" then (
                    let mode = std.sys.argv_at(i + 1);
                    let mode = if mode == "human" then (
                        :Human
                    ) else if mode == "json" then (
                        :Json
                    ) else (
                        panic("Unknown output mode " + String.escape(mode))
                    );
                    output_mode = mode;
                    i += 2;
                    continue;
                );
                if arg == "--continue-on-error" then (
                    stop_on_error = false;
                    i += 1;
                    continue;
                );
                if arg == "--color" then (
                    color = String.parse(std.sys.argv_at(i + 1));
                    i += 2;
                    continue;
                );
                panic("Unexpected arg " + arg);
            );
            :Repl
        );
        {
            .output_mode,
            .stop_on_error,
            .subcommand,
            .color,
        }
    );
);

let args = Args.parse();
(@current Stdout).color = args.color;
(@current Stderr).color = args.color;
with Diagnostic.HandlerContext = Diagnostic.default_handler(.stop_on_error = args.stop_on_error);
let output = @current Output;
match args.subcommand with (
    | :Tokenize { .paths } => (
        let process = (path :: SourcePath) => (
            Log.info(
                () => ansi.with_mode(
                    :Bold,
                    () => output.write("Lexing " + to_string(path) + "\n\n"),
                ),
            );
            match args.output_mode with (
                | :Human => (
                    let mut lexer = Lexer.new(Source.read(path));
                    loop (
                        let token = &mut lexer |> Lexer.next;
                        token |> Token.print_impl(.verbose = true);
                        output.write("\n");
                        if token.shape is :Eof then break;
                    );
                )
                | :Json => (
                    let mut lexer = Lexer.new(Source.read(path));
                    let mut json_tokens = ArrayList.new();
                    loop (
                        let token = &mut lexer |> Lexer.next;
                        &mut json_tokens |> ArrayList.push_back(Serialize.as_json(token));
                        if token.shape is :Eof then break;
                    );
                    let json = :Array json_tokens;
                    Json.print(&json);
                    output.write("\n");
                )
            );
        );
        if &paths |> ArrayList.length == 0 then (
            process(:Stdin);
        );
        for path in paths |> ArrayList.into_iter do (
            process(SourcePath.file(path));
        );
    )
    | :ParseSyntaxRules { .paths } => (
        let process = (path :: SourcePath) => (
            ansi.with_mode(:Bold, () => output.write("Parsing syntax rules from " + to_string(path) + "\n\n"));
            let mut lexer = Lexer.new(Source.read(path));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let rules = SyntaxParser.parse_syntax_rules(&mut token_stream);
            dbg.print(rules);
        );
        if &paths |> ArrayList.length == 0 then (
            process(:Stdin);
        );
        for path in paths |> ArrayList.into_iter do (
            process(SourcePath.file(path));
        );
    )
    | :ParseSyntaxRuleset { .paths } => (
        let mut ruleset = SyntaxRuleset.new();
        let process = (path :: SourcePath) => (
            ansi.with_mode(:Bold, () => output.write("Parsing syntax rules from " + to_string(path) + "\n\n"));
            let mut lexer = Lexer.new(Source.read(path));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let rules = SyntaxParser.parse_syntax_rules(&mut token_stream);
            for &rule in ArrayList.iter(&rules) do (
                &mut ruleset |> SyntaxRuleset.add(rule);
            );
        );
        if &paths |> ArrayList.length == 0 then (
            process(:Stdin);
        );
        for path in paths |> ArrayList.into_iter do (
            process(SourcePath.file(path));
        );
        SyntaxRuleset.print(&ruleset);
    )
    | :Parse { .ruleset_path, .paths } => (
        let ruleset_path = ruleset_path |> Option.unwrap_or("tests/syntax/kast.ks");
        ansi.with_mode(:Bold, () => output.write("Parsing syntax rules from " + ruleset_path + "\n\n"));
        let mut lexer = Lexer.new(Source.read(SourcePath.file(ruleset_path)));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);
        let process = (path :: SourcePath) => (
            ansi.with_mode(:Bold, () => output.write("Parsing " + to_string(path) + "\n\n"));
            let source = Source.read(path);
            let entire_source_span = (
                let start = Position.beginning();
                let mut end = start;
                for c in source.contents |> String.iter do (
                    &mut end |> Position.advance(c);
                );
                {
                    .start,
                    .end,
                    .path = source.path,
                }
            );
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let parsed = Parser.parse(
                .ruleset,
                .entire_source_span,
                .path = source.path,
                .token_stream = &mut token_stream,
            );

            Ast.print(&parsed.ast);
            (@current Output).write("\n");
        );
        if &paths |> ArrayList.length == 0 then (
            process(:Stdin);
        );
        for path in paths |> ArrayList.into_iter do (
            process(SourcePath.file(path));
        );
    )
    | :ParseJson { .use_kast_parser, .paths } => (
        let process = (path :: SourcePath) => (
            let source = Source.read(path);
            let json = if use_kast_parser then (
                let json = Json.parse(source.contents)
                    |> std.Result.unwrap_or_else(
                        error => panic("TODO Error happened")
                    );
                json
            ) else (
                let mut reader = dep_json.Reader.create(&source.contents);
                let json = dep_json.parse(&mut reader)
                    |> std.Result.unwrap_or_else(
                        error => panic("TODO Error happened")
                    );
                Json.from_dep(json)
            );
            Json.print(&json);
            (@current Output).write("\n");
        );
        if &paths |> ArrayList.length == 0 then (
            process(:Stdin);
        );
        for path in paths |> ArrayList.into_iter do (
            process(SourcePath.file(path));
        );
    )
    | :Highlight args => Highlight.Cli.run(args)
    | :Format args => Format.Cli.run(args)
    | :Lsp args => Lsp.run(args)
    | :StructuralFindAndReplace args => StructuralFindAndReplace.Cli.run(args)
    | :Repl => (
        let ruleset_path = "tests/syntax/kast.ks";
        let mut lexer = Lexer.new(Source.read(SourcePath.file(ruleset_path)));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);
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
        Readline.run(.highlight, .prompt);
    )
);

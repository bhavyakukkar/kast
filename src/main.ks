use (import "./common.ks").*;
use (import "./serialize.ks").*;
use (import "./error.ks").*;
use (import "./output.ks").*;
use (import "./position.ks").*;
use (import "./source.ks").*;
use (import "./lexer.ks").*;
use (import "./token.ks").*;
use (import "./token_stream.ks").*;
use (import "./syntax_parser.ks").*;
use (import "./syntax_ruleset.ks").*;
use (import "./ast.ks").*;
use (import "./parser.ks").*;
use (import "./json.ks").*;
const dep_json = import "../deps/json/lib.ks";

# @eval Serialize.do_impl();
with Output = stdout();

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
    );
    
    const t = newtype {
        .subcommand :: Subcommand,
        .output_mode :: (
            | :Human
            | :Json
        ),
        .stop_on_error :: Bool,
    };
    
    const parse = () -> t => (
        let mut output_mode = :Human;
        let mut stop_on_error = true;
        let subcommand = unwindable subcommand (
            let mut i = 1;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                if arg == "lex" or arg == "tokenize" then (
                    unwind subcommand (:Tokenize LexerArgs.parse(i + 1));
                );
                if arg == "parse_syntax_rules" then (
                    unwind subcommand (:ParseSyntaxRules ParseSyntaxRulesArgs.parse(i + 1));
                );
                if arg == "parse_syntax_ruleset" then (
                    unwind subcommand (:ParseSyntaxRuleset ParseSyntaxRulesArgs.parse(i + 1));
                );
                if arg == "parse" then (
                    unwind subcommand (:Parse ParseArgs.parse(i + 1));
                );
                if arg == "parse-json" then (
                    unwind subcommand (:ParseJson ParseJsonArgs.parse(i + 1));
                );
                if arg == "--output-mode" then (
                    let mode = std.sys.argv_at(i + 1);
                    let mode = if mode == "human" then (
                        :Human
                    ) else if mode == "json" then (
                        :Json
                    ) else (
                        panic("Unknown output mode " + escape_string(mode))
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
                panic("Unexpected arg " + arg);
            );
            panic("No default subcommand")
        );
        {
            .output_mode,
            .stop_on_error,
            .subcommand,
        }
    );
);

let args = Args.parse();
with Error.HandlerContext = Error.init_handler(.stop_on_error = args.stop_on_error);
let output = @current Output;
match args.subcommand with (
    | :Tokenize { .paths } => (
        for &path in ArrayList.iter(&paths) do (
            match args.output_mode with (
                | :Human => (
                    ansi.with_mode(:Bold, () => output.write("Lexing " + path + "\n\n"));
                    let mut lexer = Lexer.new(Source.read_file(path));
                    loop (
                        let token = &mut lexer |> Lexer.next;
                        token |> Token.print_impl(.verbose = true);
                        output.write("\n");
                        if token.shape is :Eof then break;
                    );
                )
                | :Json => (
                    let mut lexer = Lexer.new(Source.read_file(path));
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
    )
    | :ParseSyntaxRules { .paths } => (
        for &path in ArrayList.iter(&paths) do (
            ansi.with_mode(:Bold, () => output.write("Parsing syntax rules from " + path + "\n\n"));
            let mut lexer = Lexer.new(Source.read_file(path));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let rules = SyntaxParser.parse_syntax_rules(&mut token_stream);
            dbg.print(rules);
        );
    )
    | :ParseSyntaxRuleset { .paths } => (
        let mut ruleset = SyntaxRuleset.new();
        for &path in ArrayList.iter(&paths) do (
            ansi.with_mode(:Bold, () => output.write("Parsing syntax rules from " + path + "\n\n"));
            let mut lexer = Lexer.new(Source.read_file(path));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let rules = SyntaxParser.parse_syntax_rules(&mut token_stream);
            for &rule in ArrayList.iter(&rules) do (
                &mut ruleset |> SyntaxRuleset.add(rule);
            );
        );
        SyntaxRuleset.print(&ruleset);
    )
    | :Parse { .ruleset_path, .paths } => (
        let ruleset_path = ruleset_path |> Option.unwrap_or("tests/syntax/kast.ks");
        ansi.with_mode(:Bold, () => output.write("Parsing syntax rules from " + ruleset_path + "\n\n"));
        let mut lexer = Lexer.new(Source.read_file(ruleset_path));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);
        for &path in ArrayList.iter(&paths) do (
            ansi.with_mode(:Bold, () => output.write("Parsing " + path + "\n\n"));
            let source = Source.read_file(path);
            let entire_source_span = (
                let start = Position.beginning();
                let mut end = start;
                for c in source.contents |> String.iter do (
                    &mut end |> Position.advance(c);
                );
                {
                    .start,
                    .end,
                    .uri = source.uri,
                }
            );
            let mut lexer = Lexer.new(source);
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let parsed = Parser.parse(
                .ruleset,
                .entire_source_span,
                .uri = source.uri,
                .token_stream = &mut token_stream,
            );
            
            Ast.print(&parsed.ast);
            (@current Output).write("\n");
        );
    )
    | :ParseJson { .use_kast_parser, .paths } => (
        for path in paths |> ArrayList.into_iter do (
            let json = std.fs.read_file(path);
            let json = if use_kast_parser then (
                let json = Json.parse(json)
                    |> std.Result.unwrap_or_else(
                        error => panic("TODO Error happened")
                    );
                json
            ) else (
                let mut reader = dep_json.Reader.create(&json);
                let json = dep_json.parse(&mut reader)
                    |> std.Result.unwrap_or_else(
                        error => panic("TODO Error happened")
                    );
                Json.from_dep(json)
            );
            Json.print(&json);
            (@current Output).write("\n");
        );
    )
);

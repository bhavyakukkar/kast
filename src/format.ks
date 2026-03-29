use (import "./common.ks").*;
use (import "./tuple.ks").*;
use (import "./output.ks").*;
use (import "./parser.ks").*;
use (import "./source.ks").*;
use (import "./source_path.ks").*;
use (import "./lexer.ks").*;
use (import "./syntax_rule.ks").*;
use (import "./syntax_parser.ks").*;
use (import "./token_stream.ks").*;
use (import "./ast.ks").*;
use (import "./span.ks").*;
use (import "./position.ks").*;
use (import "./token.ks").*;

module:

const Format = (
    module:

    const ContextT = newtype {
        .queued_newlines :: Int32,
        .prev_span :: Span,
        .just_printed_newline :: Bool,
        .queued_indentation :: Int32,
    };

    const Context = @context ContextT;

    const flush = () => (
        let output = @current Output;
        let mut ctx = @current Context;
        for _ in 0..ctx.queued_newlines do (
            output.write("\n");
            ctx.just_printed_newline = true;
        );
        ctx.queued_newlines = 0;
        while ctx.queued_indentation < 0 do (
            output.dec_indentation();
            ctx.queued_indentation += 1;
        );
        while ctx.queued_indentation > 0 do (
            output.inc_indentation();
            ctx.queued_indentation -= 1;
        );
    );

    const print_raw = (raw :: String) => (
        flush();
        (@current Output).write(raw);
        (@current Context).just_printed_newline = false;
    );

    const inc_indentation = () => (
        (@current Context).queued_indentation += 1;
    );

    const dec_indentation = () => (
        (@current Context).queued_indentation -= 1;
    );

    const print_newline = () => (
        (@current Context).queued_newlines += 1;
    );

    const print_token = (token :: Token.t) => (
        let output = @current Output;
        let mut ctx = @current Context;
        if ctx.just_printed_newline and token.span.start.line - ctx.prev_span.end.line > 1 then (
            print_newline();
        );
        if token.shape is :Comment { .raw, .ty } then (
            if ctx.prev_span.end.line == token.span.start.line then (
                output.write(" ");
            );
            output.write(raw);
            ctx.just_printed_newline = false;
            match ty with (
                | :Line => (
                    print_newline();
                    flush();
                )
                | :Block => ()
            );
        ) else (
            flush();
            print_raw(Token.raw(token));
        );
        ctx.prev_span = token.span;
    );

    const format = (parsed :: &Parser.Parsed, output :: OutputT) => (
        with Context = {
            .queued_newlines = 0,
            .queued_indentation = 0,
            .just_printed_newline = true,
            .prev_span = Span.empty(
                .position = Position.beginning(),
                .path = parsed^.ast.span.path,
            ),
        };
        with Output = output;
        let {
            .ast = ref ast,
            .ignored_trailing_tokens = ref ignored_trailing_tokens,
        } = parsed^;
        walk_ast(ast, .parent = :None);
        walk_ignored_tokens(ignored_trailing_tokens);
        print_newline();
        flush();
    );

    const Parent = Option.t[type { .wrapped :: Bool, .priority :: SyntaxRule.Priority }];

    const walk_ast = (ast :: &Ast.t, .parent :: Parent) => (
        let {
            .ignored_tokens_before = ref ignored_tokens_before,
            .shape = ref shape,
            .span = _,
        } = ast^;
        walk_ignored_tokens(ignored_tokens_before);
        match shape^ with (
            | :Empty => ()
            | :Token token => print_token(token)
            | :InterpolatedString { .delimiter = _, .open, .parts, .close } => (
                print_token(open);
                for part in &parts |> ArrayList.iter do (
                    match part^ with (
                        | :Content { .raw, ... } => (
                            print_raw(raw);
                        )
                        | :Interpolated { .open, .close, .ast = ref inner } => (
                            print_token(open);
                            walk_ast(inner, .parent = :None);
                            print_token(close);
                        )
                    );
                );
                print_token(close);
            )
            | :Rule { .rule = ref rule, .root = ref root } => (
                walk_ast_group(
                    &rule^.parts,
                    root,
                    rule^.wrap_mode,
                    .priority = rule^.priority,
                    .parent,
                );
            )
            | :Syntax { .command, .value_after } => (
                for token in &command.raw_tokens |> ArrayList.iter do (
                    print_token(token^);
                );
                if value_after is :Some ref ast then (
                    walk_ast(ast, .parent = :None);
                );
            )
            | :Error { .parts } => (
                panic("Refusing to format code with errors")
            )
        )
    );
    
    const walk_ast_group = (
        rule_parts :: &ArrayList.t[SyntaxRule.Part],
        group :: &Ast.Group,
        wrap_mode :: SyntaxRule.WrapMode,
        .priority :: SyntaxRule.Priority,
        .parent :: Parent,
    ) => (
        let group_wrapped = group^.span.start.line != group^.span.end.line;
        let wrapped = match wrap_mode with (
            | :Never => false
            | :Always => true
            | :IfAnyNonAssociative => group_wrapped
            | :IfAnyAssociative => match parent with (
                | :Some parent => group_wrapped or (parent.wrapped and parent.priority == priority)
                | :None => group_wrapped
            )
        );
        let mut unnmed_child_idx = 0;
        let next_member = name => match name with (
            | :Some name => :Name name
            | :None => (
                let result = :Index unnmed_child_idx;
                unnmed_child_idx += 1;
                result
            )
        );
        let mut part_idx = 0;
        let expect_part = [T] (
            name :: String,
            f :: &Ast.Part -> Option.t[T],
        ) -> T => with_return (
            while part_idx < &group^.parts |> ArrayList.length do (
                let part = &group^.parts |> ArrayList.at(part_idx);
                if f(part) is :Some result then (
                    part_idx += 1;
                    return result;
                );
                if part^ is :Ignored token then (
                    walk_ignored_token(token);
                ) else (
                    panic("expected " + name);
                );
                part_idx += 1;
            );
            panic("expected more parts")
        );
        for part in rule_parts |> ArrayList.iter do (
            match part^ with (
                | :Keyword expected_keyword => with_return (
                    let keyword_token = expect_part(
                        "keyword",
                        part => match part^ with (
                            | :Keyword token => :Some token
                            | _ => :None
                        )
                    );
                    if expected_keyword != Token.raw(keyword_token) then (
                        panic("Expected different keyword");
                    );
                    print_token(keyword_token);
                )
                | :Whitespace whitespace => (
                    let s = if wrapped then (
                        whitespace.wrap
                    ) else (
                        whitespace.no_wrap
                    );
                    for c in String.iter(s) do (
                        if c == '\t' then (
                            inc_indentation();
                        ) else if c == '\n' then (
                            print_newline();
                        ) else if c == '\\' then (
                            dec_indentation();
                        ) else if c == ' ' then (
                            print_raw(" ");
                        )
                    );
                )
                | :Value { .name, ... } => (
                    let member = next_member(name); # we use this to advance unnamed idx
                    let child = expect_part(
                        "value",
                        part => match part^ with (
                            | :Value ref child => :Some child
                            | _ => :None
                        )
                    );
                    walk_ast(child, .parent = :Some { .wrapped, .priority });
                )
                | :Group {
                    .name,
                    .parts = ref rule_group_parts,
                    .quantifier,
                    .wrap_mode = override_wrap_mode,
                    .span = _,
                } => (
                    let member = next_member(name);
                    let child = match &group^.children |> Tuple.get(member) with (
                        | :Some &(:Group ref group) => :Some group
                        | :None => :None
                        | :Some &(:Value _) => panic("Expected group, got value")
                    );
                    match quantifier with (
                        | :None => if child is :None then (
                            panic("Expected group")
                        )
                        | :Optional => ()
                    );
                    if child is :Some group then (
                        expect_part(
                            "group",
                            part => match part^ with (
                                | :Group _ => :Some ()
                                | _ => :None
                            )
                        );
                        walk_ast_group(
                            rule_group_parts,
                            group,
                            override_wrap_mode |> Option.unwrap_or(wrap_mode),
                            .priority,
                            .parent,
                        );
                    );
                )
            )
        );
        while part_idx < &group^.parts |> ArrayList.length do (
            let part = &group^.parts |> ArrayList.at(part_idx);
            if part^ is :Ignored token then (
                walk_ignored_token(token);
            ) else (
                panic("unexpected part");
            );
            part_idx += 1;
        );
    );

    const walk_ignored_tokens = (tokens :: &ArrayList.t[Token.t]) => (
        for token in tokens |> ArrayList.iter do (
            walk_ignored_token(token^);
        );
    );

    const walk_ignored_token = (token :: Token.t) => (
        print_token(token);
    );

    const Cli = (
        module:

        const Args = (
            module:
        
            const t = newtype {
                .ruleset :: Option.t[String],
                .paths :: ArrayList.t[String],
            };
            
            const parse = start_index -> t => (
                let mut ruleset = :None;
                let mut paths = ArrayList.new();
                let mut i = start_index;
                while i < std.sys.argc() do (
                    let arg = std.sys.argv_at(i);
                    if arg == "--ruleset" then (
                        ruleset = :Some std.sys.argv_at(i + 1);
                        i += 2;
                        continue;
                    );
                    &mut paths |> ArrayList.push_back(arg);
                    i += 1;
                );
                { .ruleset, .paths }
            );
        );

        const run = (args :: Args.t) => (
            let ruleset_path = args.ruleset |> Option.unwrap_or("tests/syntax/kast.ks");
            let mut lexer = Lexer.new(Source.read(SourcePath.file(ruleset_path)));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);
            let process = (path :: SourcePath) => (
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

                format(&parsed, @current Output);
            );
            if &args.paths |> ArrayList.length == 0 then (
                process(:Stdin);
            );
            for path in args.paths |> ArrayList.into_iter do (
                process(SourcePath.file(path));
            );
        );
    );
);
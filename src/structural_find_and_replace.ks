 # Find this:
#   &group^.parts |> ArrayList.at(part_idx)
# and replace with this:
#   group^.parts.[part_idx]
#
# kast find-ast --pattern '&$arr |> ArrayList.at($idx)' --replace '$arr.[$idx]'
use (import "./common.ks").*;
use (import "./tuple.ks").*;
use (import "./span.ks").*;
use (import "./source.ks").*;
use (import "./source_path.ks").*;
use (import "./syntax_parser.ks").*;
use (import "./parser.ks").*;
use (import "./token.ks").*;
use (import "./token_stream.ks").*;
use (import "./lexer.ks").*;
use (import "./output.ks").*;
use (import "./syntax_rule.ks").*;
use (import "./syntax_ruleset.ks").*;
use (import "./ast.ks").*;
use (import "./highlight.ks").*;
use std.collections.OrdMap;

module:

const StructuralFindAndReplace = (
    module:

    const Pattern = newtype {
        .ast :: Ast.t,
        .bindings :: OrdMap.t[String, type (&Ast.t)],
    };

    const compile_pattern = (
        source :: Source,
        .ruleset :: SyntaxRuleset.t,
    ) -> Pattern => (
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let parsed = Parser.parse(
            .ruleset,
            .entire_source_span = Source.entire_span(&source),
            .path = source.path,
            .token_stream = &mut token_stream,
        );
        let mut bindings = OrdMap.new();
        with FindBindingsContext = {
            .bindings = &mut bindings,
        };
        find_bindings_ast(&parsed.ast);
        {
            .ast = parsed.ast,
            .bindings,
        }
    );

    const FindBindingsContext = @context newtype {
        .bindings :: &mut OrdMap.t[String, type (&Ast.t)],
    };

    const find_bindings_ast = (ast :: &Ast.t) => with_return (
        if unquote_name(ast) is :Some name then (
            (@current FindBindingsContext).bindings |> OrdMap.add(name, ast);
            return;
        );
        match ast^.shape with (
            | :Empty => ()
            | :Token _ => ()
            | :Rule { .root = ref root, ... } => (
                find_bindings_ast_group(root);
            )
            | :Syntax { .value_after, ... } => (
                if value_after is :Some ref ast then (
                    find_bindings_ast(ast);
                );
            )
        );
    );

    const find_bindings_ast_group = (group :: &Ast.Group) => (
        for part in &group^.parts |> ArrayList.iter do (
            match part^ with (
                | :Ignored _ => ()
                | :Keyword _ => ()
                | :Value ref ast => find_bindings_ast(ast)
                | :Group ref group => find_bindings_ast_group(group)
            );
        );
    );

    const Found = newtype {
        .ast :: &Ast.t,
        .bindings :: OrdMap.t[String, type (&Ast.t)],
    };

    impl Found as module = (
        module:

        const print_ast = (ast :: &Ast.t) => (
            let output = @current Output;
            Highlight.print_single_line(ast);
            ansi.with_mode(
                :Dim,
                () => (
                    output.write(" at ");
                    Span.print(ast^.span);
                ),
            );
        );

        const print = (self :: &Found) => (
            let output = @current Output;
            output.write("Found ");
            print_ast(self^.ast);
            output.write(":\n");
            for &{ .key = name, .value = ast } in &self^.bindings |> OrdMap.iter do (
                output.write(name);
                output.write(" = ");
                print_ast(ast);
                output.write("\n");
            );
        );
    );

    const TryMatchContext = @context newtype {
        .bindings :: &mut OrdMap.t[String, type (&Ast.t)],
        .fail :: [T] () -> T,
    };

    const try_match = (
        ast :: &Ast.t,
        pattern :: &Pattern,
    ) -> Option.t[Found] => with_return (
        let mut bindings = OrdMap.new();
        with TryMatchContext = {
            .bindings = &mut bindings,
            .fail = [T] () -> T => return :None,
        };
        walk_and_match_ast(ast, .pattern = &pattern^.ast);
        :Some { .ast, .bindings }
    );

    const unquote_name = (ast :: &Ast.t) -> Option.t[String] => with_return (
        if ast^.shape is :Rule { .rule, .root } then (
            if rule.name == "core:unquote" then (
                let child = root.children |> Tuple.unwrap_unnamed_1 |> Ast.unwrap_child_value;
                if child.shape is :Token token then (
                    return :Some Token.raw(token);
                );
            );
        );
        :None
    );

    const walk_and_match_ast = (
        ast :: &Ast.t,
        .pattern :: &Ast.t,
    ) => with_return (
        let ctx = @current TryMatchContext;
        if unquote_name(pattern) is :Some name then (
            ctx.bindings |> OrdMap.add(name, ast);
            return;
        );
        match { ast^.shape, pattern^.shape } with (
            | { :Empty, :Empty } => ()
            | { :Token a, :Token b } => (
                if Token.raw(a) != Token.raw(b) then (
                    ctx.fail();
                );
            )
            | {
                :Rule { .rule = ref ast_rule, .root = ref ast_root },
                :Rule { .rule = ref pattern_rule, .root = ref pattern_root },
            } => (
                if not SyntaxRule.is_same(ast_rule, pattern_rule) then (
                    ctx.fail();
                );
                walk_and_match_ast_group(ast_root, .pattern = pattern_root);
            )
            | _ => ctx.fail()
        )
    );

    const walk_and_match_ast_group = (
        group :: &Ast.Group,
        .pattern :: &Ast.Group,
    ) => (
        let ctx = @current TryMatchContext;
        let mut group_part_idx = 0;
        let mut pattern_part_idx = 0;
        let next_respected_part = (idx, parts) -> Option.t[type (&Ast.Part)] => with_return (
            while idx^ < parts |> ArrayList.length do (
                let part = parts |> ArrayList.at(idx^);
                idx^ += 1;
                if part^ is :Ignored _ then () else (
                    return :Some part;
                );
            );
            :None
        );
        loop (
            let group_part = next_respected_part(&mut group_part_idx, &group^.parts);
            let pattern_part = next_respected_part(&mut pattern_part_idx, &pattern^.parts);
            match { group_part, pattern_part } with (
                | { :None, :None } => break
                | { :Some a, :Some b } => match { a^, b^ } with (
                    | { :Keyword a, :Keyword b } => (
                        if Token.raw(a) != Token.raw(b) then (
                            ctx.fail();
                        );
                    )
                    | { :Value ref a, :Value ref b } => (
                        walk_and_match_ast(a, .pattern = b);
                    )
                    | { :Group ref a, :Group ref b } => (
                        walk_and_match_ast_group(a, .pattern = b);
                    )
                    | _ => ctx.fail()
                )
                | _ => ctx.fail()
            )
        );
    );

    const FindContext = @context newtype {
        .pattern :: &Pattern,
        .found :: &mut ArrayList.t[Found],
    };

    const find = (ast :: &Ast.t, pattern :: &Pattern) -> ArrayList.t[Found] => (
        let mut found = ArrayList.new();
        with FindContext = {
            .pattern,
            .found = &mut found,
        };
        walk_ast(ast);
        found
    );

    const walk_ast = (ast :: &Ast.t) => (
        let ctx = @current FindContext;
        if try_match(ast, ctx.pattern) is :Some found then (
            ctx.found |> ArrayList.push_back(found);
        ) else match ast^.shape with (
            | :Empty => ()
            | :Token _ => ()
            | :Rule { .root = ref root, ... } => walk_ast_group(root)
            | :Syntax { .value_after = ref value_after, ... } => (
                if value_after^ is :Some ref ast then (
                    walk_ast(ast);
                );
            )
        );
    );

    const walk_ast_group = (group :: &Ast.Group) => (
        for { _member, child } in &group^.children |> Tuple.iter do (
            match child^ with (
                | :Value ref ast => walk_ast(ast)
                | :Group ref group => walk_ast_group(group)
            )
        );
    );

    const Cli = (
        module:

        const Args = (
            module:

            const t = newtype {
                .ruleset :: Option.t[String],
                .paths :: ArrayList.t[String],
                .pattern :: String,
                .replace :: Option.t[String],
                .replace_ruleset :: Option.t[String],
                .inplace :: Bool,
            };

            const parse = start_index -> t => (
                let mut ruleset = :None;
                let mut paths = ArrayList.new();
                let mut pattern = :None;
                let mut replace = :None;
                let mut replace_ruleset = :None;
                let mut inplace = false;
                let mut i = start_index;
                while i < std.sys.argc() do (
                    let arg = std.sys.argv_at(i);
                    if arg == "--ruleset" then (
                        ruleset = :Some std.sys.argv_at(i + 1);
                        i += 2;
                        continue;
                    );
                    if arg == "--pattern" then (
                        pattern = :Some std.sys.argv_at(i + 1);
                        i += 2;
                        continue;
                    );
                    if arg == "--replace" then (
                        replace = :Some std.sys.argv_at(i + 1);
                        i += 2;
                        continue;
                    );
                    if arg == "--replace-ruleset" then (
                        replace_ruleset = :Some std.sys.argv_at(i + 1);
                        i += 2;
                        continue;
                    );
                    if arg == "--inplace" then (
                        inplace = true;
                        i += 1;
                        continue;
                    );
                    &mut paths |> ArrayList.push_back(arg);
                    i += 1;
                );
                let pattern = pattern |> Option.unwrap_or_else(() => panic("missing --pattern arg"));
                {
                    .ruleset,
                    .paths,
                    .pattern,
                    .replace,
                    .replace_ruleset,
                    .inplace,
                }
            );
        );

        const run = (args :: Args.t) => (
            let ruleset_path = args.ruleset |> Option.unwrap_or("tests/syntax/kast.ks");
            let mut lexer = Lexer.new(Source.read(SourcePath.file(ruleset_path)));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);
            let replace_ruleset = match args.replace_ruleset with (
                | :None => ruleset
                | :Some path => (
                    let mut lexer = Lexer.new(Source.read(SourcePath.file(path)));
                    let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
                    SyntaxParser.parse_syntax_ruleset(&mut token_stream)
                )
            );
            let pattern = compile_pattern(
                {
                    .contents = args.pattern,
                    .path = :Special "pattern",
                },
                .ruleset,
            );
            let replace = args.replace
                |> Option.map(
                    contents => compile_pattern(
                        {
                            .contents,
                            .path = :Special "replace pattern",
                        },
                        .ruleset = replace_ruleset,
                    )
                );
            let process = (path :: SourcePath) => (
                let source = Source.read(path);
                let mut lexer = Lexer.new(source);
                let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
                let parsed = Parser.parse(
                    .ruleset,
                    .entire_source_span = Source.entire_span(&source),
                    .path = source.path,
                    .token_stream = &mut token_stream,
                );
                let founds = find(&parsed.ast, &pattern);
                match replace with (
                    | :None => (
                        for found in founds |> ArrayList.into_iter do (
                            Found.print(&found);
                            (@current Output).write("\n");
                        );
                    )
                    | :Some replace_pattern => (
                        let mut output = Highlight.new_output(:Terminal);
                        let replace_ast = (ast :: &Ast.t, f :: &Ast.t -> ()) => (
                            for found in &founds |> ArrayList.iter do (
                                # TODO this is hack only working currently
                                # I mean the equality - we are checking for identity equality
                                if found^.ast^ == ast^ then (
                                    with Highlight.Context = {
                                        ...(@current Highlight.Context),
                                        .replace_ast = :Some (
                                            (ast :: &Ast.t, f :: &Ast.t -> ()) => (
                                                for &{ .key = binding_name, .value = binding_ast } in &replace_pattern.bindings |> OrdMap.iter do (
                                                    # TODO identity equality
                                                    if ast^ == binding_ast^ then (
                                                        f((&found^.bindings |> OrdMap.get(binding_name) |> Option.unwrap)^);
                                                        break;
                                                    );
                                                );
                                            )
                                        ),
                                    };
                                    f(&replace_pattern.ast);
                                    break;
                                );
                            );
                        );
                        output.replace_ast = :Some replace_ast;
                        if args.inplace then (
                            let result = output_to_string(
                                () => (
                                    Highlight.highlight(&parsed, output);
                                    (@current Output).write("\n");
                                )
                            );
                            let path = match path |> SourcePath.file_path with (
                                | :Some path => path
                                | :None => panic("Inplace is only available given file path")
                            );
                            # TODO std.fs.write_file
                            @native "(await import('fs')).writeFileSync(\(path), \(result))";
                        ) else (
                            Highlight.highlight(&parsed, output);
                        );
                    )
                );
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

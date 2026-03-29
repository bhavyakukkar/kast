use (import "./common.ks").*;
use (import "./output.ks").*;
use (import "./source.ks").*;
use (import "./source_path.ks").*;
use (import "./position.ks").*;
use (import "./span.ks").*;
use (import "./token.ks").*;
use (import "./ast.ks").*;
use (import "./lexer.ks").*;
use (import "./token_stream.ks").*;
use (import "./syntax_parser.ks").*;
use (import "./parser.ks").*;

module:

const Highlight = (
    module:

    const TokenType = newtype (
        | :StringDelimeter
        | :StringContent
        | :Escape
        | :Keyword
        | :Number
        | :Ident
        | :RawIdent
        | :Regular
        | :Error
        | :Comment
        | :SyntaxCommand
    );

    const OutputT = newtype {
        .print :: (Span, TokenType, String) -> (),
    };

    const Context = @context OutputT;

    const highlight = (parsed :: &Parser.Parsed, output :: OutputT) => (
        with Context = output;
        let {
            .ast = ref ast,
            .ignored_trailing_tokens = ref ignored_trailing_tokens,
            .eof = _,
        } = parsed^;
        walk_ast(ast);
        walk_ignored_tokens(ignored_trailing_tokens);
    );

    const walk_string_parts = (
        content_token_type :: TokenType,
        parts :: &ArrayList.t[Token.RawStringPart],
    ) => (
        for part in parts |> ArrayList.iter do (
            match part^ with (
                | :Content { .raw, .span } => (
                    (@current Context).print(span, content_token_type, raw);
                )
                | :Escape { .raw, .span } => (
                    (@current Context).print(span, :Escape, raw);
                )
            );
        );
    );

    const walk_ast = (ast :: &Ast.t) => (
        let {
            .ignored_tokens_before = ref ignored_tokens_before,
            .shape = ref shape,
            .span = _,
        } = ast^;
        walk_ignored_tokens(ignored_tokens_before);
        match shape^ with (
            | :Empty => ()
            | :Token token => (
                match token.shape with (
                    | :Ident { .raw, .string, ... } => (
                        match string with (
                            | :None => (
                                (@current Context).print(token.span, :Ident, raw);
                            )
                            | :Some { { .open, .close, .raw_parts, ... }, .at_token } => (
                                (@current Context).print(at_token.span, :Ident, Token.raw(at_token));
                                (@current Context).print(open.span, :RawIdent, Token.raw(open));
                                walk_string_parts(:RawIdent, &raw_parts);
                                (@current Context).print(close.span, :RawIdent, Token.raw(close));
                            )
                        )
                    )
                    | :Number { .raw, ... } => (
                        (@current Context).print(token.span, :Number, raw);
                    )
                    | :String { .open, .close, .raw_parts, ... } => (
                        (@current Context).print(open.span, :StringDelimeter, Token.raw(open));
                        walk_string_parts(:StringContent, &raw_parts);
                        (@current Context).print(close.span, :StringDelimeter, Token.raw(close));
                    )
                )
            )
            | :InterpolatedString { .delimiter = _, .open, .parts, .close } => (
                (@current Context).print(open.span, :StringDelimeter, Token.raw(open));
                for part in &parts |> ArrayList.iter do (
                    match part^ with (
                        | :Content { .raw = _, .raw_parts, .span, .contents = _ } => (
                            walk_string_parts(:StringContent, &raw_parts);
                        )
                        | :Interpolated {
                            .open,
                            .close,
                            .ast = ref inner,
                            .ignored_trailing_tokens = ref ignored_trailing_tokens,
                        } => (
                            (@current Context).print(open.span, :Escape, Token.raw(open));
                            walk_ast(inner);
                            walk_ignored_tokens(ignored_trailing_tokens);
                            (@current Context).print(close.span, :Escape, Token.raw(close));
                        )
                    );
                );
                (@current Context).print(close.span, :StringDelimeter, Token.raw(close));
            )
            | :Rule { .root = ref root, ... } => (
                walk_ast_group(root);
            )
            | :Syntax { .command, .value_after } => (
                for token in &command.raw_tokens |> ArrayList.iter do (
                    (@current Context).print(token^.span, :SyntaxCommand, Token.raw(token^));
                );
                if value_after is :Some ref ast then (
                    walk_ast(ast);
                );
            )
            | :Error { .parts = ref parts } => (
                walk_ast_parts(parts)
            )
        )
    );
    
    const walk_ast_group = (group :: &Ast.Group) => (
        walk_ast_parts(&group^.parts);
    );

    const walk_ast_parts = (parts :: &ArrayList.t[Ast.Part]) => (
        for part in parts |> ArrayList.iter do (
            match part^ with (
                | :Keyword token => (
                    (@current Context).print(token.span, :Keyword, Token.raw(token));
                )
                | :Value ref ast => (
                    walk_ast(ast);
                )
                | :Group ref inner_group => (
                    walk_ast_group(inner_group);
                )
                | :Ignored ref token => (
                    walk_ignored_token(token);
                )
            );
        );
    );

    const walk_ignored_token = (token :: &Token.t) => (
        if token^.shape is :Comment { .raw, ... } then (
            (@current Context).print(token^.span, :Comment, raw);
        ) else (
            # ignored because of error
            (@current Context).print(token^.span, :Error, Token.raw(token^));
        )
    );

    const walk_ignored_tokens = (tokens :: &ArrayList.t[Token.t]) => (
        for token in tokens |> ArrayList.iter do (
            walk_ignored_token(token);
        );
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

            let init_state = () => {
                .position = Position.beginning(),
            };
            let mut state = init_state();
            let print_token = (span, token_type, s) => (
                while state.position.line < span.start.line do (
                    # TODO fix copy
                    state.position = { ...state.position };
                    state.position.line += 1;
                    state.position.column = PositionColumn.zero();
                    (@current Output).write("\n");
                );
                for _ in state.position.column.string_encoding..span.start.column.string_encoding do (
                    (@current Output).write(" ");
                );
                let mode :: Option.t[ansi.Mode] = match token_type with (
                    | :Keyword => :Some :Magenta
                    | :Number => :Some :Italic
                    | :StringContent => :Some :Green
                    | :StringDelimeter => :Some :Cyan
                    | :Escape => :Some :Cyan
                    | :RawIdent => :None
                    | :Ident => :None
                    | :Regular => :None
                    | :Error => :Some :Red
                    | :Comment => :Some :Gray
                    | :SyntaxCommand => :Some :Yellow
                );
                match mode with (
                    | :Some mode => ansi.with_mode(mode, () => (@current Output).write(s))
                    | :None => (@current Output).write(s)
                );
                state.position = span.end;
            );
            let output :: OutputT = {
                .print = print_token,
            };
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
                
                Highlight.highlight(&parsed, output);
                (@current Output).write("\n");
                state = init_state();
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
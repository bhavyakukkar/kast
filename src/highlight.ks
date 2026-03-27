use (import "./common.ks").*;
use (import "./output.ks").*;
use (import "./source.ks").*;
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
        | :String
        | :Escape
        | :Keyword
        | :Number
        | :Regular
    );

    const OutputT = newtype {
        .print :: (Span, TokenType, String) -> (),
    };

    const Context = @context OutputT;

    const ast = (ast :: &Ast.t, output :: OutputT) => (
        with Context = output;
        walk_ast(ast);
    );

    const walk_ast = (ast :: &Ast.t) => (
        match ast^.shape with (
            | :Empty => ()
            | :Token token => (
                match token.shape with (
                    | :Ident { .raw, ... } => (
                        (@current Context).print(token.span, :Regular, raw);
                    )
                    | :Number { .raw, ... } => (
                        (@current Context).print(token.span, :Number, raw);
                    )
                    | :String { .raw, ... } => (
                        (@current Context).print(token.span, :String, raw);
                    )
                )
            )
            | :InterpolatedString { .delimiter = _, .open, .parts, .close } => (
                (@current Context).print(open.span, :String, Token.Shape.raw(open.shape));
                for part in &parts |> ArrayList.iter do (
                    match part^ with (
                        | :Content { .raw, .span, .contents = _ } => (
                            (@current Context).print(span, :String, raw);
                        )
                        | :Interpolated { .open, .close, .ast = ref inner } => (
                            (@current Context).print(open.span, :Escape, Token.Shape.raw(open.shape));
                            walk_ast(inner);
                            (@current Context).print(close.span, :Escape, Token.Shape.raw(close.shape));
                        )
                    );
                );
                (@current Context).print(close.span, :String, Token.Shape.raw(close.shape));
            )
            | :Rule { .root = ref root, ... } => (
                walk_ast_group(root);
            )
            | :Syntax _ => ()
        )
    );
    
    const walk_ast_group = (group :: &Ast.Group) => (
        for part in &group^.parts |> ArrayList.iter do (
            match part^ with (
                | :Keyword token => (
                    (@current Context).print(token.span, :Keyword, Token.Shape.raw(token.shape));
                )
                | :Value ref ast => (
                    walk_ast(ast);
                )
                | :Group ref inner_group => (
                    walk_ast_group(inner_group);
                )
            );
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
            let mut lexer = Lexer.new(Source.read_file(ruleset_path));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            let ruleset = SyntaxParser.parse_syntax_ruleset(&mut token_stream);

            let init_state = () => {
                .position = Position.beginning(),
            };
            let mut state = init_state();
            let print_token = (span, token_type, s) => (
                while state.position.line < span.start.line do (
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
                    | :String => :Some :Green
                    | :Escape => :Some :Cyan
                    | :Regular => :None
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
            for path in args.paths |> ArrayList.into_iter do (
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
                
                Highlight.ast(&parsed.ast, output);
                (@current Output).write("\n");
                state = init_state();
            );
        );
    );
);
use (import "../output.ks").*;
use (import "../diagnostic.ks").*;
use (import "../tuple.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../token_stream.ks").*;
use (import "../lexer.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../ast.ks").*;
use (import "./ir.ks").*;
use std.collections.OrdMap;
use std.collections.OrdSet;

module:

const Compiler = (
    module:

    const TypeKind = newtype (
        | :Enum
        | :Union
        | :Struct
    );

    const TopLevelItem = newtype (
        | :Type {
            .name :: String,
            .kind :: TypeKind,
            .def :: Ast.t,
        }
        | :Fn {
            .name :: String,
            .def :: Ast.t,
        }
    );

    const State = newtype {
        .top_level_items :: ArrayList.t[TopLevelItem],
        .type_names :: OrdSet.t[String],
        .program :: Ir.Program,
    };

    const init = () -> State => {
        .top_level_items = ArrayList.new(),
        .type_names = OrdSet.new(),
        .program = {
            .types = OrdMap.new(),
            .fns = OrdMap.new(),
        },
    };

    const expect_ident = (ast :: Ast.t) -> String => (
        match ast.shape with (
            | :Token { .shape = :Ident { .name, ... }, ... } => name
            | _ => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        (@current Output).write("Expected an ident");
                    ),
                    .span = ast.span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );

    const expect_string_literal = (ast :: Ast.t) -> String => (
        match ast.shape with (
            | :Token { .shape = :String { .contents, ... }, ... } => contents
            | _ => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        (@current Output).write("Expected a string literal");
                    ),
                    .span = ast.span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );

    const add_source_ast = (state :: &mut State, ast :: Ast.t) => (
        let type_def = (kind :: TypeKind, root :: Ast.Group) => (
            let name = (
                &root.children
                    |> Tuple.get_named("name")
                    |> Option.unwrap
            )^
                |> Ast.unwrap_child_value
                |> expect_ident;
            let def = (
                &root.children
                    |> Tuple.get_named("def")
                    |> Option.unwrap
            )^
                |> Ast.unwrap_child_value;
            let ty = { .name, .kind, .def };
            &mut state^.top_level_items |> ArrayList.push_back(:Type ty);
        );

        for item in Ast.iter_list(
            ast,
            .binary_rule_name = "then",
            .trailing_or_leading_rule_name = :Some "stmt",
        ) do (
            match item.shape with (
                | :Rule { .rule, .root } => (
                    if rule.name == "enum" then (
                        type_def(:Enum, root);
                        continue;
                    );
                    if rule.name == "union" then (
                        type_def(:Union, root);
                        continue;
                    );
                    if rule.name == "struct" then (
                        type_def(:Struct, root);
                        continue;
                    );
                    if rule.name == "top_level_fn" then (
                        continue;
                    );
                )
            );
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    (@current Output).write("Unexpected top level item");
                ),
                .span = item.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
    );

    const add_source = (state :: &mut State, source :: Source) => (
        let ruleset = (
            let syntax_path = std.path.dirname(__FILE__) + "/syntax.ks";
            let mut lexer = Lexer.new(Source.read(SourcePath.file(syntax_path)));
            let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
            SyntaxParser.parse_syntax_ruleset(&mut token_stream)
        );
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let parsed = Parser.parse(
            .ruleset,
            .entire_source_span = Source.entire_span(&source),
            .path = source.path,
            .token_stream = &mut token_stream,
        );
        add_source_ast(state, parsed.ast)
    );

    const Context = @context State;

    const compile = (mut state :: State) -> Ir.Program => (
        with Context = state;
        for item in &state.top_level_items |> ArrayList.iter do (
            match item^ with (
                | :Type { .name, ... } => (
                    &mut state.type_names |> OrdSet.add(name);
                )
                | :Fn _ => panic("TODO fn")
            )
        );
        for item in &state.top_level_items |> ArrayList.iter do (
            match item^ with (
                | :Type ty => (
                    process_type(...ty);
                )
                | :Fn _ => panic("TODO fn")
            )
        );
        state.program
    );

    const expect_rule_with_two_children = (
        ast :: Ast.t,
        .rule :: String = rule_name,
        .child_names :: Option.t[type { String, String }],
        .error_msg :: String,
    ) -> { Ast.t, Ast.t } => with_return (
        if ast.shape is :Rule { .rule, .root } then (
            if rule.name == rule_name then (
                let { a, b } = match child_names with (
                    | :None => (
                        root.children
                            |> Tuple.unwrap_unnamed_2
                    )
                    | :Some { a, b } => (
                        let a = (
                            &root.children
                                |> Tuple.get_named(a)
                                |> Option.unwrap
                        )^;
                        let b = (
                            &root.children
                                |> Tuple.get_named(b)
                                |> Option.unwrap
                        )^;
                        { a, b }
                    )
                );
                return {
                    a |> Ast.unwrap_child_value,
                    b |> Ast.unwrap_child_value,
                };
            );
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                (@current Output).write(error_msg);
            ),
            .span = ast.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const expect_rule_with_single_child = (
        ast :: Ast.t,
        .rule :: String = rule_name,
        .child_name :: Option.t[String],
        .error_msg :: String,
    ) -> Ast.t => with_return (
        if ast.shape is :Rule { .rule, .root } then (
            if rule.name == rule_name then (
                let child = match child_name with (
                    | :None => (
                        root.children
                            |> Tuple.unwrap_unnamed_1
                    )
                    | :Some name => (
                        &root.children
                            |> Tuple.get_named(name)
                            |> Option.unwrap
                    )^
                );
                return child |> Ast.unwrap_child_value;
            );
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                (@current Output).write(error_msg);
            ),
            .span = ast.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const process_type = (
        .name :: String,
        .kind :: TypeKind,
        .def :: Ast.t,
    ) => (
        let def = expect_rule_with_single_child(
            def,
            .rule = "record",
            .child_name = :None,
            .error_msg = "Expected {}",
        );
        match kind with (
            | :Enum => process_enum(.name, .def)
            | :Union => process_struct_or_union(.name, .kind, .def)
            | :Struct => process_struct_or_union(.name, .kind, .def)
        );
    );

    const process_enum = (
        .name :: String,
        .def :: Ast.t,
    ) => (
        let mut ctx = @current Context;
        let mut variants = OrdSet.new();
        for variant in Ast.iter_list(
            def,
            .binary_rule_name = "union",
            .trailing_or_leading_rule_name = :Some "leading union",
        ) do (
            let variant_name = expect_rule_with_single_child(
                variant,
                .rule = "variant",
                .child_name = :Some "label",
                .error_msg = "Expected :VariantName",
            )
                |> expect_ident;
            &mut variants |> OrdSet.add(variant_name);
        );
        let ty = :Enum { .variants };
        &mut ctx.program.types |> OrdMap.add(name, ty);
    );

    const process_struct_or_union = (
        .name :: String,
        .kind :: TypeKind,
        .def :: Ast.t,
    ) => (
        let mut ctx = @current Context;
        let mut fields = OrdMap.new();
        for field in Ast.iter_list(
            def,
            .binary_rule_name = "comma",
            .trailing_or_leading_rule_name = :Some "trailing comma",
        ) do (
            let { name, ty } = expect_rule_with_two_children(
                field,
                .rule = "field def",
                .child_names = :Some { "label", "type" },
                .error_msg = "Expected .name :: Type",
            );
            let name = name |> expect_ident;
            let ty = { .name = ty |> expect_ident };
            &mut fields |> OrdMap.add(name, ty);
        );
        let ty = match kind with (
            | :Struct => :Struct { .fields }
            | :Union => :Union { .variants = fields }
            | _ => panic("unreachable")
        );
        &mut ctx.program.types |> OrdMap.add(name, ty);
    );
);

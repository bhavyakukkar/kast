use (import "../log.ks").*;
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
use std.collections.OrdMap;
use std.collections.OrdSet;

use (import "./ir.ks").*;
use (import "./ast_helpers.ks").*;

module:

const Compiler = (
    module:

    const TypeKind = newtype (
        | :Opaque
        | :Enum
        | :Struct
        | :Union
    );

    const TopLevelKind = newtype (
        | :Type TypeKind
        | :Fn
    );

    const TopLevelItem = newtype {
        .name :: String,
        .kind :: TopLevelKind,
        .def :: Ast.t,
    };

    const State = newtype {
        .top_level_items :: ArrayList.t[TopLevelItem],
        .type_names :: OrdSet.t[String],
        .fn_types :: OrdMap.t[String, Ir.FnType],
        .program :: Ir.Program,
    };

    const init = () -> State => {
        .top_level_items = ArrayList.new(),
        .type_names = OrdSet.new(),
        .fn_types = OrdMap.new(),
        .program = {
            .types = OrdMap.new(),
            .fns = OrdMap.new(),
        },
    };

    const add_source_ast = (state :: &mut State, ast :: Ast.t) => (
        let type_def = (name :: String, kind :: TypeKind, root :: Ast.Group) => (
            let def = (
                &root.children
                    |> Tuple.get_named("def")
            )^
                |> Ast.unwrap_child_value;
            let def = def
                |> AstHelpers.expect_rule("record")
                |> AstHelpers.expect_single_child(:None);
            let item = {
                .name,
                .kind = :Type kind,
                .def,
            };
            &mut state^.top_level_items |> ArrayList.push_back(item);
        );
        let top_level_item = (root :: Ast.Group) => with_return (
            let name = (
                &root.children
                    |> Tuple.get_named("name")
            )^
                |> Ast.unwrap_child_value
                |> AstHelpers.expect_ident;
            let value = (
                &root.children
                    |> Tuple.get_named("value")
            )^
                |> Ast.unwrap_child_value;
            if value.shape is :Rule { .rule, .root } then (
                if rule.name == "enum" then (
                    type_def(name, :Enum, root);
                    return;
                );
                if rule.name == "struct" then (
                    type_def(name, :Struct, root);
                    return;
                );
                if rule.name == "union" then (
                    type_def(name, :Union, root);
                    return;
                );
                if rule.name == "opaque_type" then (
                    let item = {
                        .name,
                        .kind = :Type :Opaque,
                        .def = value,
                    };
                    &mut state^.top_level_items |> ArrayList.push_back(item);
                    return;
                );
                if rule.name == "fn" then (
                    let item = {
                        .name,
                        .kind = :Fn,
                        .def = value,
                    };
                    &mut state^.top_level_items |> ArrayList.push_back(item);
                    return;
                );
            );
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    (@current Output).write("Unexpected top level item");
                ),
                .span = value.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
        for item in Ast.iter_list(
            ast,
            .binary_rule_name = "then",
            .trailing_or_leading_rule_name = :Some "stmt",
        ) do (
            if item.shape is :Rule { .rule, .root } then (
                if rule.name == "const" then (
                    top_level_item(root);
                    continue;
                );
            );
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    (@current Output).write("Expected const <name> = <top_level_item>");
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
            match item^.kind with (
                | :Type _ => (
                    &mut state.type_names |> OrdSet.add(item^.name);
                )
                | :Fn => ()
            )
        );
        for item in &state.top_level_items |> ArrayList.iter do (
            match item^.kind with (
                | :Type _ => ()
                | :Fn => (
                    preprocess_fn(item^.name, item^.def);
                )
            )
        );
        for item in &state.top_level_items |> ArrayList.iter do (
            process_top_level_item(item^);
        );
        state.program
    );

    const parse_type = (ast :: Ast.t) -> Ir.TypeName => with_return (
        match ast.shape with (
            | :Rule { .rule, .root } => (
                if rule.name == "fn_type" then (
                    let { arg_asts, result } = root
                        |> AstHelpers.expect_two_children(:Some { "args", "result" });
                    let arg_asts = arg_asts
                        |> AstHelpers.expect_rule("scope")
                        |> AstHelpers.expect_single_child(:None);
                    let mut args = ArrayList.new();
                    for arg_ast in Ast.iter_list(
                        arg_asts,
                        .binary_rule_name = "comma",
                        .trailing_or_leading_rule_name = :Some "trailing comma",
                    ) do (
                        &mut args |> ArrayList.push_back(parse_type(arg_ast));
                    );
                    let result = parse_type(result);
                    return :Fn { .args, .result };
                );
            )
            | :Token token => match token.shape with (
                | :Ident ident => (
                    let name = ident.name;
                    if not &(@current Context).type_names |> OrdSet.contains(name) then (
                        let diagnostic = {
                            .severity = :Error,
                            .source = :Compiler,
                            .message = () => (
                                let output = @current Output;
                                output.write("Type ");
                                output.write(String.escape(name));
                                output.write(" not found");
                            ),
                            .span = token.span,
                            .related = ArrayList.new(),
                        };
                        Diagnostic.report(diagnostic);
                    );
                    return :Named name;
                )
                | _ => ()
            )
            | _ => ()
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                (@current Output).write("Expected a type");
            ),
            .span = ast.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const preprocess_fn = (
        name :: String,
        def :: Ast.t,
    ) => (
        let { args, result_ty, body } = def
            |> AstHelpers.expect_rule("fn")
            |> AstHelpers.expect_three_children("args", "result_ty", "body");
        let args = args
            |> AstHelpers.expect_rule("scope")
            |> AstHelpers.expect_single_child(:None);
        let mut arg_types = ArrayList.new();
        for arg in Ast.iter_list(
            args,
            .binary_rule_name = "comma",
            .trailing_or_leading_rule_name = :Some "trailing comma",
        ) do (
            let { name, ty } = arg
                |> AstHelpers.expect_rule("type ascribe")
                |> AstHelpers.expect_two_children(:Some { "expr", "type" });
            let ty = parse_type(ty);
            &mut arg_types |> ArrayList.push_back(ty);
        );
        let result_ty = parse_type(result_ty);
        let mut ctx = @current Context;
        let fn_type = {
            .args = arg_types,
            .result = result_ty,
        };
        Log.info(
            () => (
                let output = @current Output;
                output.write("fn ");
                output.write(name);
                output.write(" :: ");
                Ir.Print.fn_type(&fn_type);
            )
        );
        &mut ctx.fn_types |> OrdMap.add(name, fn_type);
    );

    const process_top_level_item = (item :: TopLevelItem) => (
        let mut ctx = @current Context;
        let { .name, .kind, .def } = item;
        match kind with (
            | :Type kind => match kind with (
                | :Opaque => (
                    &mut ctx.program.types |> OrdMap.add(name, :Opaque);
                )
                | :Enum => process_enum(.name, .def)
                | :Union => process_struct_or_union(.name, .kind, .def)
                | :Struct => process_struct_or_union(.name, .kind, .def)
            )
            | :Fn => ()
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
            let variant_name = variant
                |> AstHelpers.expect_rule("variant")
                |> AstHelpers.expect_single_child(:Some "label")
                |> AstHelpers.expect_ident;
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
            let { name, ty } = field
                |> AstHelpers.expect_rule("field def")
                |> AstHelpers.expect_two_children(:Some { "label", "type" });
            let name = name |> AstHelpers.expect_ident;
            let ty = parse_type(ty);
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

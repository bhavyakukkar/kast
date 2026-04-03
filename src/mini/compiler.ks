use (import "../log.ks").*;
use (import "../output.ks").*;
use (import "../diagnostic.ks").*;
use (import "../tuple.ks").*;
use (import "../span.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../token_stream.ks").*;
use (import "../lexer.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../ast.ks").*;
use (import "../highlight.ks").*;
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
        | :Native
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
                if rule.name == "native" then (
                    let def = root |> AstHelpers.expect_single_child(:None);
                    let item = {
                        .name,
                        .kind = :Type :Native,
                        .def,
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

    const Scope = newtype {
        .parent :: Option.t[Scope],
        .vars :: OrdMap.t[String, Ir.Type],
    };
    const ScopeContext = @context Scope;

    const find_in_scope = (scope :: &Scope, name :: String) -> Option.t[Ir.Type] => (
        match &scope^.vars |> OrdMap.get(name) with (
            | :Some &result => :Some result
            | :None => match scope^.parent with (
                | :Some ref parent => find_in_scope(parent, name)
                | :None => :None
            )
        )
    );

    const find_ident_ty = (span :: Span, name :: String) -> Ir.Type => with_return (
        let ctx = @current Context;
        if find_in_scope(&(@current ScopeContext), name) is :Some result then (
            return result;
        );
        if &ctx.fn_types |> OrdMap.get(name) is :Some &fn_type then (
            return :Fn fn_type;
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                let output = @current Output;
                output.write(name);
                output.write(" not found in current scope");
            ),
            .span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const resolve_type_aliases = (ty :: Ir.Type) -> Ir.Type => (
        match ty with (
            | :Unit => :Unit
            | :Int32 => :Int32
            | :Bool => :Bool
            | :String => :String
            | :Named name => (
                let def = &(@current Context).program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap;
                match def^ with (
                    | :Alias ty => (
                        Log.trace(
                            () => (
                                let output = @current Output;
                                output.write(name);
                                output.write(" is alias to ");
                                Ir.Print.type_name(&ty);
                            )
                        );
                        resolve_type_aliases(ty)
                    )
                    | _ => :Named name
                )
            )
            | :Fn { .args, .result } => (
                let mut resolved_args = ArrayList.new();
                for &arg in &args |> ArrayList.iter do (
                    &mut resolved_args |> ArrayList.push_back(resolve_type_aliases(arg));
                );
                let result = resolve_type_aliases(result);
                :Fn {
                    .args = resolved_args,
                    .result = resolve_type_aliases(result),
                }
            )
        )
    );

    const type_check = (span :: Span, expected :: Ir.Type, actual :: Ir.Type) => (
        if not std.repr.structurally_equal(
            resolve_type_aliases(expected),
            resolve_type_aliases(actual),
        ) then (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    let output = @current Output;
                    output.write("Expected type ");
                    Ir.Print.type_name(&expected);
                    output.write(", got ");
                    Ir.Print.type_name(&actual);
                ),
                .span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
    );

    const ParsedExpr = newtype {
        .shape :: Ir.ExprShape,
        .ty :: Ir.Type,
    };

    const parse_if = (expected_ty :: Option.t[Ir.Type], root :: Ast.Group) -> ParsedExpr => (
        let cond = (&root.children |> Tuple.get_named("cond"))^
            |> Ast.unwrap_child_value;
        let then_case = (&root.children |> Tuple.get_named("then_case"))^
            |> Ast.unwrap_child_value;
        let else_case = &root.children
            |> Tuple.get_named_opt("else_case")
            |> Option.map(child => child^ |> Ast.unwrap_child_value);
        let cond = parse_expr(:Some :Bool, cond);
        let then_case = parse_expr(expected_ty, then_case);
        let else_case = else_case
            |> Option.map(ast => parse_expr(:Some then_case.ty, ast));
        {
            .shape = :If { .cond, .then_case, .else_case },
            .ty = then_case.ty,
        }
    );

    const parse_expr = (expected_ty :: Option.t[Ir.Type], ast :: Ast.t) -> Ir.Expr => (
        let { .shape, .ty } :: ParsedExpr = with_return (
            match ast.shape with (
                | :Empty => return {
                    .shape = :Unit,
                    .ty = :Unit,
                }
                | :Rule { .rule, .root } => (
                    if rule.name == "if" then (
                        return parse_if(expected_ty, root);
                    );
                    if rule.name == "if_without_else" then (
                        return parse_if(expected_ty, root);
                    );
                    if rule.name == "native" then (
                        let inner = root |> AstHelpers.expect_single_child(:None);
                        let mut native_parts = ArrayList.new();
                        match inner.shape with (
                            | :InterpolatedString { .parts, ... } => (
                                for part in parts |> ArrayList.into_iter do (
                                    match part with (
                                        | :Content s => (
                                            &mut native_parts |> ArrayList.push_back(:Raw s.contents);
                                        )
                                        | :Interpolated { .ast, ... } => (
                                            let part = :Interpolated parse_expr(:None, ast);
                                            &mut native_parts |> ArrayList.push_back(part);
                                        )
                                    )
                                )
                            )
                            | :Token { .shape = :String s, ... } => (
                                &mut native_parts |> ArrayList.push_back(:Raw s.contents);
                            )
                            | _ => (
                                let diagnostic = {
                                    .severity = :Error,
                                    .source = :Compiler,
                                    .message = () => (
                                        (@current Output).write("Expected a native string");
                                    ),
                                    .span = inner.span,
                                    .related = ArrayList.new(),
                                };
                                Diagnostic.report_and_unwind(diagnostic)
                            )
                        );
                        return {
                            .shape = :Native { .parts = native_parts },
                            .ty = expected_ty |> Option.unwrap_or(:Unit),
                        };
                    );
                    if rule.name == "stmt" then (
                        let inner = root |> AstHelpers.expect_single_child(:None);
                        return {
                            .shape = :Stmt parse_expr(:None, inner),
                            .ty = :Unit,
                        };
                    );
                    if rule.name == "scope" then (
                        with ScopeContext = {
                            .parent = :Some (@current ScopeContext),
                            .vars = OrdMap.new(),
                        };
                        let inner = root |> AstHelpers.expect_single_child(:None);
                        let inner = parse_expr(expected_ty, inner);
                        return {
                            .shape = :Scope inner,
                            .ty = inner.ty,
                        };
                    );
                    if rule.name == "then" then (
                        let mut last_expr_ty = :Unit;
                        let mut expr_asts = ArrayList.new();
                        for expr in Ast.iter_list(
                            ast,
                            .binary_rule_name = "then",
                            .trailing_or_leading_rule_name = :None,
                        ) do (
                            &mut expr_asts |> ArrayList.push_back(expr);
                        );
                        let mut exprs = ArrayList.new();
                        for { i, &expr } in &expr_asts |> ArrayList.iter |> std.iter.enumerate do (
                            let expected_ty = if i + 1 < ArrayList.length(&expr_asts) then (
                                :None
                            ) else (
                                expected_ty
                            );
                            let expr = parse_expr(expected_ty, expr);
                            &mut exprs |> ArrayList.push_back(expr);
                        );
                        return {
                            .shape = :Then exprs,
                            .ty = last_expr_ty,
                        };
                    );
                    if rule.name == "apply" then (
                        let f_ast = (&root.children |> Tuple.get_named("f"))^
                            |> Ast.unwrap_child_value;
                        let args_ast = (&root.children |> Tuple.get_unnamed(0))^
                            |> Ast.unwrap_child_group;
                        let args_ast = args_ast |> AstHelpers.expect_single_child(:Some "args");
                        let f = parse_expr(:None, f_ast);
                        let f_type = match f.ty with (
                            | :Fn ty => ty
                            | _ => (
                                let diagnostic = {
                                    .severity = :Error,
                                    .source = :Compiler,
                                    .message = () => (
                                        (@current Output).write("Expected a function, got ");
                                        Ir.Print.type_name(&f.ty);
                                    ),
                                    .span = f_ast.span,
                                    .related = ArrayList.new(),
                                };
                                Diagnostic.report_and_unwind(diagnostic)
                            )
                        );
                        let mut arg_asts = ArrayList.new();
                        for arg in Ast.iter_list(
                            args_ast,
                            .binary_rule_name = "comma",
                            .trailing_or_leading_rule_name = :Some "trailing comma",
                        ) do (
                            &mut arg_asts |> ArrayList.push_back(arg);
                        );

                        let actual_arg_count = &arg_asts |> ArrayList.length;
                        let expected_arg_count = &f_type.args |> ArrayList.length;
                        if actual_arg_count != expected_arg_count then (
                            let diagnostic = {
                                .severity = :Error,
                                .source = :Compiler,
                                .message = () => (
                                    let output = @current Output;
                                    output.write("Expected ");
                                    output.write(to_string(expected_arg_count));
                                    output.write(" args, got ");
                                    output.write(to_string(actual_arg_count));
                                ),
                                .span = args_ast.span,
                                .related = ArrayList.new(),
                            };
                            Diagnostic.report_and_unwind(diagnostic)
                        );
                        let mut args = ArrayList.new();
                        for { i, arg_ast } in arg_asts |> ArrayList.into_iter |> std.iter.enumerate do (
                            let expected_arg_ty = :Some (&f_type.args |> ArrayList.at(i))^;
                            let arg = parse_expr(expected_arg_ty, arg_ast);
                            &mut args |> ArrayList.push_back(arg);
                        );
                        return {
                            .shape = :Apply { .f, .args },
                            .ty = f_type.result,
                        };
                    );
                )
                | :Token token => (
                    match token.shape with (
                        | :Ident ident => (
                            return {
                                .shape = :Ident ident.name,
                                .ty = find_ident_ty(token.span, ident.name),
                            }
                        )
                        | :String str => (
                            return {
                                .shape = :StringLiteral str.contents,
                                .ty = :String,
                            };
                        )
                        | _ => ()
                    )
                )
                | _ => ()
            );
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    (@current Output).write("Expected an expr, got ");
                    Highlight.print_single_line(&ast);
                ),
                .span = ast.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
        if expected_ty is :Some expected_ty then (
            type_check(ast.span, expected_ty, ty);
        );
        { .shape, .ty, .span = ast.span }
    );

    const parse_type = (ast :: Ast.t) -> Ir.Type => with_return (
        match ast.shape with (
            | :Empty => return :Unit
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
                if rule.name == "scope" then (
                    let inner = root
                        |> AstHelpers.expect_single_child(:None);
                    return parse_type(inner);
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
                (@current Output).write("Expected a type, got ");
                Highlight.print_single_line(&ast);
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
        Log.debug(
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
                | :Native => process_native_type(.name, .def)
            )
            | :Fn => process_fn(.name, .def)
        );
    );

    const process_fn = (
        .name :: String,
        .def :: Ast.t,
    ) => (
        let mut ctx = @current Context;
        let fn_type = &ctx.fn_types |> OrdMap.get(name) |> Option.unwrap;
        let { args_ast, result_ty, body } = def
            |> AstHelpers.expect_rule("fn")
            |> AstHelpers.expect_three_children("args", "result_ty", "body");
        let args_ast = args_ast
            |> AstHelpers.expect_rule("scope")
            |> AstHelpers.expect_single_child(:None);
        let mut args :: ArrayList.t[Ir.FnArg] = ArrayList.new();
        for arg in Ast.iter_list(
            args_ast,
            .binary_rule_name = "comma",
            .trailing_or_leading_rule_name = :Some "trailing comma",
        ) do (
            let { name, ty } = arg
                |> AstHelpers.expect_rule("type ascribe")
                |> AstHelpers.expect_two_children(:Some { "expr", "type" });
            let name = name |> AstHelpers.expect_ident;
            let ty = parse_type(ty);
            &mut args |> ArrayList.push_back({ .name, .ty });
        );
        let mut scope = {
            .parent = :None,
            .vars = OrdMap.new(),
        };
        for arg in &args |> ArrayList.iter do (
            &mut scope.vars |> OrdMap.add(arg^.name, arg^.ty);
        );
        with ScopeContext = scope;
        let body = parse_expr(:Some fn_type^.result, body);
        let fn = {
            .args,
            .result_ty = fn_type^.result,
            .body,
        };
        &mut ctx.program.fns |> OrdMap.add(name, fn);
    );

    const process_native_type = (
        .name :: String,
        .def :: Ast.t,
    ) => (
        let mut ctx = @current Context;
        let ty = with_return (
            if def.shape is :Token token then (
                if token.shape is :String s then (
                    let name = s.contents;
                    if name == "Int32" then (
                        return :Int32;
                    );
                    if name == "Bool" then (
                        return :Bool;
                    );
                    if name == "String" then (
                        return :String;
                    );
                    if name == "Unit" then (
                        return :Unit;
                    );
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .message = () => (
                            let output = @current Output;
                            output.write(String.escape(name));
                            output.write(" is not valid native type");
                        ),
                        .span = def.span,
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                );
            );
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    (@current Output).write("Expected a string literal as name of native type");
                ),
                .span = def.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
        &mut ctx.program.types |> OrdMap.add(name, :Alias ty);
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

use (import "../log.ks").*;
use (import "../output.ks").*;
use (import "../diagnostic.ks").*;
use (import "../tuple.ks").*;
use (import "../span.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../token_stream.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../syntax_ruleset.ks").*;
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

    const Const = newtype {
        .name :: String,
        .ty :: Ast.t,
        .value :: Ast.t,
    };

    const State = newtype {
        .top_level_items :: ArrayList.t[TopLevelItem],
        .consts :: ArrayList.t[Const],
        .type_names :: OrdSet.t[String],
        .fn_types :: OrdMap.t[String, Ir.FnType],
        .program :: Ir.Program,
    };

    const init = () -> State => {
        .top_level_items = ArrayList.new(),
        .type_names = OrdSet.new(),
        .fn_types = OrdMap.new(),
        .consts = ArrayList.new(),
        .program = {
            .types = OrdMap.new(),
            .consts = OrdMap.new(),
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
                |> Ast.unwrap_child_value;
            let value = (
                &root.children
                    |> Tuple.get_named("value")
            )^
                |> Ast.unwrap_child_value;
            if name.shape is :Rule { .rule, .root } then (
                if rule.name == "type ascribe" then (
                    let { name, ty } = root
                        |> AstHelpers.expect_two_children(:Some { "expr", "type" });
                    let name = name |> AstHelpers.expect_ident;
                    let @"const" = {
                        .name,
                        .ty,
                        .value,
                    };
                    &mut state^.consts |> ArrayList.push_back(@"const");
                    return;
                );
            );
            let name = name |> AstHelpers.expect_ident;
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

    const ruleset_path = () -> String => (
        "src/mini/syntax.ks"
    );

    const ruleset = () -> SyntaxRuleset.t => (
        let mut lexer = Lexer.new(Source.read(SourcePath.file(ruleset_path())));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        SyntaxParser.parse_syntax_ruleset(&mut token_stream)
    );

    const add_source = (state :: &mut State, source :: Source) => (
        let ruleset = ruleset();
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
                    preprocess_toplevel_fn(item^.name, item^.def);
                )
            )
        );
        for item in &state.top_level_items |> ArrayList.iter do (
            let { .name, .kind, .def } = item^;
            match kind with (
                | :Type kind => match kind with (
                    | :Opaque => (
                        &mut state.program.types |> OrdMap.add(name, :Opaque);
                    )
                    | :Enum => process_enum(.name, .def)
                    | :Union => process_struct_or_union(.name, .kind, .def)
                    | :Struct => process_struct_or_union(.name, .kind, .def)
                    | :Native => process_native_type(.name, .def)
                )
                | _ => ()
            );
        );
        for c in state.consts |> ArrayList.into_iter do (
            process_const(c);
        );
        for item in &state.top_level_items |> ArrayList.iter do (
            let { .name, .kind, .def } = item^;
            match kind with (
                | :Fn => process_toplevel_fn(.name, .def)
                | _ => ()
            );
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
        if &ctx.program.consts |> OrdMap.get(name) is :Some c then (
            return c^.ty;
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

    const ParsedExprShape = newtype (
        | :Expr Ir.ExprShape
        | :Place Ir.PlaceExprShape
    );

    const ParsedExpr = newtype {
        .shape :: ParsedExprShape,
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
            .shape = :Expr :If { .cond, .then_case, .else_case },
            .ty = then_case.ty,
        }
    );

    const expect_known_type = (
        expected_ty :: Option.t[Ir.Type],
        span :: Span,
    ) -> Ir.Type => (
        match expected_ty with (
            | :Some ty => ty
            | :None => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        let output = @current Output;
                        output.write("Unable to infer type");
                    ),
                    .span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        )
    );

    const field_ty = (
        obj_ty :: Ir.Type,
        field :: String,
        field_span :: Span,
    ) -> Ir.Type => with_return (
        let ctx = @current Context;
        let obj_ty = resolve_type_aliases(obj_ty);
        if obj_ty is :Named name then (
            let type_def = &ctx.program.types
                |> OrdMap.get(name)
                |> Option.unwrap;
            let field_ty = match type_def^ with (
                | :Union { .variants = ref variants } => (
                    variants |> OrdMap.get(field)
                )
                | :Struct { .fields = ref fields } => (
                    fields |> OrdMap.get(field)
                )
                | _ => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .message = () => (
                            Ir.Print.type_name(&obj_ty);
                            (@current Output).write(" doesn't have fields");
                        ),
                        .span = field_span,
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            );
            match field_ty with (
                | :Some &ty => return ty
                | :None => (
                    let diagnostic = {
                        .severity = :Error,
                        .source = :Compiler,
                        .message = () => (
                            let output = @current Output;
                            Ir.Print.type_name(&obj_ty);
                            output.write(" doesn't have field ");
                            output.write(String.escape(field));
                        ),
                        .span = field_span,
                        .related = ArrayList.new(),
                    };
                    Diagnostic.report_and_unwind(diagnostic)
                )
            )
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                Ir.Print.type_name(&obj_ty);
                (@current Output).write(" doesn't have fields");
            ),
            .span = field_span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const parse_expr_impl = (
        expected_ty :: Option.t[Ir.Type],
        ast :: Ast.t,
    ) -> ParsedExpr => (
        let { .shape, .ty } = with_return (
            match ast.shape with (
                | :Empty => return {
                    .shape = :Expr :Unit,
                    .ty = :Unit,
                }
                | :Rule { .rule, .root } => (
                    if rule.name == "fn" then (
                        let fn = parse_fn_def(
                            ast,
                            .parent_scope = :Some (@current ScopeContext),
                        );
                        let fn_ty = {
                            .args = (
                                let mut args = ArrayList.new();
                                for arg in &fn.args |> ArrayList.iter do (
                                    &mut args |> ArrayList.push_back(arg^.ty);
                                );
                                args
                            ),
                            .result = fn.result_ty,
                        };
                        return {
                            .shape = :Expr :Fn fn,
                            .ty = :Fn fn_ty,
                        };
                    );
                    if rule.name == "field" then (
                        let { obj, field } = root
                            |> AstHelpers.expect_two_children(:Some { "obj", "field" });
                        let field_span = field.span;
                        let field = field |> AstHelpers.expect_ident;
                        let obj = parse_place_expr(:None, obj);
                        let field_ty = field_ty(obj.ty, field, field_span);
                        return {
                            .shape = :Place :Field {
                                .obj,
                                .field,
                            },
                            .ty = field_ty,
                        };
                    );
                    if rule.name == "assign" then (
                        let { assignee, value } = root
                            |> AstHelpers.expect_two_children(:Some { "assignee", "value" });
                        let assignee = parse_place_expr(:None, assignee);
                        let value = parse_expr(:Some assignee.ty, value);
                        return {
                            .shape = :Expr :Assign {
                                .assignee,
                                .value,
                            },
                            .ty = :Unit,
                        };
                    );
                    if rule.name == "uninitialized" then (
                        let expected_ty = expect_known_type(expected_ty, ast.span);
                        return { .shape = :Expr :Uninitialized, .ty = expected_ty };
                    );
                    if rule.name == "type ascribe" then (
                        let { expr, ty } = root
                            |> AstHelpers.expect_two_children(:Some { "expr", "type" });
                        let ty = parse_type(ty);
                        let expr = parse_expr_impl(:Some ty, expr);
                        return { .shape = expr.shape, .ty = expr.ty };
                    );
                    if rule.name == "if" then (
                        return parse_if(expected_ty, root);
                    );
                    if rule.name == "if_without_else" then (
                        return parse_if(expected_ty, root);
                    );
                    if rule.name == "let" then (
                        let { pattern, value } = root
                            |> AstHelpers.expect_two_children(:Some { "pattern", "value" });
                        let { name, expected_ty } = with_return (
                            if pattern.shape is :Rule { .rule, .root } then (
                                if rule.name == "type ascribe" then (
                                    let { name, ty } = root
                                        |> AstHelpers.expect_two_children(:Some { "expr", "type" });
                                    let name = name |> AstHelpers.expect_ident;
                                    return { name, :Some parse_type(ty) };
                                );
                            );
                            let name = pattern |> AstHelpers.expect_ident;
                            { name, :None }
                        );
                        let value = parse_expr(expected_ty, value);
                        &mut (@current ScopeContext).vars |> OrdMap.add(name, value.ty);
                        return {
                            .shape = :Expr :Let { .name, .value },
                            .ty = :Unit,
                        };
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
                            .shape = :Expr :Native { .parts = native_parts },
                            .ty = expected_ty |> Option.unwrap_or(:Unit),
                        };
                    );
                    if rule.name == "stmt" then (
                        let inner = root |> AstHelpers.expect_single_child(:None);
                        return {
                            .shape = :Expr :Stmt parse_expr(:None, inner),
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
                            .shape = :Expr :Scope inner,
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
                            .shape = :Expr :Then exprs,
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
                            .shape = :Expr :Apply { .f, .args },
                            .ty = f_type.result,
                        };
                    );
                )
                | :Token token => (
                    match token.shape with (
                        | :Ident ident => (
                            return {
                                .shape = :Place :Ident ident.name,
                                .ty = find_ident_ty(token.span, ident.name),
                            }
                        )
                        | :String str => (
                            return {
                                .shape = :Expr :StringLiteral str.contents,
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
        { .shape, .ty }
    );

    const parse_expr = (
        expected_ty :: Option.t[Ir.Type],
        ast :: Ast.t,
    ) -> Ir.Expr => (
        let { .shape, .ty } = parse_expr_impl(expected_ty, ast);
        let shape = match shape with (
            | :Expr shape => shape
            | :Place shape => :Claim { .shape, .ty, .span = ast.span }
        );
        { .shape, .ty, .span = ast.span }
    );

    const parse_place_expr = (
        expected_ty :: Option.t[Ir.Type],
        ast :: Ast.t,
    ) -> Ir.PlaceExpr => (
        let { .shape, .ty } = parse_expr_impl(expected_ty, ast);
        let shape = match shape with (
            | :Place shape => shape
            | :Expr shape => :Temp { .shape, .ty, .span = ast.span }
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
                    if name == "Unit" then return :Unit;
                    if name == "Bool" then return :Bool;
                    if name == "Int32" then return :Int32;
                    if name == "String" then return :String;
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

    const preprocess_toplevel_fn = (
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

    const process_const = (@"const" :: Const) => (
        let { .name, .ty, .value } = @"const";
        let mut ctx = @current Context;
        let value = parse_expr(:Some parse_type(ty), value);
        &mut ctx.program.consts |> OrdMap.add(name, value);
    );

    const parse_fn_def = (
        def :: Ast.t,
        .parent_scope :: Option.t[Scope],
    ) -> Ir.FnDef => (
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
        let result_ty = parse_type(result_ty);
        let mut scope = {
            .parent = parent_scope,
            .vars = OrdMap.new(),
        };
        for arg in &args |> ArrayList.iter do (
            &mut scope.vars |> OrdMap.add(arg^.name, arg^.ty);
        );
        with ScopeContext = scope;
        let body = parse_expr(:Some result_ty, body);
        {
            .args,
            .result_ty = result_ty,
            .body,
        }
    );

    const process_toplevel_fn = (
        .name :: String,
        .def :: Ast.t,
    ) => (
        let mut ctx = @current Context;
        let fn = parse_fn_def(def, .parent_scope = :None);
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

use (import "../log.ks").*;
use (import "../output.ks").*;
use (import "../diagnostic.ks").*;
use (import "../tuple.ks").*;
use (import "../position.ks").*;
use (import "../span.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../token.ks").*;
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
        | :Alias
        | :Native
    );

    const TopLevelKind = newtype (
        | :Type TypeKind
        | :Fn
        | :Context
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
        .top_level_item_def_span :: OrdMap.t[String, Span],
        .top_level_items :: ArrayList.t[TopLevelItem],
        .consts :: ArrayList.t[Const],
        .type_names :: OrdSet.t[String],
        .fn_types :: OrdMap.t[String, Ir.FnType],
        .program :: Ir.Program,
    };

    const init = () -> State => {
        .top_level_item_def_span = OrdMap.new(),
        .top_level_items = ArrayList.new(),
        .type_names = OrdSet.new(),
        .fn_types = OrdMap.new(),
        .consts = ArrayList.new(),
        .program = {
            .types = OrdMap.new(),
            .contexts = OrdMap.new(),
            .consts = OrdMap.new(),
            .consts_order = ArrayList.new(),
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
        let set_top_level_def = (name :: String, span :: Span) => (
            if &state^.top_level_item_def_span |> OrdMap.get(name) is :Some &other_span then (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        let output = @current Output;
                        output.write("Top level item ");
                        output.write(String.escape(name));
                        output.write(" has already been defined");
                    ),
                    .span,
                    .related = (
                        let mut related = ArrayList.new();
                        let info = {
                            .span = other_span,
                            .message = () => (
                                let output = @current Output;
                                output.write("Previous definition is here");
                            ),
                        };
                        &mut related |> ArrayList.push_back(info);
                        related
                    ),
                };
                Diagnostic.report_and_unwind(diagnostic)
            );
            &mut state^.top_level_item_def_span |> OrdMap.add(name, span);
        );
        let top_level_item = (root :: Ast.Group) => with_return (
            let name_ast = (
                &root.children
                    |> Tuple.get_named("name")
            )^
                |> Ast.unwrap_child_value;
            let value = (
                &root.children
                    |> Tuple.get_named("value")
            )^
                |> Ast.unwrap_child_value;
            if name_ast.shape is :Rule { .rule, .root } then (
                if rule.name == "type ascribe" then (
                    let { name_ast, ty } = root
                        |> AstHelpers.expect_two_children(:Some { "expr", "type" });
                    let name = name_ast |> AstHelpers.expect_ident;
                    set_top_level_def(name, name_ast.span);
                    let @"const" = {
                        .name,
                        .ty,
                        .value,
                    };
                    &mut state^.consts |> ArrayList.push_back(@"const");
                    return;
                );
            );
            let name = name_ast |> AstHelpers.expect_ident;
            set_top_level_def(name, name_ast.span);
            if value.shape is :Rule { .rule, .root } then (
                if rule.name == "type alias" then (
                    let item = {
                        .name,
                        .kind = :Type :Alias,
                        .def = root |> AstHelpers.expect_single_child(:None),
                    };
                    &mut state^.top_level_items |> ArrayList.push_back(item);
                    return;
                );
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
                if rule.name == "create_context_type" then (
                    let item = {
                        .name,
                        .kind = :Context,
                        .def = root |> AstHelpers.expect_single_child(:Some "type"),
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
        # Empty scope is need for initializing consts (they do name lookups)
        let mut scope = {
            .parent = :None,
            .vars = OrdMap.new(),
        };
        with ScopeContext = scope;
        for item in &state.top_level_items |> ArrayList.iter do (
            match item^.kind with (
                | :Type _ => (
                    &mut state.type_names |> OrdSet.add(item^.name);
                )
                | _ => ()
            )
        );
        for item in &state.top_level_items |> ArrayList.iter do (
            match item^.kind with (
                | :Fn => (
                    preprocess_toplevel_fn(item^.name, item^.def);
                )
                | :Context => (
                    &mut state.program.contexts
                        |> OrdMap.add(item^.name, parse_type(item^.def));
                )
                | _ => ()
            )
        );
        for item in &state.top_level_items |> ArrayList.iter do (
            let { .name, .kind, .def } = item^;
            match kind with (
                | :Type kind => match kind with (
                    | :Opaque => (
                        &mut state.program.types |> OrdMap.add(name, :Opaque);
                    )
                    | :Alias => (
                        &mut state.program.types |> OrdMap.add(name, :Alias parse_type(def));
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
    # TODO should take &
    const resolve_type_aliases = (ty :: Ir.Type) -> Ir.Type => (
        match ty with (
            | :Any => :Any
            | :Unit => :Unit
            | :Int32 => :Int32
            | :Int64 => :Int64
            | :Float64 => :Float64
            | :Char => :Char
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
            | :Ref referenced => :Ref resolve_type_aliases(referenced)
            | :List element => :List resolve_type_aliases(element)
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

    const TypeCheckContext = @context newtype {
        .fail :: [T] (() -> ()) -> T,
    };

    const short_type_name = (ty :: &Ir.Type) -> String => (
        match ty^ with (
            | :Any => "Any"
            | :Ref _ => "a reference"
            | :List _ => "a list"
            | :Unit => "()"
            | :Int32 => "Int32"
            | :Int64 => "Int64"
            | :Float64 => "Float64"
            | :Bool => "Bool"
            | :Char => "Char"
            | :String => "String"
            | :Named name => (
                let ctx = @current Context;
                let def = &ctx.program.types |> OrdMap.get(name) |> Option.unwrap;
                let short_def = match def^ with (
                    | :Opaque => "opaque"
                    | :Enum _ => "enum"
                    | :Union _ => "union"
                    | :Struct _ => "struct"
                    | :Alias _ => "alias"
                );
                name + " (" + short_def + ")"
            )
            | :Fn _ => "a function"
        )
    );

    const type_check_impl = (expected :: &Ir.Type, actual :: &Ir.Type) => (
        match { expected^, actual^ } with (
            | { :Any, _ } => ()
            | { _, :Any } => ()
            | { :Ref ref a, :Ref ref b } => (
                let parent_ctx = @current TypeCheckContext;
                with TypeCheckContext = {
                    .fail = [T] msg -> T => (
                        parent_ctx.fail(
                            () => (
                                let output = @current Output;
                                output.write("referenced types are different");
                                msg()
                            )
                        )
                    ),
                };
                type_check_impl(a, b);
            )
            | { :List ref a, :List ref b } => (
                let parent_ctx = @current TypeCheckContext;
                with TypeCheckContext = {
                    .fail = [T] msg -> T => (
                        parent_ctx.fail(
                            () => (
                                let output = @current Output;
                                output.write("list element types are different");
                                msg()
                            )
                        )
                    ),
                };
                type_check_impl(a, b);
            )
            | { :UnwindToken ref a, :UnwindToken ref b } => (
                let parent_ctx = @current TypeCheckContext;
                with TypeCheckContext = {
                    .fail = [T] msg -> T => (
                        parent_ctx.fail(
                            () => (
                                let output = @current Output;
                                output.write("unwind tokens' result types are different");
                                msg()
                            )
                        )
                    ),
                };
                type_check_impl(a, b);
            )
            | { :Unit, :Unit } => ()
            | { :Int32, :Int32 } => ()
            | { :Int64, :Int64 } => ()
            | { :Float64, :Float64 } => ()
            | { :Bool, :Bool } => ()
            | { :Char, :Char } => ()
            | { :String, :String } => ()
            | { :Named a, :Named b } => (
                if a != b then (
                    (@current TypeCheckContext).fail(
                        () => (
                            let output = @current Output;
                            output.write("Expected ");
                            output.write(a);
                            output.write(", got ");
                            output.write(b);
                        )
                    );
                );
            )
            | { :Fn ref a, :Fn ref b } => (
                if &a^.args |> ArrayList.length != &b^.args |> ArrayList.length then (
                    (@current TypeCheckContext).fail(
                        () => (
                            let output = @current Output;
                            output.write("Expected ");
                            output.write(to_string(&a^.args |> ArrayList.length));
                            output.write(" args, got ");
                            output.write(to_string(&b^.args |> ArrayList.length));
                        )
                    );
                );
                for i in 0..&a^.args |> ArrayList.length do (
                    let parent_ctx = @current TypeCheckContext;
                    with TypeCheckContext = {
                        .fail = [T] msg -> T => (
                            parent_ctx.fail(
                                () => (
                                    let output = @current Output;
                                    output.write("argument #");
                                    output.write(to_string(i));
                                    output.write(" is different\n");
                                    msg()
                                )
                            )
                        ),
                    };
                    type_check_impl(
                        &a^.args |> ArrayList.at(i),
                        &b^.args |> ArrayList.at(i),
                    );
                );
                let parent_ctx = @current TypeCheckContext;
                with TypeCheckContext = {
                    .fail = [T] msg -> T => (
                        parent_ctx.fail(
                            () => (
                                let output = @current Output;
                                output.write("result type is different\n");
                                msg()
                            )
                        )
                    ),
                };
                type_check_impl(
                    &a^.result,
                    &b^.result,
                );
            )
            | _ => (@current TypeCheckContext).fail(
                () => (
                    let output = @current Output;
                    output.write("Expected ");
                    output.write(short_type_name(expected));
                    output.write(", got ");
                    output.write(short_type_name(actual));
                )
            )
        );
    );

    const type_check = (span :: Span, expected :: Ir.Type, actual :: Ir.Type) => (
        with TypeCheckContext = {
            .fail = [T] (inner_msg) -> T => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        let output = @current Output;
                        output.write("Expected type ");
                        Ir.Print.type_name(&expected);
                        output.write(", got ");
                        Ir.Print.type_name(&actual);
                        output.write("\n");
                        inner_msg();
                    ),
                    .span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        };
        type_check_impl(
            &resolve_type_aliases(expected),
            &resolve_type_aliases(actual),
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

    const parse_context_type = (ast :: Ast.t) -> { String, Ir.Type } => (
        let ctx = @current Context;
        let context_name = ast |> AstHelpers.expect_ident;
        let context_ty = match &ctx.program.contexts |> OrdMap.get(context_name) with (
            | :Some &ty => ty
            | :None => (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Compiler,
                    .message = () => (
                        let output = @current Output;
                        output.write("Unknown context ");
                        output.write(String.escape(context_name));
                    ),
                    .span = ast.span,
                    .related = ArrayList.new(),
                };
                Diagnostic.report_and_unwind(diagnostic)
            )
        );
        { context_name, context_ty }
    );

    const type_info = (ty :: &Ir.Type) -> Ir.PlaceExpr => with_return (
        let ty = resolve_type_aliases(ty^);
        let ty = &ty;
        let span :: Span = {
            .start = Position.beginning(),
            .end = Position.beginning(),
            .path = :Special __FILE__
        };
        let name = output_to_string(
            () => (
                Ir.Print.type_name_as_ident(ty);
                (@current Output).write("_TypeInfo");
            )
        );
        let place_of_result :: Ir.PlaceExpr = {
            .shape = :Ident name,
            .ty = :Named "TypeInfo",
            .span,
        };
        let mut ctx = @current Context;
        if &ctx.program.consts |> OrdMap.get(name) is :Some _ then (
            return place_of_result;
        );
        # Temp add uninitialized to prevent cycles
        &mut ctx.program.consts
            |> OrdMap.add(
                name,
                {
                    .shape = :Uninitialized,
                    .ty = :Named name,
                    .span,
                }
            );
        # TODO make work with other targets than JavaScript
        let details_members = (
            members_map :: &OrdMap.t[String, Ir.Type],
        ) -> Ir.Expr => (
            let mut members = ArrayList.new();
            for &{
                .key = name,
                .value = ref ty,
            } in members_map |> OrdMap.iter do (
                let mut fields = ArrayList.new();
                let offset_or_name = {
                    .name = "offset_or_name",
                    .value = {
                        .shape = :Literal :String name,
                        .ty = :String,
                        .span,
                    },
                };
                &mut fields |> ArrayList.push_back(offset_or_name);
                let ty = {
                    .name = "ty",
                    .value = {
                        .shape = :Ref type_info(ty),
                        .ty = :Ref :Named "TypeInfo",
                        .span,
                    },
                };
                &mut fields |> ArrayList.push_back(ty);
                &mut members
                    |> ArrayList.push_back(
                        {
                            .shape = :Record fields,
                            .ty = :Named "MemberInfo",
                            .span,
                        }
                    );
            );
            let mut details = ArrayList.new();
            &mut details
                |> ArrayList.push_back(
                    {
                        .name = "members",
                        .value = {
                            .shape = :List members,
                            .ty = :List :Named "MemberInfo",
                            .span,
                        },
                    }
                );
            {
                .shape = :Record details,
                .ty = :Named "TypeInfoDefails",
                .span,
            }
        );
        let details_primitive = () -> Ir.Expr => {
            .shape = :Record (
                let mut fields = ArrayList.new();
                &mut fields |> ArrayList.push_back(
                    {
                        .name = "primitive",
                        .value = {
                            .shape = :Unit,
                            .ty = :Unit,
                            .span,
                        },
                    }
                );
                fields
            ),
            .ty = :Named "TypeInfoDefails",
            .span,
        };
        let details_inner_ty = (
            inner_ty :: &Ir.Type,
        ) -> Ir.Expr => {
            .shape = :Record (
                let mut fields = ArrayList.new();
                &mut fields |> ArrayList.push_back(
                    {
                        .name = "inner_ty",
                        .value = {
                            .shape = :Ref type_info(inner_ty),
                            .ty = :Unit,
                            .span,
                        },
                    }
                );
                fields
            ),
            .ty = :Named "TypeInfoDefails",
            .span,
        };
        let { kind, details } = match ty^ with (
            | :List ref inner_ty => { "Array", details_inner_ty(inner_ty) }
            | :Named name => (
                let def = &ctx.program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap;
                match def^ with (
                    | :Struct { .fields = ref fields } => (
                        { "Object", details_members(fields) }
                    )
                    | :Union { .variants = ref variants } => (
                        { "Object", details_members(variants) }
                    )
                    | :Enum _ => { "Primitive", details_primitive() }
                    | :Opaque => { "Primitive", details_primitive() }
                    | :Alias _ => (
                        let diagnostic = {
                            .severity = :Error,
                            .source = :Internal,
                            .message = () => (
                                let output = @current Output;
                                output.write("type info is supposed to get alias-resolved type, got ");
                                Ir.Print.type_name(ty);
                            ),
                            .span,
                            .related = ArrayList.new(),
                        };
                        Diagnostic.report_and_unwind(diagnostic)
                    )
                )
            )
            | _ => { "Primitive", details_primitive() }
        );
        let mut fields = ArrayList.new();
        let details = {
            .name = "details",
            .value = details,
        };
        &mut fields |> ArrayList.push_back(details);
        let kind = {
            .name = "kind",
            .value = {
                .shape = :Variant kind,
                .ty = :Named "TypeKind",
                .span,
            },
        };
        &mut fields |> ArrayList.push_back(kind);
        &mut ctx.program.consts
            |> OrdMap.add(
                name,
                {
                    .shape = :Record fields,
                    .ty = :Named name,
                    .span,
                }
            );
        &mut ctx.program.consts_order |> ArrayList.push_back(name);
        place_of_result
    );

    const expect_ty_enum = (
        ty :: &Ir.Type,
        .span :: Span,
    ) -> &OrdSet.t[String] => with_return (
        let ctx = @current Context;
        match resolve_type_aliases(ty^) with (
            | :Named name => (
                let def = &ctx.program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap;
                match def^ with (
                    | :Enum { .variants = ref variants } => (
                        return variants;
                    )
                    | _ => ()
                );
            )
            | _ => ()
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                let output = @current Output;
                output.write("Expected a variant type, got ");
                Ir.Print.type_name(ty);
            ),
            .span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );

    const WithSpan = [T] newtype {
        T,
        .span :: Span,
    };

    const check_variant = (
        .variant :: WithSpan[String],
        .ty :: WithSpan[type (&Ir.Type)],
    ) -> () => (
        let variant_names = expect_ty_enum(...ty);
        if not variant_names |> OrdSet.contains(variant.0) then (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    let output = @current Output;
                    output.write("This variant doesn't exist in type ");
                    Ir.Print.type_name(ty.0);
                ),
                .span = variant.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        );
    );

    const parse_expr_impl = (
        expected_ty :: Option.t[Ir.Type],
        ast :: Ast.t,
    ) -> ParsedExpr => (
        let ctx = @current Context;
        let { .shape, .ty } = with_return (
            match ast.shape with (
                | :Empty => return {
                    .shape = :Expr :Unit,
                    .ty = :Unit,
                }
                | :Rule { .rule, .root } => (
                    if rule.name == "variant" then (
                        let variant = root
                            |> AstHelpers.expect_single_child(:Some "label")
                            |> AstHelpers.expect_ident;
                        let ty = expected_ty |> expect_known_type(ast.span);
                        check_variant(
                            .variant = { variant, .span = ast.span },
                            .ty = { &ty, .span = ast.span },
                        );
                        return {
                            .shape = :Expr :Variant variant,
                            .ty,
                        };
                    );
                    if rule.name == "==" then (
                        let { lhs, rhs } = root
                            |> AstHelpers.expect_two_children(:None);
                        let enum = parse_expr(:None, lhs);
                        let variant = rhs
                            |> AstHelpers.expect_rule("variant")
                            |> AstHelpers.expect_single_child(:Some "label")
                            |> AstHelpers.expect_ident;
                        check_variant(
                            .variant = { variant, .span = rhs.span },
                            .ty = { &enum.ty, .span = lhs.span },
                        );
                        return {
                            .shape = :Expr :EnumIs { .enum, .variant },
                            .ty = :Bool,
                        };
                    );
                    if rule.name == "type_info" then (
                        let ty = root
                            |> AstHelpers.expect_single_child(:Some "type")
                            |> parse_type;
                        let type_info = type_info(&ty);
                        return {
                            .shape = :Place type_info.shape,
                            .ty = type_info.ty,
                        };
                    );
                    if rule.name == "index" then (
                        let { list_ast, index_ast } = root
                            |> AstHelpers.expect_two_children(:Some { "list", "index" });
                        let list = parse_place_expr(:None, list_ast);
                        let element_ty = match list.ty with (
                            | :List ty => ty
                            | other => (
                                let diagnostic = {
                                    .severity = :Error,
                                    .source = :Compiler,
                                    .message = () => (
                                        let output = @current Output;
                                        output.write("Expected a list, got ");
                                        Ir.Print.type_name(&other);
                                    ),
                                    .span = list_ast.span,
                                    .related = ArrayList.new(),
                                };
                                Diagnostic.report_and_unwind(diagnostic)
                            )
                        );
                        let index = parse_expr(:Some :Int32, index_ast);
                        return {
                            .shape = :Place :Index {
                                .list,
                                .index,
                            },
                            .ty = element_ty,
                        };
                    );
                    if rule.name == "list" then (
                        let inner = root |> AstHelpers.expect_single_child(:None);
                        let element_ty = match expected_ty |> expect_known_type(ast.span) with (
                            | :List ty => ty
                            | other => (
                                let diagnostic = {
                                    .severity = :Error,
                                    .source = :Compiler,
                                    .message = () => (
                                        let output = @current Output;
                                        output.write("Expected ");
                                        Ir.Print.type_name(&other);
                                        output.write(", got a list");
                                    ),
                                    .span = ast.span,
                                    .related = ArrayList.new(),
                                };
                                Diagnostic.report_and_unwind(diagnostic)
                            )
                        );
                        let mut elements = ArrayList.new();
                        for element in Ast.iter_list(
                            inner,
                            .binary_rule_name = "comma",
                            .trailing_or_leading_rule_name = :Some "trailing comma",
                        ) do (
                            let element = parse_expr(:Some element_ty, element);
                            &mut elements |> ArrayList.push_back(element);
                        );
                        return {
                            .shape = :Expr :List elements,
                            .ty = :List element_ty,
                        };
                    );
                    if rule.name == "deref" then (
                        let reference_ast = root |> AstHelpers.expect_single_child(:None);
                        let expected_reference_ty = match expected_ty with (
                            | :None => :None
                            | :Some ty => :Some :Ref ty
                        );
                        let reference = parse_expr(expected_reference_ty, reference_ast);
                        let ty = match resolve_type_aliases(reference.ty) with (
                            | :Ref referenced => referenced
                            | _ => (
                                let diagnostic = {
                                    .severity = :Error,
                                    .source = :Compiler,
                                    .message = () => (
                                        (@current Output).write("Expected a reference, got");
                                        Ir.Print.type_name(&reference.ty);
                                    ),
                                    .span = reference_ast.span,
                                    .related = ArrayList.new(),
                                };
                                Diagnostic.report_and_unwind(diagnostic)
                            )
                        );
                        return {
                            .shape = :Place :Deref reference,
                            .ty,
                        };
                    );
                    if rule.name == "ref" then (
                        let referenced = root |> AstHelpers.expect_single_child(:None);
                        let expected_referenced_ty = match expected_ty with (
                            | :None => :None
                            | :Some ty => match resolve_type_aliases(ty) with (
                                | :Ref referenced => :Some referenced
                                | _ => (
                                    let diagnostic = {
                                        .severity = :Error,
                                        .source = :Compiler,
                                        .message = () => (
                                            (@current Output).write("Expected not a reference but ");
                                            Ir.Print.type_name(&ty);
                                        ),
                                        .span = ast.span,
                                        .related = ArrayList.new(),
                                    };
                                    Diagnostic.report_and_unwind(diagnostic)
                                )
                            )
                        );
                        let referenced = parse_place_expr(expected_referenced_ty, referenced);
                        return {
                            .shape = :Expr :Ref referenced,
                            .ty = :Ref referenced.ty
                        };
                    );
                    if rule.name == "record" then (
                        let ty = match expected_ty with (
                            | :Some ty => ty
                            | :None => (
                                let diagnostic = {
                                    .severity = :Error,
                                    .source = :Compiler,
                                    .message = () => (
                                        (@current Output).write("Couldn't figure out the type of this record");
                                    ),
                                    .span = ast.span,
                                    .related = ArrayList.new(),
                                };
                                Diagnostic.report_and_unwind(diagnostic)
                            )
                        );
                        const Kind = newtype (
                            | :Union
                            | :Struct
                        );
                        let {
                            kind :: Kind,
                            field_types :: OrdMap.t[String, Ir.Type],
                        } = with_return (
                            match resolve_type_aliases(ty) with (
                                | :Named name => (
                                    let def = &ctx.program.types |> OrdMap.get(name) |> Option.unwrap;
                                    match def^ with (
                                        | :Struct { .fields } => return { :Struct, fields }
                                        | :Union { .variants } => return { :Union, variants }
                                        | _ => ()
                                    )
                                )
                                | _ => ()
                            );
                            let diagnostic = {
                                .severity = :Error,
                                .source = :Compiler,
                                .message = () => (
                                    let output = @current Output;
                                    output.write("Type ");
                                    Ir.Print.type_name(&ty);
                                    output.write(" can't be used as record");
                                ),
                                .span = ast.span,
                                .related = ArrayList.new(),
                            };
                            Diagnostic.report_and_unwind(diagnostic)
                        );
                        let mut fields_initialized = OrdMap.new();
                        let mut fields = ArrayList.new();
                        for field in Ast.iter_list(
                            root |> AstHelpers.expect_single_child(:None),
                            .binary_rule_name = "comma",
                            .trailing_or_leading_rule_name = :Some "trailing comma",
                        ) do (
                            let { name_ast, value } = field
                                |> AstHelpers.expect_rule("field init")
                                |> AstHelpers.expect_two_children(:Some { "label", "value" });
                            let name = name_ast |> AstHelpers.expect_ident;
                            let field_ty = match &field_types |> OrdMap.get(name) with (
                                | :Some &ty => ty
                                | :None => (
                                    let diagnostic = {
                                        .severity = :Error,
                                        .source = :Compiler,
                                        .message = () => (
                                            let output = @current Output;
                                            output.write("Type ");
                                            Ir.Print.type_name(&ty);
                                            output.write("doesn't have field ");
                                            output.write(String.escape(name));
                                        ),
                                        .span = name_ast.span,
                                        .related = ArrayList.new(),
                                    };
                                    Diagnostic.report_and_unwind(diagnostic)
                                )
                            );
                            if &fields_initialized |> OrdMap.get(name) is :Some &other_span then (
                                let diagnostic = {
                                    .severity = :Error,
                                    .source = :Compiler,
                                    .message = () => (
                                        let output = @current Output;
                                        output.write("Field ");
                                        output.write(String.escape(name));
                                        output.write(" has already been initialized");
                                    ),
                                    .span = name_ast.span,
                                    .related = (
                                        let mut related = ArrayList.new();
                                        let info = {
                                            .span = other_span,
                                            .message = () => (
                                                let output = @current Output;
                                                output.write("Previously field was initialized here");
                                            ),
                                        };
                                        &mut related |> ArrayList.push_back(info);
                                        related
                                    ),
                                };
                                Diagnostic.report_and_unwind(diagnostic)
                            );
                            &mut fields_initialized |> OrdMap.add(name, name_ast.span);
                            let value = parse_expr(:Some field_ty, value);
                            &mut fields |> ArrayList.push_back({ .name, .value });
                        );
                        return {
                            .shape = :Expr :Record fields,
                            .ty,
                        };
                    );
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
                    if rule.name == "inject_context" then (
                        let { context_type, value } = root
                            |> AstHelpers.expect_two_children(:Some { "context_type", "value" });
                        let { context_name, context_ty } = parse_context_type(context_type);
                        return {
                            .shape = :Expr :InjectContext {
                                .name = context_name,
                                .value = parse_expr(:Some context_ty, value),
                            },
                            .ty = :Unit,
                        };

                    );
                    if rule.name == "current_context" then (
                        let context_type = root
                            |> AstHelpers.expect_single_child(:Some "context_type");
                        let { context_name, context_ty } = parse_context_type(context_type);
                        return {
                            .shape = :Place :CurrentContext context_name,
                            .ty = context_ty,
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
                            last_expr_ty = expr.ty;
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
                        let f_type = match resolve_type_aliases(f.ty) with (
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
                        | :Number { .raw, ... } => (
                            let ty = expect_known_type(expected_ty, token.span);
                            # TODO catch parse errors
                            let literal = match resolve_type_aliases(ty) with (
                                | :Int32 => :Int32 parse(raw)
                                | :Int64 => :Int64 parse(raw)
                                | :Float64 => :Float64 parse(raw)
                                | _ => (
                                    let diagnostic = {
                                        .severity = :Error,
                                        .source = :Compiler,
                                        .message = () => (
                                            (@current Output).write("Number literal can't be ");
                                            Ir.Print.type_name(&ty);
                                        ),
                                        .span = token.span,
                                        .related = ArrayList.new(),
                                    };
                                    Diagnostic.report_and_unwind(diagnostic)
                                )
                            );
                            return {
                                .shape = :Expr :Literal literal,
                                .ty,
                            };
                        )
                        | :String str => (
                            let open_raw = Token.raw(str.open);
                            let { ty, literal } = if open_raw == "\"" then (
                                { :String, :String str.contents }
                            ) else if open_raw == "'" then (
                                if String.length(str.contents) == 0 then (
                                    let diagnostic = {
                                        .severity = :Error,
                                        .source = :Compiler,
                                        .message = () => (
                                            (@current Output).write("Char literal can't be empty");
                                        ),
                                        .span = token.span,
                                        .related = ArrayList.new(),
                                    };
                                    Diagnostic.report_and_unwind(diagnostic)
                                );
                                let c = String.at(str.contents, 0);
                                if Char.string_encoding_len(c) != String.length(str.contents) then (
                                    let diagnostic = {
                                        .severity = :Error,
                                        .source = :Compiler,
                                        .message = () => (
                                            (@current Output).write("Char literal must have exactly 1 char");
                                        ),
                                        .span = token.span,
                                        .related = ArrayList.new(),
                                    };
                                    Diagnostic.report_and_unwind(diagnostic)
                                );
                                { :Char, :Char c }
                            ) else (
                                panic("Unkown string delimeter")
                            );
                            return {
                                .shape = :Expr :Literal literal,
                                .ty,
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
        let ty = if expected_ty is :Some expected_ty then (
            type_check(ast.span, expected_ty, ty);
            expected_ty
        ) else (
            ty
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
                if rule.name == "ref" then (
                    let referenced = root |> AstHelpers.expect_single_child(:None);
                    return :Ref parse_type(referenced);
                );
                if rule.name == "unwind_token" then (
                    let result_ty = root |> AstHelpers.expect_single_child(:None);
                    return :UnwindToken parse_type(result_ty);
                );
                if rule.name == "list" then (
                    let element = root |> AstHelpers.expect_single_child(:None);
                    return :List parse_type(element);
                );
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
                    if name == "Any" then return :Any;
                    if name == "Unit" then return :Unit;
                    if name == "Bool" then return :Bool;
                    if name == "Int32" then return :Int32;
                    if name == "Int64" then return :Int64;
                    if name == "Float64" then return :Float64;
                    if name == "Char" then return :Char;
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
        &mut ctx.program.consts_order |> ArrayList.push_back(name);
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

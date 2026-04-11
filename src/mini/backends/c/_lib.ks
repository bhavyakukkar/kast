use (import "../../ir.ks").*;
use std.collections.OrdMap;
use std.collections.OrdSet;

module:

const C = (
    module:

    use (import "./ast.ks").*;

    const Compiled = Ast.Program;

    const print = (compiled :: Compiled) => (
        Ast.Print.program(&compiled);
    );

    const ContextT = newtype {
        .program :: Ir.Program,
        .defined_types :: OrdSet.t[String],
        .result :: Ast.Program,
        .next_id :: Int32,
    };
    const Context = @context ContextT;

    const ScopeT = newtype {
        .block :: &mut Ast.Block,
    };
    const Scope = @context ScopeT;

    const ident = (name :: String) -> Ast.Ident => (
        # TODO make sure its valid C identifier
        {
            .name
        }
    );

    const new_ident = (name_prefix :: String) -> Ast.Ident => (
        let mut ctx = @current Context;
        let ident = ident(name_prefix + to_string(ctx.next_id));
        ctx.next_id += 1;
        ident
    );

    const convert_ty = (ty :: &Ir.Type) -> Ast.Ty => (
        match ty^ with (
            | :Any => :Void
            | :Ref ref t => :Pointer convert_ty(t)
            | :Unit => :Void
            | :Int32 => :Int32
            | :Int64 => :Int64
            | :Float64 => :Float64
            | :Bool => :Bool
            | :Char => :Char
            | :Named name => :Named ident(name)
            | :Native s => :Raw s
        # | :Fn FnType
        )
    );

    const Pure = newtype {
        .expr :: Option.t[Ast.Expr],
    };

    const void = () -> Pure => {
        .expr = :None,
    };

    const pure = (pure :: Pure) -> Ast.Expr => (
        match pure.expr with (
            | :Some expr => expr
            | :None => panic("Pure doesnt have expr")
        )
    );

    const Place = newtype {
        .get :: () -> Pure,
        .set :: Ast.Expr -> (),
    };

    const calculate_place = (ir_expr :: &Ir.PlaceExpr) -> Place => with_return (
        match ir_expr^.shape with (
            | :Ident name => {
                .get = () => { .expr = :Some :Ident ident(name) },
                .set = value => (
                    insert_stmt(:Assign { .assignee = :Ident ident(name), .value });
                ),
            }
            | :Field { .obj = ref obj, .field } => (
                let obj = calculate_place(obj);
                let field_expr = :Field {
                    .obj = pure(obj.get()),
                    .field = ident(field),
                };
                {
                    .get = () => { .expr = :Some field_expr },
                    .set = value => (
                        insert_stmt(:Assign { .assignee = field_expr, .value });
                    ),
                }
            )
            # | :Index 
            # | :CurrentContext String
            # | :Deref Expr
            | :Temp ref expr => (
                let value = calculate(expr);
                {
                    .get = () => value,
                    .set = _ => panic("can't assign to temp expr"),
                }
            )
        )
    );

    const calculate_literal = (literal :: &Ir.Literal) -> Pure => with_return (
        let literal :: Ast.Literal = match literal^ with (
            | :Bool x => :Bool x
            | :Int32 x => :Int32 x
            | :Int64 x => :Int64 x
            | :Float64 x => :Float64 x
            | :Char x => :Char x
            | :String s => return {
                .expr = :Some :CompoundLiteral {
                    .ty = :Named ident("StringView"),
                    .fields = (
                        let mut fields = ArrayList.new();
                        let contents = {
                            .name = ident("contents"),
                            .value = :Literal :String s,
                        };
                        &mut fields |> ArrayList.push_back(contents);
                        let length = {
                            .name = ident("length"),
                            .value = :Literal :Int String.length(s),
                        };
                        &mut fields |> ArrayList.push_back(length);
                        fields
                    )
                },
            }
        );
        { .expr = :Some :Literal literal }
    );

    const calculate = (ir_expr :: &Ir.Expr) -> Pure => with_return (
        let mut ctx = @current Context;
        let expr :: Ast.Expr = match ir_expr^.shape with (
            | :Unit => return void()
            # | :Uninitialized
            | :Claim ref place => return calculate_place(place).get()
            # | :Ref PlaceExpr
            | :Native { .parts = ref parts } => (
                let mut raw_parts = ArrayList.new();
                for part in parts |> ArrayList.iter do (
                    match part^ with (
                        | :Raw s => (
                            if String.strip_prefix(s, .prefix = "#include ") is :Some @"include" then (
                                &mut ctx.result.includes |> OrdSet.add(@"include");
                                return void();
                            );
                            &mut raw_parts |> ArrayList.push_back(:Raw s);
                        )
                        | :Interpolated ref expr => (
                            &mut raw_parts |> ArrayList.push_back(pure(calculate(expr)));
                        )
                    )
                );
                :RawParts raw_parts
            )
            | :Literal ref literal => return calculate_literal(literal)
            # | :Variant String
            | :Stmt ref expr => (
                calculate(expr);
                return void()
            )
            | :Then ref exprs => (
                let mut result = void();
                for expr in exprs |> ArrayList.iter do (
                    result = calculate(expr);
                );
                return result
            )
            | :If { .cond = ref cond, .then_case = ref then_case, .else_case = ref else_case } => (
                let ident = new_ident("if_result");
                let is_void = if ir_expr^.ty is :Unit then true else false;
                if not is_void then (
                    insert_stmt(
                        :LetVar {
                            .ty = convert_ty(&ir_expr^.ty),
                            .ident,
                            .value = :None,
                        }
                    );
                );
                let cond = pure(calculate(cond));
                let then_case = (
                    let mut block = new_block();
                    with Scope = {
                        .block = &mut block,
                    };
                    if calculate(then_case).expr is :Some result then (
                        insert_stmt(:Assign { .assignee = :Ident ident, .value = result });
                    );
                    block
                );
                let else_case = match else_case^ with (
                    | :None => :None
                    | :Some ref else_case => (
                        let mut block = new_block();
                        with Scope = {
                            .block = &mut block,
                        };
                        if calculate(else_case).expr is :Some result then (
                            insert_stmt(:Assign { .assignee = :Ident ident, .value = result });
                        );
                        :Some block
                    )
                );
                insert_stmt(:If { .cond, .then_case, .else_case });
                return { .expr = if is_void then :None else :Some :Ident ident }
            )
            # | :Let { .name, .value }
            # | :Assign { .assignee, .value }
            # | :Fn FnDef
            | :Scope ref expr => (
                with Scope = {
                    .block = (@current Scope).block,
                };
                return calculate(expr)
            )
            | :Apply { .f = ref f, .args = ref ir_args } => (
                let f = pure(calculate(f));
                let mut args = ArrayList.new();
                for arg in ir_args |> ArrayList.iter do (
                    let arg = pure(calculate(arg));
                    &mut args |> ArrayList.push_back(arg);
                );
                :Apply { .f, .args }
            )
        # | :InjectContext
        # | :Record ArrayList.t[Field]
        # | :EnumIs
        );
        if ir_expr^.ty is :Unit then (
            insert_stmt(:Expr expr);
            void()
        ) else (
            let ident = new_ident("value");
            let_var(&ir_expr^.ty, ident, expr);
            { .expr = :Some :Ident ident }
        )
    );

    const let_var = (ty :: &Ir.Type, ident :: Ast.Ident, value :: Ast.Expr) => (
        insert_stmt(
            :LetVar {
                .ty = convert_ty(ty),
                .ident,
                .value = :Some value,
            }
        );
    );

    const insert_stmt = (stmt :: Ast.Stmt) => (
        &mut (@current Scope).block^.stmts |> ArrayList.push_back(stmt);
    );

    const new_block = () -> Ast.Block => {
        .stmts = ArrayList.new(),
    };

    const add_fn = (
        name :: String,
        def :: &Ir.FnDef,
    ) => (
        let mut ctx = @current Context;
        let mut args = ArrayList.new();
        for arg in &def^.args |> ArrayList.iter do (
            let arg :: Ast.FnArg = {
                .name = ident(arg^.name),
                .ty = convert_ty(&arg^.ty),
            };
            &mut args |> ArrayList.push_back(arg);
        );
        let fn :: Ast.Fn = {
            .signature = {
                .name = ident(name),
                .args,
                .result_ty = if name == "main" then (
                    if def^.result_ty is :Unit then (
                        :Int
                    ) else (
                        panic("main must return unit")
                    )
                ) else (
                    convert_ty(&def^.result_ty)
                ),
            },
            .body = (
                let mut block = new_block();
                with Scope = { .block = &mut block };
                let result = calculate(&def^.body);
                if result.expr is :Some expr then (
                    insert_stmt(:Return expr);
                );
                if name == "main" then (
                    insert_stmt(:Return :Literal :Int 0);
                );
                block
            ),
        };
        &mut ctx.result.fns |> ArrayList.push_back(fn);
    );

    const make_sure_all_are_defined = (ty :: &Ir.Type) => (
        match ty^ with (
            | :Any => ()
            | :Ref ref inner => make_sure_all_are_defined(inner)
            | :Unit => ()
            | :Int32 => ()
            | :Int64 => ()
            | :Float64 => ()
            | :Bool => ()
            | :Char => ()
            | :Named name => (
                let def = &(@current Context).program.types
                    |> OrdMap.get(name)
                    |> Option.unwrap;
                type_def(name, def);
            )
            | :Fn { .args = ref args, .result = ref result } => (
                for arg in args |> ArrayList.iter do (
                    make_sure_all_are_defined(arg);
                );
                make_sure_all_are_defined(result);
            )
            | :Native String => ()
        )
    );

    const type_def = (name :: String, def :: &Ir.TypeDef) => with_return (
        let mut ctx = @current Context;
        if &ctx.defined_types |> OrdSet.contains(name) then (
            return;
        );
        &mut ctx.defined_types |> OrdSet.add(name);
        match def^ with (
            | :Opaque => ()
            | :Enum _ => ()
            | :Union { .variants = ref variants } => (
                for &{ .key = _, .value = ref ty } in variants |> OrdMap.iter do (
                    make_sure_all_are_defined(ty);
                );
            )
            | :Struct { .fields = ref fields } => (
                for &{ .key = _, .value = ref ty } in fields |> OrdMap.iter do (
                    make_sure_all_are_defined(ty);
                );
            )
            | :Alias ref ty => (
                make_sure_all_are_defined(ty);
            )
        );
        let def :: Ast.TyDefShape = match def^ with (
            | :Opaque => :Alias :Pointer :Void
            | :Enum { .variants = ref variants } => :Enum (
                let mut idents = ArrayList.new();
                for &variant in variants |> OrdSet.iter do (
                    &mut idents |> ArrayList.push_back(ident(variant));
                );
                { .variants = idents }
            )
            | :Union { .variants = ref variants } => :Union (
                let mut ast_fields = ArrayList.new();
                for &{ .key = name, .value = ref ty } in variants |> OrdMap.iter do (
                    let field = {
                        .name = ident(name),
                        .ty = convert_ty(ty),
                    };
                    &mut ast_fields |> ArrayList.push_back(field);
                );
                { .fields = ast_fields }
            )
            | :Struct { .fields = ref fields } => :Struct (
                let mut ast_fields = ArrayList.new();
                for &{ .key = name, .value = ref ty } in fields |> OrdMap.iter do (
                    let field = {
                        .name = ident(name),
                        .ty = convert_ty(ty),
                    };
                    &mut ast_fields |> ArrayList.push_back(field);
                );
                { .fields = ast_fields }
            )
            | :Alias ref ty => :Alias convert_ty(ty)
        );
        let def :: Ast.TyDef = {
            .name = ident(name),
            .def,
        };
        &mut ctx.result.types |> ArrayList.push_back(def);
    );

    const compile = (program :: Ir.Program) -> Compiled => (
        with Context = {
            .defined_types = OrdSet.new(),
            .program,
            .result = {
                .includes = OrdSet.new(),
                .types = ArrayList.new(),
                .fns = ArrayList.new(),
            },
            .next_id = 0,
        };
        let mut ctx = @current Context;
        # for int32_t and similar
        &mut ctx.result.includes
            |> OrdSet.add("<stdint.h>");
        &mut ctx.result.includes |> OrdSet.add("<stdbool.h>");
        for &{ .key = name, .value = ref ty_def } in &ctx.program.types |> OrdMap.iter do (
            type_def(name, ty_def);
        );
        for &{ .key = name, .value = ref def } in &ctx.program.fns |> OrdMap.iter do (
            add_fn(name, def);
        );
        ctx.result
    );
);

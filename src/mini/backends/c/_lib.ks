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
            # | :List Type
            # | :UnwindToken Type
            | :Unit => :Void
            | :Int32 => :Int32
            | :Int64 => :Int64
            | :Float64 => :Float64
            | :Bool => :Bool
            | :Char => :Char
            # | :String => 
            | :Named name => :Named ident(name)
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
                )
            }
            # | :Field 
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

    const calculate_literal = (literal :: &Ir.Literal) -> Pure => (
        let literal :: Ast.Literal = match literal^ with (
            | :Bool x => :Bool x
            | :Int32 x => :Int32 x
            | :Int64 x => :Int64 x
            | :Float64 x => :Float64 x
            | :Char x => :Char x
            | :String x => :String x
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
                insert_stmt(:If {.cond, .then_case, .else_case });
                return { .expr = if is_void then :None else :Some :Ident ident }
            )
            # | :Let { .name, .value }
            # | :Assign { .assignee, .value }
            # | :List
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

    const compile = (program :: Ir.Program) -> Compiled => (
        with Context = {
            .program,
            .result = {
                .includes = OrdSet.new(),
                .fns = ArrayList.new(),
            },
            .next_id = 0,
        };
        let mut ctx = @current Context;
        # for int32_t and similar
        &mut ctx.result.includes |> OrdSet.add("<stdint.h>");
        &mut ctx.result.includes |> OrdSet.add("<stdbool.h>");
        for &{ .key = name, .value = ref def } in &ctx.program.fns |> OrdMap.iter do (
            add_fn(name, def);
        );
        ctx.result
    );
);

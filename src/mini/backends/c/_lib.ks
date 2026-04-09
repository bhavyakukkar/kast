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

    const calculate = (ir_expr :: &Ir.Expr) -> Pure => with_return (
        let mut ctx = @current Context;
        let expr :: Ast.Expr = match ir_expr^.shape with (
            # | :Unit
            # | :Uninitialized
            # | :Claim PlaceExpr
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
            # | :Literal Literal
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
            # | :If
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
                .value,
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
        let ctx = @current Context;
        for &{ .key = name, .value = ref def } in &ctx.program.fns |> OrdMap.iter do (
            add_fn(name, def);
        );
        ctx.result
    );
);

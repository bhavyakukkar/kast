use (import "../../ir.ks").*;
use std.collections.OrdMap;

module:

const JavaScript = (
    module:

    use (import "./ast.ks").*;

    const State = newtype {
        .next_var_id :: Int32,
        .top_level :: Ast.Block,
    };
    const Context = @context State;

    const ScopeT = newtype {
        .block :: &mut Ast.Block,
        .ctx_var :: Ast.Var,
    };
    const Scope = @context ScopeT;

    const new_block = () -> Ast.Block => {
        .stmts = ArrayList.new(),
    };

    const insert_stmt = (stmt :: Ast.Stmt) => (
        &mut (@current Scope).block^.stmts |> ArrayList.push_back(stmt);
    );

    const var = (name :: String) -> Ast.Var => (
        { .name }
    );

    const new_var = (prefix :: String) -> Ast.Var => (
        let mut ctx = @current Context;
        let name = prefix + "_" + to_string(ctx.next_var_id);
        ctx.next_var_id += 1;
        { .name }
    );
    ## calculates expr and stores it in a var
    const calculate = (expr :: Ir.Expr) -> Ast.Var => with_return (
        let value :: Ast.Expr = match expr.shape with (
            | :Unit => :Null
            | :StringLiteral s => :StringLiteral s
            | :Let { .name, .value } => (
                let value = calculate(value);
                let_var(var(name), :Var value);
                :Undefined
            )
            | :Native { .parts = ir_parts } => (
                let mut parts = ArrayList.new();
                for part in ir_parts |> ArrayList.into_iter do (
                    let part = match part with (
                        | :Raw s => :Raw s
                        | :Interpolated expr => :Var calculate(expr)
                    );
                    &mut parts |> ArrayList.push_back(part);
                );
                :RawConcat { .parts }
            )
            | :Ident name => :Var var(name)
            | :Stmt expr => (
                calculate(expr);
                :Null
            )
            | :If { .cond, .then_case, .else_case } => (
                let result_var = new_var("if_result");
                let_var(result_var, :Undefined);
                let cond = calculate(cond);
                let then_case = (
                    let mut block = new_block();
                    with Scope = {
                        .block = &mut block,
                        .ctx_var = (@current Scope).ctx_var,
                    };
                    assign(result_var, :Var calculate(then_case));
                    block
                );
                let else_case = match else_case with (
                    | :Some else_case => :Some (
                        let mut block = new_block();
                        with Scope = {
                            .block = &mut block,
                            .ctx_var = (@current Scope).ctx_var,
                        };
                        assign(result_var, :Var calculate(else_case));
                        block
                    )
                    | :None => :None
                );
                insert_stmt(
                    :If {
                        .cond = :Var cond,
                        .then_case,
                        .else_case,
                    }
                );
                return result_var
            )
            | :Then exprs => (
                let mut result = :Null;
                for expr in exprs |> ArrayList.into_iter do (
                    result = :Var calculate(expr);
                );
                result
            )
            | :Scope expr => return calculate(expr)
            | :Apply { .f, .args = ir_args } => (
                let f :: Ast.Var = calculate(f);
                let mut args :: ArrayList.t[Ast.Expr] = ArrayList.new();
                &mut args |> ArrayList.push_back(:Var (@current Scope).ctx_var);
                for arg in ir_args |> ArrayList.into_iter do (
                    let arg :: Ast.Expr = :Var calculate(arg);
                    &mut args |> ArrayList.push_back(arg);
                );
                :Apply {
                    .f = :Var f,
                    .args,
                }
            )
        );
        let result_var = new_var("temp");
        let_var(result_var, value);
        result_var
    );

    const compile_fn = (fn :: Ir.FnDef) -> Ast.Expr => (
        let mut args = ArrayList.new();
        let ctx_var = new_var("ctx");
        &mut args |> ArrayList.push_back(ctx_var);
        for arg in fn.args |> ArrayList.into_iter do (
            let arg = var(arg.name);
            &mut args |> ArrayList.push_back(arg);
        );
        let mut body = new_block();
        with Scope = {
            .block = &mut body,
            .ctx_var,
        };
        let result_var = calculate(fn.body);
        insert_stmt(:Return :Var result_var);
        :Fn {
            .args,
            .body,
        }
    );

    const let_var = (var :: Ast.Var, value :: Ast.Expr) => (
        let stmt = :Let { .var, .value };
        insert_stmt(stmt);
    );

    const assign = (var :: Ast.Var, value :: Ast.Expr) => (
        let stmt = :Assign { .var, .value };
        insert_stmt(stmt);
    );

    const Compiled = newtype {
        .top_level :: Ast.Block,
    };

    const print = (compiled :: Compiled) => (
        Ast.Print.stmts(compiled.top_level.stmts);
    );

    const compile = (program :: Ir.Program) -> Compiled => (
        let mut state :: State = {
            .next_var_id = 0,
            .top_level = new_block(),
        };
        with Context = state;
        let ctx_var = new_var("ctx");
        with Scope = {
            .block = &mut state.top_level,
            .ctx_var,
        };
        let_var(ctx_var, :Obj ArrayList.new());
        for { .key = name, .value = def } in program.fns |> OrdMap.into_iter do (
            let_var(var(name), compile_fn(def));
        );
        for { .key = name, .value } in program.consts |> OrdMap.into_iter do (
            let_var(var(name), :Var calculate(value));
        );
        if &program.fns |> OrdMap.get("main") is :Some _ then (
            insert_stmt(
                :Expr :Apply {
                    .f = :Var var("main"),
                    .args = (
                        let mut args = ArrayList.new();
                        &mut args |> ArrayList.push_back(:Var ctx_var);
                        args
                    ),
                }
            );
        );
        { .top_level = state.top_level }
    );
);

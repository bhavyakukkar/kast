use (import "../../output.ks").*;
use (import "../ir.ks").*;
use std.collections.OrdMap;

module:

const JavaScript = (
    module:

    const Ast = (
        module:

        const Var = newtype {
            .name :: String,
        };

        const Expr = newtype (
            | :Raw String
            | :RawConcat {
                .parts :: ArrayList.t[Expr],
            }
            | :Undefined
            | :Null
            | :StringLiteral String
            | :Var Var
            | :Fn {
                .args :: ArrayList.t[Var],
                .body :: Block,
            }
            | :Apply {
                .f :: Expr,
                .args :: ArrayList.t[Expr],
            }
        );

        const Stmt = newtype (
            | :Expr Expr
            | :Let {
                .var :: Var,
                .value :: Expr,
            }
            | :Assign {
                .var :: Var,
                .value :: Expr,
            }
            | :If {
                .cond :: Expr,
                .then_case :: Block,
                .else_case :: Option.t[Block],
            }
            | :Scope Block
            | :Return Expr
        );

        const Block = newtype {
            .stmts :: ArrayList.t[Stmt],
        };

        const Print = (
            module:

            const expr = (self :: Expr) => (
                let output = @current Output;
                output.write("(");
                match self with (
                    | :Raw s => output.write(s)
                    | :RawConcat { .parts } => (
                        for part in parts |> ArrayList.into_iter do (
                            if part is :Raw s then (
                                output.write(s);
                            ) else (
                                Print.expr(part);
                            );
                        );
                    )
                    | :Null => output.write("null")
                    | :Undefined => output.write("undefined")
                    | :StringLiteral s => output.write(String.escape(s))
                    | :Var var => output.write(var.name)
                    | :Fn { .args, .body } => (
                        output.write("(");
                        for { i, arg } in args |> ArrayList.into_iter |> std.iter.enumerate do (
                            if i != 0 then (
                                output.write(", ");
                            );
                            output.write(arg.name);
                        );
                        output.write(") => ");
                        Print.block(body);
                    )
                    | :Apply { .f, .args } => (
                        Print.expr(f);
                        output.write("(");
                        for { i, arg } in args |> ArrayList.into_iter |> std.iter.enumerate do (
                            if i != 0 then (
                                output.write(", ");
                            );
                            Print.expr(arg);
                        );
                        output.write(")");
                    )
                );
                output.write(")");
            );

            const stmt = (self :: Stmt) => (
                let output = @current Output;
                match self with (
                    | :Expr expr => Print.expr(expr)
                    | :Let { .var, .value } => (
                        output.write("let ");
                        output.write(var.name);
                        output.write(" = ");
                        Print.expr(value);
                    )
                    | :Assign { .var, .value } => (
                        output.write(var.name);
                        output.write(" = ");
                        Print.expr(value);
                    )
                    | :Scope block => Print.block(block)
                    | :Return expr => (
                        output.write("return ");
                        Print.expr(expr);
                    )
                    | :If { .cond, .then_case, .else_case } => (
                        output.write("if (");
                        Print.expr(cond);
                        output.write(") ");
                        Print.block(then_case);
                        if else_case is :Some else_case then (
                            output.write(" else ");
                            Print.block(else_case);
                        );
                    )
                )
            );

            const block = (block :: Block) => (
                let output = @current Output;
                output.write("{\n");
                output.inc_indentation();
                Print.stmts(block.stmts);
                output.dec_indentation();
                output.write("}");
            );

            const stmts = (stmts :: ArrayList.t[Stmt]) => (
                let output = @current Output;
                for stmt in stmts |> ArrayList.into_iter do (
                    Print.stmt(stmt);
                    output.write(";\n");
                );
            );
        );
    );

    const State = newtype {
        .next_var_id :: Int32,
        .top_level :: Ast.Block,
    };
    const Context = @context State;
    const Scope = @context type (&mut Ast.Block);

    const new_block = () -> Ast.Block => {
        .stmts = ArrayList.new(),
    };

    const insert_stmt = (stmt :: Ast.Stmt) => (
        &mut (@current Scope)^.stmts |> ArrayList.push_back(stmt);
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
                    with Scope = &mut block;
                    assign(result_var, :Var calculate(then_case));
                    block
                );
                let else_case = match else_case with (
                    | :Some else_case => :Some (
                        let mut block = new_block();
                        with Scope = &mut block;
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
        for arg in fn.args |> ArrayList.into_iter do (
            let arg = var(arg.name);
            &mut args |> ArrayList.push_back(arg);
        );
        let mut body = new_block();
        with Scope = &mut body;
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
        let output = @current Output;
        Ast.Print.stmts(compiled.top_level.stmts);
    );

    const compile = (program :: Ir.Program) -> Compiled => (
        let mut state :: State = {
            .next_var_id = 0,
            .top_level = new_block(),
        };
        with Context = state;
        with Scope = &mut state.top_level;
        for { .key = name, .value = def } in program.fns |> OrdMap.into_iter do (
            let_var(var(name), compile_fn(def));
        );
        if &program.fns |> OrdMap.get("main") is :Some _ then (
            insert_stmt(
                :Expr :Apply {
                    .f = :Var var("main"),
                    .args = ArrayList.new(),
                }
            );
        );
        { .top_level = state.top_level }
    );
);

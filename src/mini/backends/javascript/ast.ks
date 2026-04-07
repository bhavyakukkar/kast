use (import "../../../output.ks").*;

module:

const Ast = (
    module:

    const Var = newtype {
        .name :: String,
    };

    const ObjPart = newtype (
        | :Field {
            .name :: String,
            .value :: Expr,
        }
        | :Unpack Expr
    );

    const Expr = newtype (
        | :Raw String
        | :RawConcat {
            .parts :: ArrayList.t[Expr],
        }
        | :Undefined
        | :Null
        | :NumberLiteral Float64
        | :StringLiteral String
        | :Var Var
        | :Obj ArrayList.t[ObjPart]
        | :Field {
            .obj :: Expr,
            .field :: String,
        }
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
            .assignee :: Expr,
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
                | :Field { .obj, .field } => (
                    Print.expr(obj);
                    output.write(".");
                    output.write(field);
                )
                | :Obj parts => (
                    output.write("{\n");
                    output.inc_indentation();
                    for part in parts |> ArrayList.into_iter do (
                        match part with (
                            | :Field { .name, .value } => (
                                output.write(name);
                                output.write(": ");
                                Print.expr(value);
                            )
                            | :Unpack packed => (
                                output.write("...");
                                Print.expr(packed);
                            )
                        );
                        output.write(",");
                    );
                    output.dec_indentation();
                    output.write("}");
                )
                | :Null => output.write("null")
                | :Undefined => output.write("undefined")
                | :NumberLiteral x => output.write(to_string(x))
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
                | :Assign { .assignee, .value } => (
                    Print.expr(assignee);
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

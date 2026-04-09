use (import "../../ir.ks").*;
use std.collections.OrdMap;

module:

const C = (
    module:

    use (import "./ast.ks").*;

    const Compiled = Ast.Program;

    const print = (compiled :: Compiled) => (
        Ast.Print.program(&compiled);
    );

    const compile = (mut program :: Ir.Program) -> Compiled => (
        let mut fns = ArrayList.new();
        let main = {
            .signature = {
                .name = "main",
                .args = ArrayList.new(),
                .result_ty = :Int,
            },
            .body = (
                let mut stmts = ArrayList.new();
                &mut stmts
                    |> ArrayList.push_back(
                        :Expr :Apply {
                            .f = :Ident "printf",
                            .args = (
                                let mut args = ArrayList.new();
                                &mut args |> ArrayList.push_back(:Literal :String "Hello, world!");
                                args
                            ),
                        }
                    );
                &mut stmts |> ArrayList.push_back(:Return :Literal :Int 0);
                { .stmts }
            )
        };
        &mut fns |> ArrayList.push_back(main);
        let mut includes = ArrayList.new();
        &mut includes |> ArrayList.push_back("<stdio.h>");
        {
            .includes,
            .fns,
        }
    );
);

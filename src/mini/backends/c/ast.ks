use (import "../../../output.ks").*;

module:

const Ast = (
    module:

    const Ty = newtype (
        | :SizeT
        | :Char
        | :Int
        | :Void
        | :Pointer Ty
    );

    const FnArg = newtype {
        .name :: String,
        .ty :: Ty,
    };

    const FnSignature = newtype {
        .name :: String,
        .args :: ArrayList.t[FnArg],
        .result_ty :: Ty,
    };

    const Fn = newtype {
        .signature :: FnSignature,
        .body :: Block,
    };

    const Literal = newtype (
        | :Int Int32
        | :Char Char
        | :String String
    );

    const Expr = newtype (
        | :Ident String
        | :Literal Literal
        | :Apply {
            .f :: Expr,
            .args :: ArrayList.t[Expr],
        }
    );

    const Stmt = newtype (
        | :Expr Expr
        | :Return Expr
    );

    const Block = newtype {
        .stmts :: ArrayList.t[Stmt],
    };

    const Program = newtype {
        .includes :: ArrayList.t[String],
        .fns :: ArrayList.t[Fn],
    };

    const Print = (
        module:

        const write = (s :: String) => (
            (@current Output).write(s);
        );

        const inc_indentation = () => (@current Output).inc_indentation();
        const dec_indentation = () => (@current Output).dec_indentation();

        const write_keyword = (s :: String) => (
            ansi.with_mode(
                :Magenta,
                () => (@current Output).write(s),
            );
        );

        const literal = (literal :: &Literal) => (
            match literal^ with (
                | :Int x => ansi.with_mode(
                    :Italic,
                    () => write(to_string(x)),
                )
                | :Char c => ansi.with_mode(
                    :Green,
                    () => (
                        write("'");
                        write(String.escape_contents(to_string(c), .delimiter = "'"));
                        write("'");
                    ),
                )
                | :String s => ansi.with_mode(
                    :Green,
                    () => write(String.escape(s)),
                )
            )
        );

        const expr = (expr :: &Expr) => with_return (
            if expr^ is :Literal ref literal then (
                Print.literal(literal);
                return;
            );
            write("(");
            match expr^ with (
                | :Ident name => write(name)
                | :Apply { .f = ref f, .args = ref args } => (
                    Print.expr(f);
                    write("(");
                    for { i, arg } in args |> ArrayList.iter |> std.iter.enumerate do (
                        if i != 0 then write(", ");
                        Print.expr(arg);
                    );
                    write(")");
                )
            );
            write(")");
        );

        const ty = (ty :: &Ty) => (
            match ty^ with (
                | :Char => write_keyword("char")
                | :SizeT => write_keyword("size_t")
                | :Void => write_keyword("void")
                | :Int => write_keyword("int")
                | :Pointer ref t => (
                    Print.ty(t);
                    write("*")
                )
            )
        );

        const stmt = (stmt :: &Stmt) => (
            match stmt^ with (
                | :Expr ref expr => Print.expr(expr)
                | :Return ref expr => (
                    write_keyword("return ");
                    Print.expr(expr);
                )
            );
        );

        const block = (block :: &Block) => (
            write("{\n");
            inc_indentation();
            for stmt in &block^.stmts |> ArrayList.iter do (
                Print.stmt(stmt);
                write(";\n");
            );
            dec_indentation();
            write("}");
        );

        const fn_signature = (signature :: &FnSignature) => (
            Print.ty(&signature^.result_ty);
            write(" ");
            write(signature^.name);
            write("(");
            for { i, arg } in &signature^.args |> ArrayList.iter |> std.iter.enumerate do (
                if i != 0 then (
                    write(", ");
                );
                Print.ty(&arg^.ty);
                write(" ");
                write(arg^.name);
            );
            write(")");
        );

        const fn = (fn :: &Fn) => (
            Print.fn_signature(&fn^.signature);
            write(" ");
            Print.block(&fn^.body);
            write("\n");
        );

        const program = (program :: &Program) => (
            for &@"include" in &program^.includes |> ArrayList.iter do (
                write_keyword("#include ");
                ansi.with_mode(
                    :Green,
                    () => write(@"include"),
                );
                write("\n");
            );
            write("\n");
            for fn in &program^.fns |> ArrayList.iter do (
                Print.fn_signature(&fn^.signature);
                write(";\n");
            );
            write("\n");
            for fn in &program^.fns |> ArrayList.iter do (
                Print.fn(fn);
            );
        );
    );
);

use (import "../span.ks").*;
use (import "../output.ks").*;
use std.collections.OrdMap;
use std.collections.OrdSet;

module:

const Ir = (
    module:

    const TypeDef = newtype (
        | :Opaque
        | :Enum {
            .variants :: OrdSet.t[String],
        }
        | :Union {
            .variants :: OrdMap.t[String, Type],
        }
        | :Struct {
            .fields :: OrdMap.t[String, Type],
        }
        | :Alias Type
    );

    const FnType = newtype {
        .args :: ArrayList.t[Type],
        .result :: Type,
    };

    const Type = newtype (
        | :Unit
        | :Int32
        | :Bool
        | :String
        | :Named String
        | :Fn FnType
    );

    const NativeExprPart = newtype (
        | :Raw String
        | :Interpolated Expr
    );

    const NativeExpr = newtype {
        .parts :: ArrayList.t[NativeExprPart],
    };

    const ExprShape = newtype (
        | :Unit
        | :Native NativeExpr
        | :StringLiteral String
        | :Ident String
        | :Stmt Expr
        | :Then ArrayList.t[Expr]
        | :Scope Expr
        | :If {
            .cond :: Expr,
            .then_case :: Expr,
            .else_case :: Option.t[Expr],
        }
        | :Apply {
            .f :: Expr,
            .args :: ArrayList.t[Expr],
        }
    );

    const Expr = newtype {
        .shape :: ExprShape,
        .ty :: Type,
        .span :: Span,
    };

    const FnArg = newtype {
        .name :: String,
        .ty :: Type,
    };

    const FnDef = newtype {
        .args :: ArrayList.t[FnArg],
        .result_ty :: Type,
        .body :: Expr,
    };

    const Program = newtype {
        .types :: OrdMap.t[String, TypeDef],
        .fns :: OrdMap.t[String, FnDef],
    };

    const Print = (
        module:

        const fn_type = (self :: &FnType) => (
            let output = @current Output;
            output.write("(");
            for { i, arg_ty } in &self^.args |> ArrayList.iter |> std.iter.enumerate do (
                if i != 0 then (
                    output.write(", ");
                );
                Ir.Print.type_name(arg_ty);
            );
            output.write(") -> ");
            Ir.Print.type_name(&self^.result);
        );

        const type_name = (self :: &Type) => (
            let output = @current Output;
            match self^ with (
                | :Unit => output.write("()")
                | :Int32 => output.write("<Int32>")
                | :Bool => output.write("<Bool>")
                | :String => output.write("<String>")
                | :Named name => output.write(name)
                | :Fn ref ty => Print.fn_type(ty)
            )
        );

        const program = (self :: &Program) => (
            let output = @current Output;
            ansi.with_mode(
                :Bold,
                () => output.write("Types:\n"),
            );
            for &{ .key = name, .value = def } in &self^.types |> OrdMap.iter do (
                output.write("- ");
                output.write(name);
                output.write("\n");
            );
            ansi.with_mode(
                :Bold,
                () => output.write("Functions:\n"),
            );
            for &{ .key = name, .value = def } in &self^.fns |> OrdMap.iter do (
                output.write("- ");
                output.write(name);
                output.write("\n");
            );
        );
    );
);

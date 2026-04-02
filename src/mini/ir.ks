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
            .variants :: OrdMap.t[String, TypeName],
        }
        | :Struct {
            .fields :: OrdMap.t[String, TypeName],
        }
        | :Alias TypeName
    );

    const FnType = newtype {
        .args :: ArrayList.t[TypeName],
        .result :: TypeName,
    };

    const TypeName = newtype (
        | :Named String
        | :Fn FnType
    );

    const ExprShape = newtype (
        | :Then ArrayList.t[Expr]
        | :Apply {
            .f :: Expr,
            .args :: ArrayList.t[Expr],
        }
    );

    const Expr = newtype {
        .shape :: ExprShape,
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

        const type_name = (self :: &TypeName) => (
            let output = @current Output;
            match self^ with (
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

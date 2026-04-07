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
        | :Any
        | :Ref Type
        | :List Type
        | :UnwindToken Type
        | :Unit
        | :Int32
        | :Int64
        | :Float64
        | :Bool
        | :Char
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

    const Literal = newtype (
        | :Bool Bool
        | :Int32 Int32
        | :Int64 Int64
        | :Float64 Float64
        | :Char Char
        | :String String
    );

    const ExprShape = newtype (
        | :Unit
        | :Uninitialized
        | :Claim PlaceExpr
        | :Ref PlaceExpr
        | :Native NativeExpr
        | :Literal Literal
        | :Stmt Expr
        | :Let {
            .name :: String,
            .value :: Expr,
        }
        | :Assign {
            .assignee :: PlaceExpr,
            .value :: Expr,
        }
        | :Fn FnDef
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
        | :InjectContext {
            .name :: String,
            .value :: Expr,
        }
        | :Record ArrayList.t[Field]
    );

    const Field = newtype {
        .name :: String,
        .value :: Expr,
    };

    const Expr = newtype {
        .shape :: ExprShape,
        .ty :: Type,
        .span :: Span,
    };

    const PlaceExprShape = newtype (
        | :Ident String
        | :Field {
            .obj :: PlaceExpr,
            .field :: String,
        }
        | :CurrentContext String
        | :Deref Expr
        | :Temp Expr
    );

    const PlaceExpr = newtype {
        .shape :: PlaceExprShape,
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
        .contexts :: OrdMap.t[String, Type],
        .consts :: OrdMap.t[String, Expr],
        .consts_order :: ArrayList.t[String],
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
                | :Any => output.write("Any")
                | :Unit => output.write("()")
                | :Bool => output.write("Bool")
                | :Int32 => output.write("Int32")
                | :Int64 => output.write("Int64")
                | :Float64 => output.write("Float64")
                | :Char => output.write("Char")
                | :String => output.write("String")
                | :Named name => output.write(name)
                | :Fn ref ty => Print.fn_type(ty)
                | :List ref element_ty => (
                    output.write("List[");
                    Print.type_name(element_ty);
                    output.write("]");
                )
                | :UnwindToken ref result_ty => (
                    output.write("UnwindToken[");
                    Print.type_name(result_ty);
                    output.write("]");
                )
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

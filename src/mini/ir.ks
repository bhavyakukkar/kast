use (import "../span.ks").*;
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
        | :Fn {
            .args :: ArrayList.t[TypeName],
            .result :: TypeName,
        }
    );

    const TypeName = newtype {
        .name :: String,
    };

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
);

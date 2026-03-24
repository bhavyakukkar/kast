module:

const Context = @context type {
    .print :: () -> String,
};

const f = (depth :: Int32) => (
    let inner = (b :: Bool) => (
        if depth < 3 then (
            let parent_context = @current Context;
            with Context = {
                .print = () => (
                    parent_context.print() + (if b then "1" else "0")
                ),
            };
            f(depth + 1);
        ) else (
            std.io.print((@current Context).print());
        );
    );
    inner(true);
    inner(false);
);

with Context = { .print = () => "" };
f(0);

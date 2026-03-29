(
    module:

    const private = (
        module:

        const f = () => (
            print("Hello from private fn");
            public.recurse();
        );
    );

    const public = (
        module:

        const recurse = () => (
            print("Hello from public recurse");
        );

        const f = () => (
            print("Hello from public fn");
            private.f();
        );
    );
).public
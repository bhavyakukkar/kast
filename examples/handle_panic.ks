unwindable block (
    with std.PanicHandler = {
        .handle = [T] s => (
            print(s);
            unwind block ()
        ),
    };
    panic("i panicked");
);
print("continued");
loop (
    let s = std.io.stdin.read_until('\n');
    dbg.print(s);
    let s = std.io.stdin.read_exactly(4);
    dbg.print(s);
);
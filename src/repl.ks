module:

const Repl = (
    module:
    # const t = @opaque_type;
    # const init = () -> Tty.t => (
    #     @native "await import('node:tty')"
    # );
    # const read_byte = () -> Int32 => (
    #     let result = @native "await new Promise(resolve => {const on_data = data => {resolve(data[0]);process.stdin.removeListener('data', on_data);};process.stdin.addListener('data', on_data);})";
    #     dbg.print(result);
    #     result
    # );
    # const read_char = () -> Char => (
    #     let b0 = read_byte();
    #     let mut code = b0;
    #     let mut first_zero_bit = 7;
    #     let mut leading_one_bits = 0;
    #     while std.op.bit_and(b0, std.op.bit_shift_left(1, first_zero_bit)) != 0 do (
    #         first_zero_bit -= 1;
    #         leading_one_bits += 1;
    #         code = std.op.bit_and(code, std.op.bit_not(std.op.bit_shift_left(1, first_zero_bit)));
    #     );
    #     dbg.print(leading_one_bits);
    #     for _ in 0..leading_one_bits - 1 do (
    #         let b = read_byte();
    #         code = std.op.bit_shift_left(code, 6) + std.op.bit_and(b, 0x7f);
    #     );
    #     Char.from_code(code)
    # );
    const read_stdin = () -> String => (
        let result = @native "await new Promise(resolve => {const on_data = data => {resolve(data.toString());process.stdin.removeListener('data', on_data);};process.stdin.addListener('data', on_data);})";
        # dbg.print(result);
        result
    );

    const with_raw_mode = f => (
        @native "process.stdin.setRawMode(true)";
        @native "process.stdin.resume()";
        const exit_raw_mode = () => (
            @native "process.stdin.setRawMode(false)";
            @native "process.stdin.pause()";
        );
        @native "await (async ()=>{try { \(f()) } finally { \(exit_raw_mode()) }})()"
    );

    const special = (
        const UP = "\x1B[A";
        const DOWN = "\x1B[B";
        const RIGHT = "\x1B[C";
        const LEFT = "\x1B[D";
    );

    const Context = @context newtype {
        .highlight :: String -> String,
        .prompt :: String,
        .read_buffer :: String,
        .write_buffer :: String,
        .handle_ctrl_c :: () -> (),
    };

    const read_char = () => (
        let mut ctx = @current Context;
        if ctx.read_buffer == "" then (
            ctx.read_buffer = read_stdin();
        );
        let c = ctx.read_buffer |> String.at(0);
        let i = Char.string_encoding_len(c);
        ctx.read_buffer = ctx.read_buffer
            |> String.substring(i, String.length(ctx.read_buffer) - i);
        c
    );

    const write = (s :: String) => (
        let mut ctx = @current Context;
        ctx.write_buffer += s;
    );

    const flush = () => (
        let mut ctx = @current Context;
        std.io.stdout.write(ctx.write_buffer);
        ctx.write_buffer = "";
    );

    const read_line = (prompt :: String) -> String => (
        let ctx = @current Context;
        write(ctx.prompt);
        write("\x1b[s"); # save cursor
        flush();
        let mut s = "";
        # TODO add string.pop_back instead
        let mut char_offsets = ArrayList.new();
        loop (
            let c = read_char();
            if c == '\x03' then (
                (@current Context).handle_ctrl_c();
            );
            if c == '\r' then (
                # enter
                break;
            );
            if c == '\x7F' then (
                # backspace
                if s != "" then (
                    s = s |> String.substring(0, &mut char_offsets |> ArrayList.pop_back);
                );
            ) else if c == '\x1b' then (
                if read_char() != '[' then panic("unexpected esc");
                let c = read_char();
                dbg.print(c);
            ) else if c == '\f' then (
                write("\x1b[2J"); # clear
                write("\x1b[1;1H"); # move cursor to 1,1
                write(ctx.prompt);
                write("\x1b[s"); # save cursor 
            ) else (
                &mut char_offsets |> ArrayList.push_back(String.length(s));
                s += to_string(c);
            );
            write("\x1b[u"); # reset position
            write("\x1b[J"); # clear everything after cursor
            write(ctx.highlight(s));
            flush();
        );
        write("\n");
        flush();
        s
    );

    const run = (
        .highlight :: String -> String,
        .prompt :: String,
    ) => (
        let main_loop = () => with_return (
            with Context = {
                .handle_ctrl_c = () => (
                    print("Ctrl-C was pressed, exiting...");
                    return;
                ),
                .read_buffer = "",
                .write_buffer = "",
                .highlight,
                .prompt,
            };
            loop (
                let line = read_line(prompt);
                dbg.print(line);
            )
        );
        with_raw_mode(main_loop);
    );
);
# Repl.run(.highlight = s => s, .prompt = "> ");


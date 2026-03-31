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

    const run = (
        .highlight :: String -> String,
        .prompt :: String,
    ) => (
        let stdout = std.io.stdout;
        const Context = @context newtype {
            .handle_ctrl_c :: () -> (),
        };
        let mut buffer = "";
        let read_char = () => (
            if buffer == "" then (
                buffer = read_stdin();
            );
            let c = buffer |> String.at(0);
            let i = Char.string_encoding_len(c);
            buffer = buffer |> String.substring(i, String.length(buffer) - i);
            c
        );
        let read_line = (prompt :: String) -> String => (
            std.io.stdout.write(prompt);
            stdout.write("\x1b[s"); # save cursor
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
                    stdout.write("\x1b[2J"); # clear
                    stdout.write("\x1b[1;1H"); # move cursor to 1,1
                    std.io.stdout.write(prompt);
                    stdout.write("\x1b[s"); # save cursor 
                ) else (
                    &mut char_offsets |> ArrayList.push_back(String.length(s));
                    s += to_string(c);
                );
                stdout.write("\x1b[u"); # reset position
                stdout.write("\x1b[J"); # clear everything after cursor
                stdout.write(highlight(s));
            );
            stdout.write("\n");
            s
        );
        let main_loop = () => with_return (
            with Context = {
                .handle_ctrl_c = () => (
                    print("Ctrl-C was pressed, exiting...");
                    return;
                ),
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


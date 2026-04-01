use (import "./queue.ks").*;

module:

const tty = (
    module:

    const Input = newtype (
        | :Content String
        | :Backspace
        | :Delete
        | :Enter
        | :ArrowLeft
        | :ArrowRight
        | :ArrowUp
        | :ArrowDown
        | :Home
        | :End
        | :ClearScreen
        | :Unknown
    );

    const ContextT = newtype {
        .last_read :: String,
        .read_buffer :: Queue.t[Input],
        .write_buffer :: String,
        .handle_ctrl_c :: () -> (),
        .cursor_position_report :: Option.t[type { Int32, Int32 }],
    };
    const Context = @context ContextT;

    const raw = (
        module:

        const Context = @context newtype {
            .data :: String,
        };

        const peek = () -> Option.t[Char] => (
            let ctx = @current Context;
            if ctx.data == "" then (
                :None
            ) else (
                :Some (ctx.data |> String.at(0))
            )
        );
        const advance = () => (
            let mut ctx = @current Context;
            if peek() is :Some c then (
                let i = Char.string_encoding_len(c);
                ctx.data = ctx.data |> String.substring(i, String.length(ctx.data) - i);
            );
        );

        const Csi = newtype {
            .parameters :: String,
            .intermediate :: String,
            .final :: Char,
        };

        const read_csi = () -> Csi => with_return (
            # reading after "\x1b["
            let mut parameters = "";
            let mut intermediate = "";
            @loop (
                let c = peek() |> Option.unwrap_or_else(() => panic("expected escape thingy"));
                advance();
                let code = Char.code(c);
                if 0x30 <= code and code <= 0x3F then (
                    parameters += to_string(c);
                ) else if 0x20 <= code and code <= 0x2F then (
                    intermediate += to_string(c);
                ) else if 0x40 <= code and code <= 0x7E then (
                    return { .parameters, .intermediate, .final = c };
                ) else (
                    panic("Unexpected char in CSI: " + String.escape(to_string(c)));
                )
            )
        );
    );

    const push_input = (input :: Input) => (
        let mut ctx = @current Context;
        &mut ctx.read_buffer |> Queue.push(input);
    );

    const read_more_stdin_data = () => (
        let mut ctx = @current Context;
        let data = @native "await new Promise(resolve => {const on_data = data => {resolve(data.toString());process.stdin.removeListener('data', on_data);};process.stdin.addListener('data', on_data);})";
        ctx.last_read = data;
        with raw.Context = { .data };
        while raw.peek() is :Some c do (
            if c == '\x03' then (
                raw.advance();
                ctx.handle_ctrl_c();
            ) else if c == '\r' then (
                raw.advance();
                push_input(:Enter);
            ) else if c == '\x7f' then (
                raw.advance();
                push_input(:Backspace);
            ) else if c == '\f' then (
                raw.advance();
                push_input(:ClearScreen);
            ) else if c == '\x1b' then (
                raw.advance();
                if std.repr.structurally_equal(raw.peek(), :Some '[') then (
                    raw.advance();
                    let { .parameters, .intermediate, .final } = raw.read_csi();
                    if final == 'R' then (
                        let { row, column } = parameters |> String.split_once(';');
                        ctx.cursor_position_report = :Some {
                            String.parse(row),
                            String.parse(column),
                        };
                    ) else if final == 'A' then (
                        push_input(:ArrowUp);
                    ) else if final == 'B' then (
                        push_input(:ArrowDown);
                    ) else if final == 'C' then (
                        push_input(:ArrowRight);
                    ) else if final == 'D' then (
                        push_input(:ArrowLeft);
                    ) else if final == 'H' then (
                        push_input(:Home);
                    ) else if final == 'F' then (
                        push_input(:End);
                    ) else if final == '~' then (
                        if parameters == "3" then (
                            push_input(:Delete);
                        ) else (
                            push_input(:Unknown);
                        );
                    ) else (
                        push_input(:Unknown);
                    );
                ) else match raw.peek() with (
                    | :Some c => (
                        push_input(:Unknown);
                    )
                    | :None => (
                        push_input(:Unknown);
                    )
                );
            ) else (
                raw.advance();
                push_input(:Content to_string(c));
            );
        );
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

    const save_cursor_position = () => (
        write("\x1b[s");
    );

    const reset_cursor_position = () => (
        write("\x1b[u");
    );

    const move_cursor_to = (row :: Int32, column :: Int32) => (
        write("\x1b[");
        write(to_string(row));
        write(";");
        write(to_string(column));
        write("H");
    );

    const MoveCursorDirection = newtype (
        | :Up
        | :Down
        | :Right
        | :Left
    );

    const move_cursor = (direction :: MoveCursorDirection, amount :: Int32) => (
        write("\x1b[");
        if amount != 1 then (
            write(to_string(amount));
        );
        let c = match direction with (
            | :Up => "A"
            | :Down => "B"
            | :Right => "C"
            | :Left => "D"
        );
        write(c);
    );

    const clear_screen = () => (
        write("\x1b[2J");
    );

    const clear_after_cursor = () => (
        write("\x1b[J");
    );

    const write_backspace = () => (
        write("\b");
    );

    const read_cursor_position = () -> { Int32, Int32 } => with_return (
        let mut ctx = @current Context;
        write("\x1b[6n");
        flush();
        @loop (
            match ctx.cursor_position_report with (
                | :Some posision => (
                    ctx.cursor_position_report = :None;
                    return posision;
                )
                | :None => (
                    read_more_stdin_data();
                )
            )
        )
    );

    const input = () -> Input => with_return (
        let mut ctx = @current Context;
        @loop (
            match &mut ctx.read_buffer |> Queue.pop with (
                | :Some input => return input
                | :None => read_more_stdin_data()
            )
        )
    );

    const run = (f :: () -> ()) => (
        const enter_raw_mode = () => (
            @native "process.stdin.setRawMode(true)";
            @native "process.stdin.resume()";
        );
        const exit_raw_mode = () => (
            @native "process.stdin.setRawMode(false)";
            @native "process.stdin.pause()";
        );

        let f = () => with_return (
            with Context = {
                .handle_ctrl_c = () => (
                    print("Ctrl-C was pressed, exiting...");
                    return;
                ),
                .last_read = "",
                .read_buffer = Queue.new(),
                .write_buffer = "",
                .cursor_position_report = :None,
            };
            f();
        );
        enter_raw_mode();
        @native "await (async ()=>{try { \(f()) } finally { \(exit_raw_mode()) }})()"
    );
);

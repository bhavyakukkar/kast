use (import "./queue.ks").*;

module:

const tty = (
    module:

    const Input = newtype (
        | :Content String
        | :Backspace
        | :Insert
        | :Delete
        | :Enter
        | :ArrowLeft
        | :ArrowRight
        | :ArrowUp
        | :ArrowDown
        | :Home
        | :End
        | :PageUp
        | :PageDown
        | :ClearScreen
        | :Escape
        | :Modified { .modifiers :: Modifiers, .inner :: Input }
        | :Unknown
    );

    const Modifiers = newtype { Int32 };

    impl Modifiers as module = (
        module:
        const has_shift = ({ self } :: Modifiers) -> Bool => (
            std.op.bit_and(self - 1, 1) != 0
        );
        const has_alt = ({ self } :: Modifiers) -> Bool => (
            std.op.bit_and(self - 1, 2) != 0
        );
        const has_ctrl = ({ self } :: Modifiers) -> Bool => (
            std.op.bit_and(self - 1, 4) != 0
        );
        const has_meta = ({ self } :: Modifiers) -> Bool => (
            std.op.bit_and(self - 1, 8) != 0
        );
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

    const parse_csi = (csi :: raw.Csi) -> Option.t[Input] => with_return (
        let { .parameters, .intermediate, .final } = csi;
        if final == 'R' then (
            let { row, column } = parameters |> String.split_once(';');
            (@current Context).cursor_position_report = :Some {
                String.parse(row),
                String.parse(column),
            };
            return :None;
        );
        let { code, modifiers } = if parameters |> String.contains(";") then (
            let { code, modifiers } = parameters |> String.split_once(';');
            { code, :Some { String.parse(modifiers) } }
        ) else (
            { parameters, :None }
        );
        let input_without_modifiers = if final == 'A' then (
            :ArrowUp
        ) else if final == 'B' then (
            :ArrowDown
        ) else if final == 'C' then (
            :ArrowRight
        ) else if final == 'D' then (
            :ArrowLeft
        ) else if final == 'H' then (
            :Home
        ) else if final == 'F' then (
            :End
        ) else if final == '~' then (
            if code == "1" then (
                :Home
            ) else if code == "2" then (
                :Insert
            ) else if code == "3" then (
                :Delete
            ) else if code == "4" then (
                :End
            ) else if code == "5" then (
                :PageUp
            ) else if code == "6" then (
                :PageDown
            ) else if code == "7" then (
                :Home
            ) else if code == "8" then (
                :End
            ) else (
                return :Some :Unknown
            )
        ) else (
            return :Some :Unknown
        );
        let input = match modifiers with (
            | :None => input_without_modifiers
            | :Some modifiers => :Modified {
                .modifiers,
                .inner = input_without_modifiers,
            }
        );
        :Some input
    );

    const read_more_stdin_data = () => (
        let mut ctx = @current Context;
        let data = @native "await new Promise(resolve => {const on_data = data => {resolve(data.toString());process.stdin.removeListener('data', on_data);};process.stdin.addListener('data', on_data);})";
        ctx.last_read += data;
        while ctx.last_read |> String.length > 40 do (
            let c = ctx.last_read |> String.at(0);
            let i = Char.string_encoding_len(c);
            ctx.last_read = ctx.last_read
                |> String.substring(i, String.length(ctx.last_read) - i);
        );
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
                    if parse_csi(raw.read_csi()) is :Some input then (
                        push_input(input);
                    );
                ) else match raw.peek() with (
                    | :Some c => (
                        raw.advance();
                        let inner = if c == '\x1b' then (
                            :Escape
                        ) else (
                            :Content to_string(c)
                        );
                        let mut modifiers = { 2 };
                        push_input(:Modified { .modifiers, .inner });
                    )
                    | :None => (
                        push_input(:Escape);
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

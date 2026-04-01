use (import "./tty.ks").*;

module:

const Repl = (
    module:

    const Context = @context newtype {
        .highlight :: String -> String,
        .prompt :: String,
    };

    const read_line = (prompt :: String) -> String => (
        let ctx = @current Context;
        tty.write(ctx.prompt);
        tty.flush();
        let mut begin_pos = tty.read_cursor_position();
        let mut s = "";
        # TODO add string.pop_back instead
        let mut char_offsets = ArrayList.new();
        loop (
            match tty.input() with (
                | :Enter => (
                    break;
                )
                | :Backspace => (
                    if s != "" then (
                        s = s |> String.substring(0, &mut char_offsets |> ArrayList.pop_back);
                    );
                )
                | :ClearScreen => (
                    tty.clear_screen();
                    tty.move_cursor(1, 1);
                    tty.write(ctx.prompt);
                    begin_pos = tty.read_cursor_position();
                )
                | :Content content => (
                    for c in String.iter(content) do (
                        &mut char_offsets |> ArrayList.push_back(String.length(s));
                        s += to_string(c);
                    );
                )
                | :Unknown => ()
            );
            tty.move_cursor(...begin_pos);
            tty.clear_after_cursor();
            tty.write(ctx.highlight(s));
            tty.save_cursor_position();
            tty.write("\n");
            tty.flush();
            # dbg.print writes directly to stderr so need to flush first
            dbg.print((@current tty.Context).last_read);
            tty.reset_cursor_position();
            tty.flush();
        );
        tty.clear_after_cursor();
        tty.write("\n");
        tty.flush();
        s
    );

    const run = (
        .highlight :: String -> String,
        .prompt :: String,
    ) => (
        let main_loop = () => with_return (
            with Context = {
                .highlight,
                .prompt,
            };
            loop (
                let line = read_line(prompt);
                dbg.print(line);
            )
        );
        tty.run(main_loop);
    );
);
# Repl.run(.highlight = s => s, .prompt = "> ");


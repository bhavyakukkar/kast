use (import "./tty.ks").*;
use std.collections.OrdMap;

module:

const Readline = (
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
        let mut cursor_pos = begin_pos;
        let mut result = {
            .before_cursor = "",
            .after_cursor = "",
        };
        let mut display_widths = OrdMap.new();
        let display_width = c => (
            (&display_widths |> OrdMap.get(c) |> Option.unwrap)^
        );
        loop (
            match tty.input() with (
                | :Enter => (
                    break;
                )
                | :Backspace => (
                    if result.before_cursor != "" then (
                        let c = &mut result.before_cursor |> String.pop_back;
                        for _ in 0..display_width(c) do (
                            tty.write_backspace();
                        );
                        cursor_pos = tty.read_cursor_position();
                    );
                )
                | :Delete => (
                    if result.after_cursor != "" then (
                        let c = result.after_cursor |> String.at(0);
                        let i = Char.string_encoding_len(c);
                        result.after_cursor = result.after_cursor
                            |> String.substring(i, String.length(result.after_cursor) - i);
                    );
                )
                | :ClearScreen => (
                    tty.clear_screen();
                    tty.move_cursor_to(1, 1);
                    tty.write(ctx.prompt);
                    begin_pos = tty.read_cursor_position();
                    tty.write(result.before_cursor);
                    cursor_pos = tty.read_cursor_position();
                )
                | :Content content => (
                    result.before_cursor += content;
                    tty.save_cursor_position();
                    for c in content |> String.iter do (
                        # we clear them afterwards anyway
                        let start = tty.read_cursor_position();
                        tty.write(to_string(c));
                        let end = tty.read_cursor_position();
                        &mut display_widths |> OrdMap.add(c, end.1 - start.1);
                        tty.reset_cursor_position();
                    );
                    tty.write(content);
                    cursor_pos = tty.read_cursor_position();
                )
                | :ArrowLeft => (
                    if result.before_cursor != "" then (
                        let c = &mut result.before_cursor |> String.pop_back;
                        result.after_cursor = to_string(c) + result.after_cursor;
                        tty.move_cursor(:Left, display_width(c));
                        cursor_pos = tty.read_cursor_position();
                    );
                )
                | :ArrowRight => (
                    if result.after_cursor != "" then (
                        let c = result.after_cursor |> String.at(0);
                        tty.write(to_string(c));
                        cursor_pos = tty.read_cursor_position();
                        let i = Char.string_encoding_len(c);
                        result.after_cursor = result.after_cursor
                            |> String.substring(i, String.length(result.after_cursor) - i);
                        result.before_cursor += to_string(c);
                    );
                )
                | :Home => (
                    result = {
                        .before_cursor = "",
                        .after_cursor = result.before_cursor + result.after_cursor,
                    };
                    cursor_pos = begin_pos;
                )
                | :End => (
                    tty.write(result.after_cursor);
                    cursor_pos = tty.read_cursor_position();
                    result = {
                        .before_cursor = result.before_cursor + result.after_cursor,
                        .after_cursor = ""
                    };
                )
                | _ => ()
            );
            tty.move_cursor_to(...begin_pos);
            tty.clear_after_cursor();
            tty.write(ctx.highlight(result.before_cursor + result.after_cursor));
            tty.write("\n");
            tty.flush();
            # dbg.print writes directly to stderr so need to flush first
            dbg.print((@current tty.Context).last_read);
            tty.move_cursor_to(...cursor_pos);
            tty.flush();
        );
        tty.clear_after_cursor();
        tty.write("\n");
        tty.flush();
        result.before_cursor + result.after_cursor
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


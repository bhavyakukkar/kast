module:

const OutputT = newtype {
    .color :: Bool,
    .write :: String -> (),
    .inc_indentation :: () -> (),
    .dec_indentation :: () -> (),
    .dispose :: () -> (),
};
const Output = @context OutputT;
const Stdout = @context OutputT;
const Stderr = @context OutputT;

const new_std_output = (write_line :: String -> ()) -> OutputT => (
    new_output(.write_line, .indentation_string = "│   ", .color = true)
);

const new_output = (
    .write_line :: String -> (),
    .indentation_string :: String,
    .color :: Bool,
) -> OutputT => (
    let mut buffer = "";
    let mut indentation = 0;
    let write = mut s => (
        loop (
            if s == "" then break;
            let i = s |> String.index_of('\n');
            if buffer |> String.length == 0 and i != 0 then (
                if (@current Output).color then (
                    buffer += "\x1b[" + ansi.Mode.open_code(:Dim) + "m";
                );
                for _ in 0..indentation do (
                    buffer += (indentation_string);
                );
                if (@current Output).color then (
                    buffer += "\x1b[" + ansi.Mode.close_code(:Dim) + "m";
                );
            );
            if i < 0 then (
                buffer += s;
                break;
            );
            let line = buffer + (s |> String.substring(0, i));
            write_line(line);
            let i = i + 1;
            s = s |> String.substring(i, String.length(s) - i);
            buffer = "";
        );
    );
    {
        .color,
        .inc_indentation = () => (
            indentation += 1;
        ),
        .dec_indentation = () => (
            indentation -= 1;
        ),
        .write,
        .dispose = () => (
            if buffer != "" then (
                write("\n");
            );
        ),
    }
);

const ansi = (
    module:

    const Mode = newtype (
        | :Bold
        | :Dim
        | :Italic
        | :Under
        | :Blink
        | :Strike
        | :Black
        | :Red
        | :Green
        | :Yellow
        | :Blue
        | :Magenta
        | :Cyan
        | :White
        | :Gray
        | :BlackBg
        | :RedBg
        | :GreenBg
        | :YellowBg
        | :BlueBg
        | :MagentaBg
        | :CyanBg
        | :WhiteBg
        | :GrayBg
    );

    impl Mode as module = (
        module:

        const open_code = (self :: Mode) -> String => match self with (
            | :Bold => "1"
            | :Dim => "2"
            | :Italic => "3"
            | :Under => "4"
            | :Blink => "5"
            | :Strike => "9"
            | :Black => "30"
            | :Red => "31"
            | :Green => "32"
            | :Yellow => "33"
            | :Blue => "34"
            | :Magenta => "35"
            | :Cyan => "36"
            | :White => "37"
            | :Gray => "90"
            | :BlackBg => "40"
            | :RedBg => "41"
            | :GreenBg => "42"
            | :YellowBg => "43"
            | :BlueBg => "44"
            | :MagentaBg => "45"
            | :CyanBg => "46"
            | :WhiteBg => "47"
            | :GrayBg => "100"
        );

        const close_code = (self :: Mode) -> String => match self with (
            | :Bold => "22"
            | :Dim => "22"
            | :Italic => "23"
            | :Under => "24"
            | :Blink => "25"
            | :Strike => "29"
            | :Black => "39"
            | :Red => "39"
            | :Green => "39"
            | :Yellow => "39"
            | :Blue => "39"
            | :Magenta => "39"
            | :Cyan => "39"
            | :White => "39"
            | :Gray => "39"
            | :BlackBg => "49"
            | :RedBg => "49"
            | :GreenBg => "49"
            | :YellowBg => "49"
            | :BlueBg => "49"
            | :MagentaBg => "49"
            | :CyanBg => "49"
            | :WhiteBg => "49"
            | :GrayBg => "49"
        );
    );

    const write_code = (code :: String) => (
        let output = @current Output;
        if output.color then (
            output.write("\x1b[");
            output.write(code);
            # if multiple code can separate with ";"
            output.write("m");
        );
    );

    const open = (mode :: Mode) => (
        write_code(mode |> Mode.open_code);
    );

    const close = (mode :: Mode) => (
        write_code(mode |> Mode.close_code);
    );

    const with_mode = (mode :: Mode, f :: () -> ()) => (
        open(mode);
        f();
        close(mode);
    );
);

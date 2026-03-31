use (import "./span.ks").*;
use (import "./output.ks").*;
use (import "./position.ks").*;
use (import "./source.ks").*;
use (import "./reader.ks").*;
use (import "./token.ks").*;
use (import "./diagnostic.ks").*;

const is_hex_digit = (c :: Char) -> Bool => (
    ('0' <= c and c <= '9')
    or ('a' <= c and c <= 'f')
    or ('A' <= c and c <= 'F')
);

const next_is = (reader :: &Reader, c :: Char) -> Bool => (
    match reader |> Reader.peek with (
        | :Some peek => c == peek
        | :None => false
    )
);

const next_two_are = (reader :: &Reader, c1 :: Char, c2 :: Char) -> Bool => with_return (
    match reader |> Reader.peek with (
        | :Some c => if c != c1 then return false
        | :None => return false
    );
    match reader |> Reader.peek2 with (
        | :Some c => if c != c2 then return false
        | :None => return false
    );
    true
);

module:

const Lexer = newtype {
    .source :: Source,
    .reader :: Reader,
};

impl Lexer as module = (
    module:

    const new = (source :: Source) -> Lexer => {
        .source,
        .reader = Reader.new(source.contents),
    };

    const ContextT = newtype {
        .lexer :: &mut Lexer,
        .reader :: &mut Reader,
        .current_token_start :: Position,
    };
    const Context = @context ContextT;

    const error_with_span = [T] (span, message) -> T => (
        let diagnostic = {
            .severity = :Error,
            .source = :Lexer,
            .span,
            .message = () => (@current Output).write(message),
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );
    const error_at_current_position = [T] message -> T => (
        let ctx = @current Context;
        let span = Span.single_char(
            .position = ctx.reader^.position,
            .char = Reader.peek(&ctx.reader^),
            .path = ctx.lexer^.source.path,
        );
        error_with_span(span, message)
    );
    const error_token = [T] message -> T => (
        let ctx = @current Context;
        let span = {
            .start = ctx.current_token_start,
            .end = ctx.reader^.position,
            .path = ctx.lexer^.source.path,
        };
        error_with_span(span, message)
    );

    const skip_whitespace = (lexer :: &mut Lexer) => (
        let reader = &mut lexer^.reader;
        while Reader.peek(&reader^) is :Some c do (
            if Char.is_whitespace(c) then (
                Reader.advance(reader);
            ) else (
                break;
            )
        );
        :None
    );

    const ReadFn = type (&mut Lexer -> Option.t[Token.Shape.t]);
    const read_eof :: ReadFn = lexer => (
        let reader = &mut lexer^.reader;
        match Reader.peek(&reader^) with (
            | :Some _ => :None
            | :None => :Some :Eof
        )
    );
    const read_punct :: ReadFn = lexer => with_return (
        let reader = &mut lexer^.reader;
        const is_punct = (c :: Char) -> Bool => (
            if c == '_' or c == '\'' or c == '"' then (
                false
            ) else if Char.is_whitespace(c) then (
                false
            ) else if Char.is_ascii_alphanumeric(c) then (
                false
            ) else (
                true
            )
        );
        const is_single_punct = (c :: Char) -> Bool => (
            "@(){}[]&^$;\\," |> String.index_of(c) >= 0
        );
        let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
        if not is_punct(c) then return :None;
        let start = reader^.position.string_encoding_index;
        Reader.advance(reader);
        if not is_single_punct(c) then (
            reader |> Reader.read_while(c => is_punct(c) and not is_single_punct(c));
        );
        let end = reader^.position.string_encoding_index;
        :Some :Punct {
            .raw = String.substring(reader^.contents, start, end - start),
        }
    );

    const is_ident_start = (c :: Char) -> Bool => (
        c == '_' or Char.is_ascii_alpha(c)
    );

    const read_ident :: ReadFn = lexer => with_return (
        let reader = &mut lexer^.reader;
        let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
        if not is_ident_start(c) then return :None;
        let start = reader^.position.string_encoding_index;
        Reader.advance(reader);
        reader |> Reader.read_while(c => Char.is_ascii_alphanumeric(c) or c == '_');
        let end = reader^.position.string_encoding_index;
        let raw = String.substring(reader^.contents, start, end - start);
        :Some :Ident {
            .raw,
            .string = :None,
            .name = raw,
        }
    );

    const read_string_with_delim = (lexer, delim :: Char) => with_return (
        let reader = &mut lexer^.reader;
        let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
        if c != delim then return :None;
        let open_token = {
            .shape = :Punct { .raw = to_string(delim) },
            .span = Span.single_char(
                .position = reader^.position,
                .char = :Some delim,
                .path = lexer^.source.path,
            ),
        };
        let start = reader^.position;
        let expected = [T] expected -> T => (
            let got = match Reader.peek(&reader^) with (
                | :Some c => to_string(c)
                | :None => "<eof>"
            );
            error_at_current_position("Expected " + expected + ", got " + got)
        );
        Reader.advance(reader);
        let mut parts = ArrayList.new();
        let reset_raw_content_part = () => {
            .start = reader^.position,
        };
        let reset_content_part = () => {
            .start = reader^.position,
            .raw_parts = ArrayList.new(),
            .raw_content_part = reset_raw_content_part(),
            .contents = "",
        };
        let mut content_part = reset_content_part();
        let add_char = (c :: Char) => (
            content_part.contents += to_string(c);
        );

        let finish_raw_content_part = () => {
            let start = content_part.raw_content_part.start.string_encoding_index;
            let end = reader^.position.string_encoding_index;
            if start != end then (
                let part :: Token.RawStringPart = :Content {
                    .raw = String.substring(reader^.contents, start, end - start),
                    .span = {
                        .start = content_part.raw_content_part.start,
                        .end = reader^.position,
                        .path = lexer^.source.path,
                    },
                };
                &mut content_part.raw_parts |> ArrayList.push_back(part);
            );
            content_part.raw_content_part = reset_raw_content_part();
        };

        let finish_content_part = () => with_return (
            finish_raw_content_part();
            if content_part.contents |> String.length == 0 then return;
            &mut parts
                |> ArrayList.push_back(
                    :Content {
                        .raw_parts = content_part.raw_parts,
                        .span = {
                            .start = content_part.start,
                            .end = reader^.position,
                            .path = lexer^.source.path,
                        },
                        .raw = (
                            let start = content_part.start.string_encoding_index;
                            let end = reader^.position.string_encoding_index;
                            String.substring(reader^.contents, start, end - start)
                        ),
                        .contents = content_part.contents,
                    }
                );
            content_part = reset_content_part();
        );
        while Reader.peek(&reader^) is :Some c do (
            const outer_loop_body = @current std.LoopBody;
            if c == '\n' then break;
            if c == delim then break;
            if c == '\\' then (
                finish_raw_content_part();
                let escape_start = reader^.position;
                let c = match Reader.peek2(&reader^) with (
                    | :Some c => c
                    | :None => error_at_current_position("Expected escaped char")
                );
                if c == '(' then (
                    # "hello, \(user.name)"
                    finish_content_part();
                    let open_token = {
                        .shape = :Punct { .raw = "\\(" },
                        .span = {
                            .start = reader^.position,
                            .end = (
                                let mut pos = reader^.position;
                                &mut pos |> Position.advance('\\');
                                &mut pos |> Position.advance('(');
                                pos
                            ),
                            .path = lexer^.source.path,
                        },
                    };
                    Reader.advance(reader);
                    Reader.advance(reader);
                    let span_start = reader^.position;
                    let mut tokens = ArrayList.new();
                    let mut bracket_balance :: Int32 = 0;
                    @loop (
                        skip_whitespace(lexer);
                        let c = match Reader.peek(&reader^) with (
                            | :Some c => c
                            | :None => (
                                let span = {
                                    .start = span_start,
                                    .end = reader^.position,
                                    .path = lexer^.source.path,
                                };
                                error_with_span(span, "Unfinished interpolation")
                            )
                        );
                        if c == ')' and bracket_balance == 0 then (
                            let close_token = {
                                .shape = :Punct { .raw = ")" },
                                .span = Span.single_char(
                                    .position = reader^.position,
                                    .char = :Some ')',
                                    .path = lexer^.source.path,
                                ),
                            };
                            &mut parts
                                |> ArrayList.push_back(
                                    :Interpolated {
                                        .tokens,
                                        .open = open_token,
                                        .close = close_token,
                                        .span = {
                                            .start = span_start,
                                            .end = reader^.position,
                                            .path = lexer^.source.path,
                                        }
                                    }
                                );
                            Reader.advance(reader);
                            content_part = reset_content_part();
                            unwind (@binding outer_loop_body) ();
                        );
                        let token = next(lexer);
                        let token_raw = Token.raw(token);
                        if token_raw == "(" then (
                            bracket_balance += 1;
                        ) else if token_raw == ")" then (
                            bracket_balance -= 1;
                        );
                        &mut tokens |> ArrayList.push_back(token);
                    )
                );
                Reader.advance(reader);
                let c = if c == '\\' then (
                    '\\'
                ) else if c == 'n' then (
                    '\n'
                ) else if c == 'r' then (
                    '\r'
                ) else if c == 'b' then (
                    '\b'
                ) else if c == 't' then (
                    '\t'
                ) else if c == 'f' then (
                    '\f'
                ) else if c == '\'' then (
                    '\''
                ) else if c == '"' then (
                    '"'
                ) else if c == 'x' then (
                    Reader.advance(reader);
                    let { c1, c2 } = unwindable chars (
                        let error_position :: Position = with_return (
                            let mut pos = reader^.position;
                            let c1 = Reader.peek(&reader^)
                                |> Option.unwrap_or_else(
                                    () => return pos
                                );
                            &mut pos |> Position.advance(c1);
                            let c2 = Reader.peek2(&reader^)
                                |> Option.unwrap_or_else(
                                    () => return pos
                                );
                            &mut pos |> Position.advance(c2);
                            if not is_hex_digit(c1) or not is_hex_digit(c2) then (
                                return pos;
                            );
                            unwind chars { c1, c2 }
                        );
                        let span = {
                            .start = escape_start,
                            .end = error_position,
                            .path = lexer^.source.path,
                        };
                        error_with_span(span, "Expected 2 hex digits after '\\x'")
                    );
                    Reader.advance(reader);
                    Reader.advance(reader);
                    let c1 = Char.to_digit_radix(c1, 16);
                    let c2 = Char.to_digit_radix(c2, 16);
                    let code = c1 * 16 + c2;
                    if code > 0x7f then (
                        let span = {
                            .start = escape_start,
                            .end = reader^.position,
                            .path = lexer^.source.path,
                        };
                        error_with_span(span, "Hex escaped char must be in range 0x00 to 0x7f");
                    );
                    add_char(Char.from_code(code));
                    continue
                ) else if c == 'u' then (
                    Reader.advance(reader);
                    let c = match Reader.peek(&reader^) with (
                        | :Some c => c
                        | :None => expected("{")
                    );
                    if c != '{' then (
                        expected("{")
                    );
                    Reader.advance(reader);
                    let hex_code_start = reader^.position;
                    let hex_code = Reader.read_while(reader, is_hex_digit);
                    let hex_code_span :: Span = {
                        .start = hex_code_start,
                        .end = reader^.position,
                        .path = lexer^.source.path,
                    };
                    let c = match Reader.peek(&reader^) with (
                        | :Some c => c
                        | :None => error_at_current_position("Expected }")
                    );
                    if c != '}' then (
                        error_at_current_position("Expected }")
                    );
                    Reader.advance(reader);
                    if hex_code |> String.length == 0 then (
                        error_with_span(hex_code_span, "This escape must have at least 1 hex digit");
                    );
                    if hex_code |> String.length > 6 then (
                        error_with_span(hex_code_span, "This escape must have at most 6 hex digits");
                    );
                    let mut code = 0;
                    for c in String.iter(hex_code) do (
                        code = code * 16 + Char.to_digit_radix(c, 16);
                    );
                    add_char(Char.from_code(code));
                    continue
                ) else (
                    error_at_current_position("Unexpected escape char " + to_string(c))
                );
                add_char(c);
                Reader.advance(reader);
                let escape_raw_part = :Escape {
                    .raw = (
                        let start = escape_start.string_encoding_index;
                        let end = reader^.position.string_encoding_index;
                        String.substring(reader^.contents, start, end - start)
                    ),
                    .span = {
                        .start = escape_start,
                        .end = reader^.position,
                        .path = lexer^.source.path,
                    }
                };
                &mut content_part.raw_parts |> ArrayList.push_back(escape_raw_part);
                content_part.raw_content_part = reset_raw_content_part();
                continue;
            );
            add_char(c);
            Reader.advance(reader);
        );
        finish_content_part();
        if &parts |> ArrayList.length == 0 then (
            &mut parts
                |> ArrayList.push_back(
                    :Content {
                        .span = Span.empty(
                            .position = reader^.position,
                            .path = lexer^.source.path,
                        ),
                        .raw_parts = ArrayList.new(),
                        .raw = "",
                        .contents = "",
                    }
                );
        );
        let close_token = (
            let span_so_far = {
                .start,
                .end = reader^.position,
                .path = lexer^.source.path,
            };
            let c = Reader.peek(&reader^)
                |> Option.unwrap_or_else(
                    () => error_with_span(span_so_far, "Unfinished string")
                );
            if c != delim then error_with_span(span_so_far, "Unfinished string");
            {
                .shape = :Punct { .raw = to_string(delim) },
                .span = Span.single_char(
                    .position = reader^.position,
                    .char = :Some delim,
                    .path = lexer^.source.path,
                ),
            }
        );
        Reader.advance(reader);
        let start = start.string_encoding_index;
        let end = reader^.position.string_encoding_index;
        let raw = String.substring(reader^.contents, start, end - start);
        if &parts |> ArrayList.length == 1 then (
            if (&parts |> ArrayList.at(0))^ is :Content { .raw_parts, .contents, ... } then (
                return :Some :String {
                    .raw,
                    .raw_parts,
                    .contents,
                    .open = open_token,
                    .close = close_token,
                };
            );
        );
        :Some :InterpolatedString {
            .delimiter = to_string(delim),
            .raw,
            .open = open_token,
            .parts,
            .close = close_token,
        }
    );

    const read_string :: ReadFn = lexer => (
        let reader = &mut lexer^.reader;
        read_string_with_delim(lexer, '\'')
            |> Option.or_else(() => read_string_with_delim(lexer, '"'))
    );

    const read_hex_number :: ReadFn = lexer => with_return (
        let reader = &mut lexer^.reader;
        let start = reader^.position.string_encoding_index;
        if not next_two_are(&reader^, '0', 'x') then (
            return :None;
        );
        Reader.advance(reader);
        Reader.advance(reader);
        reader |> Reader.read_while(is_hex_digit);
        let end = reader^.position.string_encoding_index;
        :Some :Number {
            .raw = String.substring(reader^.contents, start, end - start),
        }
    );

    const read_number :: ReadFn = lexer => with_return (
        let reader = &mut lexer^.reader;
        let start = reader^.position.string_encoding_index;
        let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
        if not Char.is_ascii_digit(c) then return :None;
        let seen_dot = match Reader.prev(&reader^) with (
            | :Some c => c == '.'
            | :None => false
        );
        Reader.read_while(reader, Char.is_ascii_digit);
        match Reader.peek(&reader^) with (
            | :Some c => (
                let digit_after_dot = match Reader.peek2(&reader^) with (
                    | :Some c => Char.is_ascii_digit(c)
                    | :None => false
                );
                if not seen_dot and c == '.' and digit_after_dot then (
                    Reader.advance(reader);
                    Reader.read_while(reader, Char.is_ascii_digit);
                )
            )
            | :None => ()
        );
        let end = reader^.position.string_encoding_index;
        :Some :Number {
            .raw = String.substring(reader^.contents, start, end - start),
        }
    );

    const read_line_comment :: ReadFn = lexer => with_return (
        let reader = &mut lexer^.reader;
        if not next_is(&reader^, '#') then (
            return :None;
        );
        let start = reader^.position.string_encoding_index;
        reader |> Reader.read_while(c => c != '\n');
        let end = reader^.position.string_encoding_index;
        :Some :Comment {
            .raw = String.substring(reader^.contents, start, end - start),
            .ty = :Line,
        }
    );

    const read_block_comment :: ReadFn = lexer => with_return (
        let reader = &mut lexer^.reader;
        if not next_two_are(&reader^, '(', '#') then (
            return :None
        );
        let start = reader^.position.string_encoding_index;
        let mut nest_depth :: Int32 = 0;
        loop (
            if next_two_are(&reader^, '(', '#') then (
                Reader.advance(reader);
                Reader.advance(reader);
                nest_depth += 1;
                continue;
            );
            if next_two_are(&reader^, '#', ')') then (
                nest_depth -= 1;
                Reader.advance(reader);
                Reader.advance(reader);
                if nest_depth == 0 then break;
                continue;
            );
            if Reader.peek(&reader^) is :None then (
                error_token("Unclosed block comment");
            );
            Reader.advance(reader);
        );
        let end = reader^.position.string_encoding_index;
        :Some :Comment {
            .raw = String.substring(reader^.contents, start, end - start),
            .ty = :Block,
        }
    );

    const read_raw_keyword :: ReadFn = lexer => with_return (
        let reader = &mut lexer^.reader;
        if not &reader^ |> next_is('@') then (
            return :None;
        );
        let start = reader^.position.string_encoding_index;
        reader |> Reader.advance;
        let _ = read_ident(lexer);
        let end = reader^.position.string_encoding_index;
        :Some :Punct {
            .raw = String.substring(reader^.contents, start, end - start),
        }
    );

    const read_raw_ident :: ReadFn = lexer => with_return (
        let reader = &mut lexer^.reader;
        if not &reader^ |> next_is('@') then (
            return :None;
        );
        let at_token :: Token.t = {
            .shape = :Punct { .raw = "@" },
            .span = Span.single_char(
                .position = reader^.position,
                .char = :Some '@',
                .path = lexer^.source.path,
            ),
        };
        match &reader^ |> Reader.peek2 with (
            | :Some c => (
                if c != '"' then return :None;
            )
            | :None => return :None
        );
        let start = lexer^.reader.position;
        reader |> Reader.advance;
        let string_token = read_string_with_delim(lexer, '"') |> Option.unwrap;
        let end = lexer^.reader.position;
        let span :: Span = {
            .start,
            .end,
            .path = lexer^.source.path,
        };
        let raw = String.substring(
            reader^.contents,
            start.string_encoding_index,
            end.string_encoding_index - start.string_encoding_index,
        );
        let string = match string_token with (
            | :String string => string
            | :InterpolatedString _ => (
                error_with_span(span, "Raw idents can't use interpolated strings")
            )
        );
        :Some :Ident {
            .raw,
            .string = :Some {
                string,
                .at_token,
            },
            .name = string.contents,
        }
    );

    const read_fns :: ArrayList.t[ReadFn] = (
        let mut read_fns = ArrayList.new();
        &mut read_fns |> ArrayList.push_back(read_eof);
        &mut read_fns |> ArrayList.push_back(read_line_comment);
        &mut read_fns |> ArrayList.push_back(read_block_comment);
        &mut read_fns |> ArrayList.push_back(read_raw_ident);
        &mut read_fns |> ArrayList.push_back(read_raw_keyword);
        &mut read_fns |> ArrayList.push_back(read_punct);
        &mut read_fns |> ArrayList.push_back(read_ident);
        &mut read_fns |> ArrayList.push_back(read_string);
        &mut read_fns |> ArrayList.push_back(read_hex_number);
        &mut read_fns |> ArrayList.push_back(read_number);
        read_fns
    );

    const next = (lexer :: &mut Lexer) -> Token.t => (
        skip_whitespace(lexer);
        let reader = &mut lexer^.reader;
        with Context = {
            .lexer,
            .reader,
            .current_token_start = reader^.position,
        };
        let start = lexer^.reader.position;
        let shape = with_return (
            with Diagnostic.UnwindableHandler = {
                .unwind_on_error = [T] () -> T => (
                    let start = start.string_encoding_index;
                    let end = lexer^.reader.position.string_encoding_index;
                    return :Error {
                        .raw = String.substring(reader^.contents, start, end - start),
                    }
                ),
            };
            for &read_fn in ArrayList.iter(&read_fns) do (
                let start = lexer^.reader.position;
                if read_fn(lexer) is :Some shape then (
                    return shape;
                );
            );
            unexpected_char(lexer, "None of the read fns returned Some")
        );
        let end = reader^.position;
        let span = { .start, .end, .path = lexer^.source.path };
        { .shape, .span }
    );

    const unexpected_char = [T] (lexer :: &mut Lexer, message :: String) -> T => (
        let char = Reader.peek(&lexer^.reader);
        let span = Span.single_char(
            .position = lexer^.reader.position,
            .char,
            .path = lexer^.source.path,
        );
        Reader.advance(&mut lexer^.reader);
        error_with_span(span, message)
    );
);

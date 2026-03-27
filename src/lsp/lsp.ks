use (import "../common.ks").*;
use (import "../error.ks").*;
use (import "../log.ks").*;
use (import "../ast.ks").*;
use (import "../lexer.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_ruleset.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../source.ks").*;
use (import "../position.ks").*;
use (import "../span.ks").*;
use (import "../output.ks").*;
use (import "../json.ks").*;
use (import "../../deps/uri/src/lib.ks").*;
const dep_json = import "../../deps/json/lib.ks";
use std.collections.OrdMap;

module:

const BufferedReader = newtype {
    .read_exactly :: Int32 -> String,
    .read_until :: Char -> String,
};

const JsonRpc = (
    module:
    
    const Message = Json.t;
    
    const Io = newtype {
        .input :: BufferedReader,
        .output :: {
            .write :: String -> (),
        },
    };
    
    const stdio = () -> Io => {
        .input = {
            .read_exactly = std.io.stdin.read_exactly,
            .read_until = std.io.stdin.read_until,
        },
        .output = {
            .write = std.io.stdout.write,
        },
    };
    
    const Handler = newtype {
        .on_notification :: Json.t -> (),
        .on_request :: Json.t -> Result.t[Json.t, String],
    };
    
    const Header = newtype {
        .content_length :: Int32,
    };
    
    const read_header = (io :: Io) -> Header => (
        let mut content_length = :None;
        loop (
            let header = (
                let s = io.input.read_until('\n');
                if s |> String.at(String.length(s) - 1) != '\r' then (
                    panic("\\r expected");
                );
                let s = s |> String.substring(0, String.length(s) - 1);
                if s |> String.length == 0 then (
                    break;
                );
                let { name, value } = s |> String.split_once(':');
                if value |> String.at(0) != ' ' then (
                    panic("Expected spance after :");
                );
                let value = value |> String.substring(1, String.length(value) - 1);
                { .name, .value }
            );
            if header.name == "Content-Length" then (
                if content_length is :Some _ then (
                    panic("Content-Length present multiple times???");
                );
                content_length = :Some parse(header.value);
            );
            if header.name == "Content-Type" then (
                # TODO maybe check that encoding is utf-8?
            );
        );
        let content_length = content_length
            |> Option.unwrap_or_else(
                () => panic("Content-Length absent")
            );
        { .content_length }
    );

    const write = (io :: Io, message :: Json.t) => (
        let contents = &message |> Json.to_str; # |> Json.into_dep |> to_string;
        std.io.eprint(contents);
        let content_length :: Int32 = @native "Buffer.byteLength(\(contents), 'utf-8')";
        io.output.write("Content-Length: ");
        io.output.write(to_string(content_length));
        io.output.write("\r\n\r\n");
        io.output.write(contents);
    );
    
    const run = (io :: Io, handler :: Handler) => (
        loop (
            let { .content_length } = read_header(io);
            let content = io.input.read_exactly(content_length);
            let json = Json.parse(content) |> std.Result.unwrap;
            let :Object message = json;
            let &(:String jsonrpc) = &message |> OrdMap.get("jsonrpc") |> Option.unwrap;
            if jsonrpc != "2.0" then (
                panic("jsonrpc is not 2.0 but " + String.escape(jsonrpc));
            );
            if &message |> OrdMap.get("id") is :Some &id then (
                let result = handler.on_request(json);
                let mut response_fields = OrdMap.new();
                &mut response_fields |> OrdMap.add("id", id);
                match result with (
                    | :Ok result => (
                        &mut response_fields |> OrdMap.add("result", result)
                    )
                    | :Error error_message => (
                        let mut error_fields = OrdMap.new();
                        const INTERNAL_ERROR = -32603 :: Float64;
                        &mut error_fields |> OrdMap.add("code", :Number INTERNAL_ERROR);
                        &mut error_fields |> OrdMap.add("message", :String error_message);
                        &mut response_fields |> OrdMap.add("error", :Object error_fields);
                    )
                );
                let response :: Json.t = :Object response_fields;
                write(io, response);
            ) else (
                handler.on_notification(json);
            );
        );
    );
);

const Lsp = (
    module:
    
    const CliArgs = (
        module:
        
        const t = newtype {  };
        
        const parse = start_index -> t => (
            let mut i = start_index;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                panic("Unexpected arg " + String.escape(arg));
                i += 1;
            );
            {  }
        );
    );

    const State = newtype {
        .syntax_ruleset :: SyntaxRuleset.t,
        .contents :: OrdMap.t[String, String],
        .parsed_files :: OrdMap.t[String, Ast.t]
    };

    const initialize = (state :: &mut State, request :: Json.t) -> Json.t => (
        Json.parse(std.fs.read_file(std.path.dirname(__FILE__) + "/init.json"))
            |> Result.unwrap
    );

    const open_or_change_doc = (state :: &mut State, uri :: Uri, contents :: String) => (
        let source :: Source = { .uri, .contents };
        let entire_source_span = (
            let start = Position.beginning();
            let mut end = start;
            for c in source.contents |> String.iter do (
                &mut end |> Position.advance(c);
            );
            {
                .start,
                .end,
                .uri = source.uri,
            }
        );
        with Error.HandlerContext = {
            .stop_on_error = false,
            .handle = (span, msg) => (),
        };
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let parsed = Parser.parse(
            .ruleset = state^.syntax_ruleset,
            .entire_source_span,
            .uri = source.uri,
            .token_stream = &mut token_stream,
        );
        &mut state^.parsed_files |> OrdMap.add(Uri.to_string(uri), parsed.ast);
        &mut state^.contents |> OrdMap.add(Uri.to_string(uri), contents);
    );

    const text_document = (
        module:

        const did_open = (state :: &mut State, n :: Json.t) -> () => (
            let :Object fields = n;
            let &(:Object params) = &fields |> OrdMap.get("params") |> Option.unwrap;
            let text_document = (
                let &value = &params |> OrdMap.get("textDocument") |> Option.unwrap;
                let :Object fields = value;
                let &(:String uri) = &fields |> OrdMap.get("uri") |> Option.unwrap;
                let &(:String text) = &fields |> OrdMap.get("text") |> Option.unwrap;
                { .uri = parse(uri), .text }
            );
            state |> open_or_change_doc(text_document.uri, text_document.text)
        );

        const did_change = (state :: &mut State, n :: Json.t) -> () => (
            let :Object fields = n;
            let &(:Object params) = &fields |> OrdMap.get("params") |> Option.unwrap;
            let text_document = (
                let &value = &params |> OrdMap.get("textDocument") |> Option.unwrap;
                let :Object fields = value;
                let &(:String uri) = &fields |> OrdMap.get("uri") |> Option.unwrap;
                { .uri = parse(uri) }
            );
            let &(:Array changes) = &params |> OrdMap.get("contentChanges") |> Option.unwrap;
            let &(:Object change) = &changes |> ArrayList.at(0);
            let &(:String text) = &change |> OrdMap.get("text") |> Option.unwrap;
            state |> open_or_change_doc(text_document.uri, text)
        );

        const selection_range = (state :: &mut State, request :: Json.t) -> Json.t => (
            panic("TODO")
        );

        const hover = (state :: &mut State, request :: Json.t) -> Json.t => (
            let :Object fields = request;
            let &(:Object params) = &fields |> OrdMap.get("params") |> Option.unwrap;
            let text_document = (
                let &value = &params |> OrdMap.get("textDocument") |> Option.unwrap;
                let :Object fields = value;
                let &(:String uri) = &fields |> OrdMap.get("uri") |> Option.unwrap;
                { .uri = parse(uri) }
            );
            let position :: Position = (
                let &value = &params |> OrdMap.get("position") |> Option.unwrap;
                let :Object fields = value;
                let &(:Number line) = &fields |> OrdMap.get("line") |> Option.unwrap;
                let &(:Number column) = &fields |> OrdMap.get("character") |> Option.unwrap;
                let line = std.convert.float64_to_int32(line);
                let column = std.convert.float64_to_int32(column);
                { .index = 0, .line, .column }
            );
            let contents = &state^.contents
                |> OrdMap.get(Uri.to_string(text_document.uri))
                |> Option.as_deref
                |> Option.unwrap_or("");
            let mut c_pos = Position.beginning();
            let mut hovered_char = :None;
            for c in String.iter(contents) do (
                if c_pos.line == position.line and c_pos.column == position.column then (
                    hovered_char = :Some c;
                    break;
                );
                &mut c_pos |> Position.advance(c);
            );
            let hover_text = match hovered_char with (
                | :Some c => "Hovered char: `" + to_string(c) + "`"
                | :None => "Not hovering a char???"
            );
            std.io.eprint(hover_text);
            :Object (
                let mut fields = OrdMap.new();
                &mut fields |> OrdMap.add("contents", :String hover_text);
                fields
            )
        );
    );

    const on_request = (state :: &mut State, request :: Json.t) -> Json.t => with_return (
        Log.info(
            () => (
                let output = @current Output;
                output.write("Received request:\n");
                Json.print(&request);
                output.write("\n");
            )
        );
        let :Object request_fields = request;
        let &(:String method) = &request_fields |> OrdMap.get("method") |> Option.unwrap;
        if method == "initialize" then (
            return state |> initialize(request);
        );
        if method == "textDocument/selectionRange" then (
            return state |> text_document.selection_range(request);
        );
        if method == "textDocument/hover" then (
            return state |> text_document.hover(request);
        );
        panic("TODO respond to " + method)
    );
    const on_notification = (state :: &mut State, notification :: Json.t) -> () => with_return (
        Log.info(
            () => (
                let output = @current Output;
                output.write("Received notification:\n");
                Json.print(&notification);
                output.write("\n");
            )
        );
        let :Object fields = notification;
        let &(:String method) = &fields |> OrdMap.get("method") |> Option.unwrap;
        if method == "textDocument/didOpen" then (
            return state |> text_document.did_open(notification);
        );
        if method == "textDocument/didChange" then (
            return state |> text_document.did_change(notification);
        );
    );
    
    const run = (arg :: CliArgs.t) => (
        (@current Stdout).color = false;
        (@current Stderr).color = false;
        let mut state :: State = {
            .syntax_ruleset = (
                let ruleset_path = "tests/syntax/kast.ks";
                let mut lexer = Lexer.new(Source.read_file(ruleset_path));
                let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
                SyntaxParser.parse_syntax_ruleset(&mut token_stream)
            ),
            .parsed_files = OrdMap.new(),
            .contents = OrdMap.new(),
        };
        JsonRpc.run(
            JsonRpc.stdio(),
            {
                .on_request = request => (
                    :Ok on_request(&mut state, request)
                ),
                .on_notification = notification => (
                    on_notification(&mut state, notification)
                ),
            }
        )
    );
);

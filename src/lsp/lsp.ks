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
use (import "../highlight.ks").*;
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
        let contents = &message |> Json.to_str;
         # |> Json.into_dep |> to_string;
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
    
    const TokenType = newtype (
        | :Type
        | :Class
        | :Enum
        | :Interface
        | :Struct
        | :TypeParameter
        | :Parameter
        | :Variable
        | :Property
        | :EnumMember
        | :Event
        | :Function
        | :Method
        | :Macro
        | :Keyword
        | :Modifier
        | :Comment
        | :String
        | :Number
        | :Regexp
        | :Operator
        | :Decorator
    );
    
    impl TokenType as module = (
        module:
        
        const index = (self :: TokenType) -> Int32 => match self with (
            | :Type => 0
            | :Class => 1
            | :Enum => 2
            | :Interface => 3
            | :Struct => 4
            | :TypeParameter => 5
            | :Parameter => 6
            | :Variable => 7
            | :Property => 8
            | :EnumMember => 9
            | :Event => 10
            | :Function => 11
            | :Method => 12
            | :Macro => 13
            | :Keyword => 14
            | :Modifier => 15
            | :Comment => 16
            | :String => 17
            | :Number => 18
            | :Regexp => 19
            | :Operator => 20
            | :Decorator => 21
        );
        
        const to_string = (self :: TokenType) -> String => match self with (
            | :Type => "type"
            | :Class => "class"
            | :Enum => "enum"
            | :Interface => "interface"
            | :Struct => "struct"
            | :TypeParameter => "typeParameter"
            | :Parameter => "parameter"
            | :Variable => "variable"
            | :Property => "property"
            | :EnumMember => "enumMember"
            | :Event => "event"
            | :Function => "function"
            | :Method => "method"
            | :Macro => "macro"
            | :Keyword => "keyword"
            | :Modifier => "modifier"
            | :Comment => "comment"
            | :String => "string"
            | :Number => "number"
            | :Regexp => "regexp"
            | :Operator => "operator"
            | :Decorator => "decorator"
        );
        
        const variants = () -> std.iter.Iterable[TokenType] => {
            .iter = f => (
                f(:Type);
                f(:Class);
                f(:Enum);
                f(:Interface);
                f(:Struct);
                f(:TypeParameter);
                f(:Parameter);
                f(:Variable);
                f(:Property);
                f(:EnumMember);
                f(:Event);
                f(:Function);
                f(:Method);
                f(:Macro);
                f(:Keyword);
                f(:Modifier);
                f(:Comment);
                f(:String);
                f(:Number);
                f(:Regexp);
                f(:Operator);
                f(:Decorator);
            ),
        };
    );
    
    const TokenModifier = newtype (
        | :Declaration
        | :Definition
        | :Readonly
        | :Static
        | :Deprecated
        | :Abstract
        | :Async
        | :Modification
        | :Documentation
        | :DefaultLibrary
    );
    
    impl TokenModifier as module = (
        module:
        
        const to_string = (self :: TokenModifier) -> String => match self with (
            | :Declaration => "declaration"
            | :Definition => "definition"
            | :Readonly => "readonly"
            | :Static => "static"
            | :Deprecated => "deprecated"
            | :Abstract => "abstract"
            | :Async => "async"
            | :Modification => "modification"
            | :Documentation => "documentation"
            | :DefaultLibrary => "defaultLibrary"
        );
        
        const index = (self :: TokenModifier) -> Int32 => match self with (
            | :Declaration => 0
            | :Definition => 1
            | :Readonly => 2
            | :Static => 3
            | :Deprecated => 4
            | :Abstract => 5
            | :Async => 6
            | :Modification => 7
            | :Documentation => 8
            | :DefaultLibrary => 9
        );
        
        const variants = () -> std.iter.Iterable[TokenModifier] => {
            .iter = f => (
                f(:Declaration);
                f(:Definition);
                f(:Readonly);
                f(:Static);
                f(:Deprecated);
                f(:Abstract);
                f(:Async);
                f(:Modification);
                f(:Documentation);
                f(:DefaultLibrary);
            ),
        };
    );
    
    const initialize = (state :: &mut State, request :: Json.t) -> Json.t => (
        let mut response = Json.parse(std.fs.read_file(std.path.dirname(__FILE__) + "/init.json"))
            |> Result.unwrap;
        let :Object ref mut fields = response;
        let &mut (:Object ref mut capabilities) = fields
            |> OrdMap.get_mut("capabilities")
            |> Option.unwrap;
        let &mut (:Object ref mut semantic_tokens_provider) = capabilities
            |> OrdMap.get_mut("semanticTokensProvider")
            |> Option.unwrap;
        let legend = semantic_tokens_provider
            |> OrdMap.get_mut("legend")
            |> Option.unwrap;
        legend^ = (
            let mut fields = OrdMap.new();
            let mut token_types = ArrayList.new();
            for token_type in TokenType.variants() do (
                &mut token_types
                    |> ArrayList.push_back(
                        :String (token_type |> TokenType.to_string)
                    );
            );
            &mut fields |> OrdMap.add("tokenTypes", :Array token_types);
            let mut token_modifiers = ArrayList.new();
            for token_modifier in TokenModifier.variants() do (
                &mut token_modifiers
                    |> ArrayList.push_back(
                        :String (token_modifier |> TokenModifier.to_string)
                    );
            );
            &mut fields |> OrdMap.add("tokenModifiers", :Array token_modifiers);
            :Object fields
        );
        response
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
                {
                    .string_encoding_index = 0,
                    .line,
                    .column = {
                        .string_encoding = column,
                        .display_width = 0,
                    },
                }
            );
            let contents = &state^.contents
                |> OrdMap.get(Uri.to_string(text_document.uri))
                |> Option.as_deref
                |> Option.unwrap_or("");
            let mut c_pos = Position.beginning();
            let mut hovered_char = :None;
            for c in String.iter(contents) do (
                if (
                    c_pos.line == position.line
                    and c_pos.column.string_encoding == position.column.string_encoding
                ) then (
                    hovered_char = :Some c;
                    break;
                );
                &mut c_pos |> Position.advance(c);
            );
            let hover_text = match hovered_char with (
                | :Some c => (
                    "Hovered char: `"
                    + to_string(c)
                    + "`\n\nposition: "
                    + to_string(position.line + 1)
                    + ":"
                    + to_string(position.column.string_encoding + 1)
                )
                | :None => "Not hovering a char???"
            );
            std.io.eprint(hover_text);
            :Object (
                let mut fields = OrdMap.new();
                &mut fields |> OrdMap.add("contents", :String hover_text);
                fields
            )
        );
        
        const semantic_tokens = (
            module:
            
            const full = (state :: &mut State, request :: Json.t) -> Json.t => with_return (
                let mut data :: ArrayList.t[Int32] = ArrayList.new();
                
                let mut prev_start :: Position = Position.beginning();
                let add_token = (
                    span :: Span,
                    token_type :: TokenType,
                    token_modifiers :: ArrayList.t[TokenModifier],
                ) => (
                    let delta_line = span.start.line - prev_start.line;
                    let delta_start = if delta_line == 0 then (
                        span.start.column.string_encoding - prev_start.column.string_encoding
                    ) else (
                        span.start.column.string_encoding
                    );
                    # TODO index and column use different indexing
                    let length = span.end.string_encoding_index - span.start.string_encoding_index;
                    let token_type = token_type |> TokenType.index;
                    let token_modifiers = (
                        let mut flags = 0;
                        for mod in token_modifiers |> ArrayList.into_iter do (
                            let index = TokenModifier.index(mod);
                            flags = flags
                                |> std.op.bit_or(
                                    std.op.bit_shift_left(1, index)
                                );
                        );
                        flags
                    );
                    &mut data |> ArrayList.push_back(delta_line);
                    &mut data |> ArrayList.push_back(delta_start);
                    &mut data |> ArrayList.push_back(length);
                    &mut data |> ArrayList.push_back(token_type);
                    &mut data |> ArrayList.push_back(token_modifiers);
                    prev_start = span.start;
                );
                
                let :Object fields = request;
                let &(:Object params) = &fields |> OrdMap.get("params") |> Option.unwrap;
                let text_document = (
                    let &value = &params |> OrdMap.get("textDocument") |> Option.unwrap;
                    let :Object fields = value;
                    let &(:String uri) = &fields |> OrdMap.get("uri") |> Option.unwrap;
                    { .uri = parse(uri) }
                );
                
                let ast = match &state^.parsed_files |> OrdMap.get(Uri.to_string(text_document.uri)) with (
                    | :Some ast => ast
                    | :None => return :Null
                );

                Highlight.ast(
                    ast,
                    {
                        .print = (span, token_type, _) => with_return (
                            let token_type = match token_type with (
                                | :Regular => return
                                | :String => :String
                                | :Keyword => :Keyword
                                | :Number => :Number
                                | :Escape => :EnumMember
                            );
                            let token_modifiers = ArrayList.new();
                            add_token(span, token_type, token_modifiers);
                        )
                    },
                );
                
                :Object (
                    let mut fields = OrdMap.new();
                    let mut json_data = ArrayList.new();
                    for x in data |> ArrayList.into_iter do (
                        &mut json_data
                            |> ArrayList.push_back(
                                :Number (std.convert.int32_to_float64(x))
                            );
                    );
                    &mut fields |> OrdMap.add("data", :Array json_data);
                    fields
                )
            );
        );
    );
    
    const on_request = (state :: &mut State, request :: Json.t) -> Json.t => with_return (
        let :Object request_fields = request;
        let &(:String method) = &request_fields |> OrdMap.get("method") |> Option.unwrap;
        Log.info(
            () => (
                let output = @current Output;
                output.write("Received request ");
                output.write(method);
            )
        );
        if method == "initialize" then (
            return state |> initialize(request);
        );
        if method == "textDocument/selectionRange" then (
            return state |> text_document.selection_range(request);
        );
        if method == "textDocument/hover" then (
            return state |> text_document.hover(request);
        );
        if method == "textDocument/semanticTokens/full" then (
            return state |> text_document.semantic_tokens.full(request);
        );
        panic("TODO respond to " + method)
    );
    const on_notification = (state :: &mut State, notification :: Json.t) -> () => with_return (
        let :Object fields = notification;
        let &(:String method) = &fields |> OrdMap.get("method") |> Option.unwrap;
        Log.info(
            () => (
                let output = @current Output;
                output.write("Received notification ");
                output.write(method);
            )
        );
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

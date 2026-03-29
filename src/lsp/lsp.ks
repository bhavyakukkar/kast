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
use (import "../source_path.ks").*;
use (import "../position.ks").*;
use (import "../span.ks").*;
use (import "../output.ks").*;
use (import "../json.ks").*;
use (import "../highlight.ks").*;
use (import "../json_rpc.ks").*;
use (import "../../deps/uri/src/lib.ks").*;
use std.collections.OrdMap;

module:

const Lsp = (
    module:

    include "./state.ks";
    include "./init.ks";
    include "./hover.ks";
    include "./selection_range.ks";
    include "./semantic_tokens.ks";
    
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
            return state |> selection_range(request);
        );
        if method == "textDocument/hover" then (
            return state |> hover(request);
        );
        if method == "textDocument/semanticTokens/full" then (
            return state |> semantic_tokens.full(request);
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
            return state |>  did_open(notification);
        );
        if method == "textDocument/didChange" then (
            return state |> did_change(notification);
        );
    );
    
    const run = (arg :: CliArgs.t) => (
        (@current Stdout).color = false;
        (@current Stderr).color = false;
        let mut state :: State = {
            .syntax_ruleset = (
                let ruleset_path = "tests/syntax/kast.ks";
                let mut lexer = Lexer.new(Source.read(SourcePath.file(ruleset_path)));
                let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
                SyntaxParser.parse_syntax_ruleset(&mut token_stream)
            ),
            .files = OrdMap.new(),
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

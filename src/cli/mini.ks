use (import "./common.ks").*;
use (import "../output.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../lexer.ks").*;
use (import "../token_stream.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../parser.ks").*;
use (import "../mini/_lib.ks").*;

const root_scope = @current_scope;

module:

const Mini = (
    module:

    const Args = (
        module:

        const t = newtype {
            .paths :: ArrayList.t[String],
        };

        const parse = start_index -> t => (
            let mut paths = ArrayList.new();
            let mut i = start_index;
            let mut ruleset_path = :None;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                &mut paths |> ArrayList.push_back(arg);
                i += 1;
            );
            {
                .paths,
            }
        );
    );

    const run = (common_args :: Common.Args.t, args :: Args.t) => (
        let process = (path :: SourcePath) => (
        );
        if &args.paths |> ArrayList.length == 0 then (
            panic("Expected at least 1 path");
        );
        let mut compiler = root_scope.Mini.Compiler.init();
        for path in args.paths |> ArrayList.into_iter do (
            let source = Source.read(SourcePath.file(path));
            &mut compiler |> root_scope.Mini.Compiler.add_source(source);
        );
        let program = compiler |> root_scope.Mini.Compiler.compile;
        let compiled = root_scope.Mini.Backends.JavaScript.compile(program);
        root_scope.Mini.Backends.JavaScript.print(compiled);
    );
);

use (import "../ast.ks").*;
use (import "./common.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../output.ks").*;
use (import "./parse.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../syntax_rule.ks").*;
use (import "../token_stream.ks").*;
use (import "../output.ks").*;

# Warning: rule core:unpack_ignore contains a `seq` or `choice` rule with a single element. This is unnecessary.
# Warning: rule break contains a `seq` or `choice` rule with a single element. This is unnecessary.
# Warning: rule continue contains a `seq` or `choice` rule with a single element. This is unnecessary.
# Warning: rule return_without_value contains a `seq` or `choice` rule with a single element. This is unnecessary.
# Warning: rule core:true contains a `seq` or `choice` rule with a single element. This is unnecessary.
# Warning: rule core:false contains a `seq` or `choice` rule with a single element. This is unnecessary.
# Warning: rule opaque_type contains a `seq` or `choice` rule with a single element. This is unnecessary.
# Warning: rule core:placeholder contains a `seq` or `choice` rule with a single element. This is unnecessary.
# Warning: rule core:__FILE__ contains a `seq` or `choice` rule with a single element. This is unnecessary.
# Warning: rule core:current_compiler_scope contains a `seq` or `choice` rule with a single element. This is unnecessary.
# Error when generating parser
# 
# Caused by:
#     Unresolved conflict for symbol sequence:
#     
#       '...'  •  '|'  …
#     
#     Possible interpretations:
#     
#       1:  (core:unpack  '...'  •  expr)           (precedence: 7)
#       2:  (core:unpack_ignore  '...')  •  '|'  …  (precedence: 7)
#     
#     Possible resolutions:
#     
#       1:  Specify a higher precedence in `core:unpack` than in the other rules.
#       2:  Specify a higher precedence in `core:unpack_ignore` than in the other rules.
#       3:  Specify a left or right associativity in `core:unpack_ignore`
#       4:  Add a conflict for these rules: `core:unpack`, `core:unpack_ignore`

use std.collections.OrdMap;

# https://github.com/tree-sitter/tree-sitter/blob/v0.27.0/crates/generate/src/parse_grammar.rs#L90
const singleton = [T] (item :: T) -> ArrayList.t[T] => (
    let mut arr = ArrayList.new();
    &mut arr |> ArrayList.push_back(item);
    arr
);

const pair = [T] (a :: T, b :: T) -> ArrayList.t[T] => (
    let mut arr = ArrayList.new();
    &mut arr |> ArrayList.push_back(a);
    &mut arr |> ArrayList.push_back(b);
    arr
);

module:

const RuleBuilder = (
    module:
    const { .Ast = JsAst, ... } = import "../mini/backends/javascript/ast.ks";

    const t = JsAst.Expr;

    const expr :: t = :Field {
        .obj = :Var { .name = "$" },
        .field = "expr",
    };

    const str = (s :: String) -> t => :StringLiteral s;

    const seq = (mut items :: ArrayList.t[t]) -> t => (
        if ArrayList.length(&items) == 1 then (
            &mut items |> ArrayList.pop_back
        ) else (
            :Apply {
                .f = :Var { .name = "seq" },
                .args = items,
            }
        )
    );

    const choice = (mut items :: ArrayList.t[t]) -> t => (
        if ArrayList.length(&items) == 1 then (
            &mut items |> ArrayList.pop_back
        ) else (
            :Apply {
                .f = :Var { .name = "choice" },
                .args = items,
            }
        )
    );

    const field = (name :: String, rule :: t) -> t => :Apply {
        .f = :Var { .name = "field" },
        .args = (
            let mut a = ArrayList.new();
            &mut a |> ArrayList.push_back(:StringLiteral name);
            &mut a |> ArrayList.push_back(rule);
            a
        )
    };

    const optional = (rule :: t) -> t => :Apply {
        .f = :Var { .name = "optional" },
        .args = (
            let mut a = ArrayList.new();
            &mut a |> ArrayList.push_back(rule);
            a
        ),
    };

    const prec = (rule :: t, precision :: Int32) => :Apply {
        .f = :Var { .name = "prec" },
        .args = pair(:Raw String.to_string(precision), rule),
    };

    const to_string = (self :: t) -> String => output_to_string(() => (
        JsAst.Print.expr(self);
    ));
);

const Grammar = (
    module:
    const { .Ast = JsAst, ... } = import "../mini/backends/javascript/ast.ks";

    const t = newtype {
        .name :: String,
        .rules :: ArrayList.t[type {
            .name :: String,
            .rule_js :: String,
        }],
        .rule_names :: ArrayList.t[String],
    };

    const new = (name :: String) -> t => {
        .name,
        .rules = ArrayList.new(),
        .rule_names = ArrayList.new(),
    };

    const add_rule = (self :: &mut t, name :: String, rule :: RuleBuilder.t) => (
        let rule_js = RuleBuilder.to_string(rule);
        &mut self^.rules |> ArrayList.push_back({.name, .rule_js});
    );

    const print = (self :: t) => (
        let expr_rule :: JsAst.ObjPart = :Field {
            .name = "expr",
            .value = :Fn {
                .args = singleton({ .name = "$" }),
                .body = {
                    .stmts = singleton(:Return (
                        self.rule_names
                            |> ArrayList.into_iter
                            |> std.iter.map(name => :Index {
                                .obj = :Var { .name = "$" },
                                .index = :StringLiteral name,
                            })
                            |> ArrayList.from_iter
                            |> RuleBuilder.choice
                    )),
                },
            },
        };

        let mut rules = singleton(expr_rule);
        ArrayList.into_iter(self.rules).iter({.name, .rule_js} => (
            &mut rules |> ArrayList.push_back(:Field {
                .name,
                .value = :Fn {
                    .args = singleton({.name = "$"}),
                    .body = {.stmts = singleton(:Return :Raw rule_js)},
                },
            });
        ));

        let grammar_file :: JsAst.Stmt = :Assign {
            .assignee = :Field {
                .obj = :Var { .name = "module" },
                .field = "exports"
            },
            .value = :Apply {
                .f = :Var { .name = "grammar" },
                .args = singleton(:Obj pair(
                    :Field {
                        .name = "name",
                        .value = :StringLiteral (self.name),
                    },
                    :Field {
                        .name = "rules",
                        .value = :Obj rules,
                    },
                )),
            },
        };

        JsAst.Print.stmt(grammar_file);
    );
);

const GenerateTreesitterGrammar = (
    module:

    const kast_syntax_path = "kast:///std/syntax.ks";
    const minikast_syntax_path = "kast:///mini/syntax.ks";
    const json_syntax_path = "kast:///json/syntax.ks";

    const Args = (
        module:

        const t = newtype {
            ## ruleset paths
            .paths :: ArrayList.t[String],
        };

        const usage :: String = "kast ts-grammar ";

        const parse = (
            start_index :: Int32,
        ) -> t => (
            let mut paths = ArrayList.new();
            let mut i = start_index;
            while i < std.sys.argc() do (
                let arg = std.sys.argv_at(i);
                let path = (
                    if arg == "--kast" then (
                        kast_syntax_path
                    ) else if arg == "--minikast" then (
                        minikast_syntax_path
                    ) else if arg == "--json" then (
                        json_syntax_path
                    # TODO: add an optional --name flag for each syntax file that gets used in the generated grammar.js name
                    ) else (
                        Common.path_arg(arg, .ext = :Some "ks")
                    )
                );
                &mut paths |> ArrayList.push_back(path);
                i += 1;
            );
            { .paths }
        );
    );

    const parse_parts = (parts :: &ArrayList.t[SyntaxRule.Part]) -> RuleBuilder.t => (
        let mut root_parts = ArrayList.new();
        let len = parts |> ArrayList.length;
        for { i, part } in parts |> ArrayList.iter |> std.iter.enumerate do (
            let dsl = match part with (
                | &(:Value { .name, .priority_filter }) => (
                    let mut expr = RuleBuilder.expr;
                    match priority_filter with (
                        | :Any => ()
                        | :GreaterOrEqual num => ()
                        | :Greater num => ()
                    );
                    if name is :Some name then (
                        expr = RuleBuilder.field(name, RuleBuilder.expr);
                    );
                    expr
                )

                | &(:Keyword keyword) => RuleBuilder.str(keyword)

                | &(:Group { .quantifier, .name, .parts, .wrap_mode, .span }) => (
                    let mut group = match name with (
                        | :Some name => RuleBuilder.field(name, &parts |> parse_parts)
                        | :None => &parts |> parse_parts
                    );
                    if quantifier is :Optional then (
                        group = RuleBuilder.optional(group);
                    );
                    group
                )

                | &(:Whitespace { .wrap, .no_wrap }) => continue
            );
            &mut root_parts |> ArrayList.push_back(dsl);
        );
        RuleBuilder.seq(root_parts)
    );

    const process = (source :: Source) => (
        # parse syntax
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let rules :: ArrayList.t[SyntaxRule.t]  = SyntaxParser.parse_syntax_rules(&mut token_stream);

        # add all rules to map with priority as key
        let rules :: OrdMap.t[Float64, SyntaxRule.t] = (
            let mut map_on_priorities = OrdMap.new();
            ArrayList.into_iter(rules).iter(rule => (
                &mut map_on_priorities |> OrdMap.add(rule.priority, rule);
            ));
            map_on_priorities
        );

        let mut grammar = Grammar.new(source.path |> to_string);

        # pull all rules out of it and assign incremental i32 as new priority
        let mut next_precision :: Int32 = 0;
        for { .key = _, .value = rule } in rules |> OrdMap.into_iter do (
            let dsl = parse_parts(&rule.parts)
                |> RuleBuilder.prec(next_precision);

            &mut grammar
                |> Grammar.add_rule(String.escape(rule.name), dsl);

            next_precision += 1;
        );

        Grammar.print(grammar);
    );

    const run = (_ :: Common.Args.t, mut args :: Args.t) => with_return (
        if &args.paths |> ArrayList.length == 0 then (
            :Stdin |> Source.read |> process;
            return;
        );

        # TODO: consider printing in JSON
        # for single ruleset, don't print labels so that output can be piped without artifacts
        let print_labels = ArrayList.length(&args.paths) > 1;

        for path in args.paths |> ArrayList.into_iter do (
            if print_labels then ansi.with_mode(
                :Bold,
                () => (@current Output).write(path + "\n\n"),
            );
            path |> SourcePath.parse |> Source.read |> process;
        );
    );
);

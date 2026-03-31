const diagnostics = (
    module:

    const Severity = (
        module:

        const t = Int32;

        # Reports an error.
        const Error :: t = 1;
        # Reports a warning.
        const Warning :: t = 2;
        # Reports an information.
        const Information :: t = 3;
        # Reports a hint.
        const Hint :: t = 4;
    );

    const document = (state :: &mut State, request :: Json.t) -> Json.t => with_return (
        let :Object fields = request;
        let &(:Object params) = &fields |> OrdMap.get("params") |> Option.unwrap;
        let text_document = (
            let &value = &params |> OrdMap.get("textDocument") |> Option.unwrap;
            let :Object fields = value;
            let &(:String uri) = &fields |> OrdMap.get("uri") |> Option.unwrap;
            { .uri = parse(uri) }
        );
        let file_state = &state^.files
            |> OrdMap.get(Uri.to_string(text_document.uri))
            |> Option.unwrap_or_else(
                () => (
                    # Technically null is not allowed response
                    return :Null
                )
            );

        let mut diagnostics = ArrayList.new();
        for diagnostic in &file_state^.diagnostics |> ArrayList.iter do (
            let diagnostic = :Object (
                let mut fields = OrdMap.new();
                let range = span_to_lsp_range(diagnostic^.span);
                &mut fields |> OrdMap.add("range", range);
                let severity = :Number std.convert.int32_to_float64(Severity.Error);
                &mut fields |> OrdMap.add("severity", severity);
                unwindable source (
                    let source = match diagnostic^.kind with (
                        | :Lexer => "lexer"
                        | :Parser => "parser"
                        | :Internal => "internal"
                        | :Other => unwind source ()
                    );
                    &mut fields |> OrdMap.add("source", :String source);
                );
                &mut fields |> OrdMap.add("message", :String diagnostic^.message);
                # Can add tags : unnecessary/deprecated
                # Can add related info
                fields
            );
            &mut diagnostics |> ArrayList.push_back(diagnostic);
        );
        :Object (
            let mut fields = OrdMap.new();
            &mut fields |> OrdMap.add("kind", :String "full");
            &mut fields |> OrdMap.add("items", :Array diagnostics);
            fields
        )
    );

    const workspace = (state :: &mut State, request :: Json.t) -> Json.t => (
        :Object (
            let mut fields = OrdMap.new();
            &mut fields |> OrdMap.add("items", :Array ArrayList.new());
            fields
        )
    );
);
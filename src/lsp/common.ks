const span_to_lsp_range = (span :: Span) -> Json.t => (
    let mut fields = OrdMap.new();
    &mut fields |> OrdMap.add("start", position_to_lsp(span.start));
    &mut fields |> OrdMap.add("end", position_to_lsp(span.end));
    :Object fields
);

const span_to_lsp_location = (span :: Span) -> Json.t => (
    let mut fields = OrdMap.new();
    &mut fields |> OrdMap.add("range", span_to_lsp_range(span));
    let uri = match span.path with (
        | :Uri uri => :String to_string(uri)
        | _ => panic("Was trying to report to lsp this path which is not uri: " + to_string(span.path))
    );
    &mut fields |> OrdMap.add("uri", uri);
    :Object fields
);

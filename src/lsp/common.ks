const span_to_lsp_range = (span :: Span) -> Json.t => (
    let mut fields = OrdMap.new();
    &mut fields |> OrdMap.add("start", position_to_lsp(span.start));
    &mut fields |> OrdMap.add("end", position_to_lsp(span.end));
    :Object fields
);
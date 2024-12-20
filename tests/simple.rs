use kast::*;

fn test_eq(source: &str, expected_value: Value) {
    let source = SourceFile {
        contents: source.to_owned(),
        filename: "<test source>".into(),
    };
    let mut kast = Kast::new();
    let value = kast
        .eval_source(source, Some(expected_value.ty()))
        .expect("Failed to eval source");
    assert_eq!(value, expected_value);
}

#[test]
fn simple() {
    test_eq("\"hello, world\"", Value::String("hello, world".to_owned()));
    test_eq(
        "const int32 = native \"int32\"; 123 :: int32",
        Value::Int32(123),
    );
    test_eq("\"hello\" |> std.dbg", Value::Unit);
    test_eq("2 + 2", Value::Int32(4));
    test_eq("2 + 2", Value::Int64(4));
    test_eq(
        "use std.*; (:Some 123 :: Option[int32]) is :Some _",
        Value::Bool(true),
    );
    test_eq(
        "use std.*; let foo = (a :: int32, b) => a + b; foo (parse \"123\", parse \"456\")",
        Value::Int32(579),
    );
}

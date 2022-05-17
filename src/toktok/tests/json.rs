use serde_json::Value;
use toktok_macros::make_parser;

make_parser!(grammar = "tests/json.toktok");

#[test]
fn json() {
    let source = r###"
    {
        "hello": "world",
        "x": [true, 12, false, -1, "true"],
        "y": { "zzz": "", "a": [] }
    }
    "###;
    let res = parser::json(source).unwrap();

    let expected = serde_json::from_str::<Value>(source).unwrap();
    assert_eq!(res, expected);
}

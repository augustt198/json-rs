json-rs
=======

JSON parser made in rust.

## Usage

A `JsonValue` can be retreived with `JsonValue::from_string`:

```rust
let src  = "{\"hello\":\"world\"}".to_string();
let json = JsonValue::from_string(src).unwrap();
```

`JsonValue` can be an object, array, boolean, null, string, integer, or float value depending on the input string.

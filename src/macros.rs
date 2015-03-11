// just testing stuff
macro_rules! json {
    (null)  => (JsonValue::JsonNull);
    (false) => (JsonValue::JsonBool(false));
    (true)  => (JsonValue::JsonBool(true));
    (int $e:expr)    => (JsonValue::JsonInt($e));
    (float $e:expr)  => (JsonValue::JsonFloat($e));
    (string $e:expr) => (JsonValue::JsonString($e));

    ( { ($key:expr : $val:expr),* } ) => {
        let mut obj = HashMap::new();
        $(
            obj.insert($key, $val);
        )*

        JsonValue::JsonObject(obj)
    }

    ( [ ($elem:expr),* ] ) => {
        let mut arr = vec!();
        $(
            arr.push($elem);
        )*
        JsonValue::JsonArray(arr);
    }
}

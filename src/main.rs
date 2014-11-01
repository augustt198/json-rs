use std::collections::HashMap;

struct StringReader {
    pos:    uint,
    source: String,
    col:    uint,
    line:   uint
}

impl StringReader {
    fn peak(&mut self) -> Option<char> {
        if self.pos < self.source.len() {
            Some(self.source.as_slice().char_at(self.pos))
        } else {
            None
        }
    }

    fn read(&mut self) -> Option<char> {
        let next = self.peak();
        if next.is_some()  {
            if next.unwrap() == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
            self.pos += 1;
        }
        next
    }
}

fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

static NULL_LITERAL : &'static str = "null";
static TRUE_LITERAL : &'static str = "true";
static FALSE_LITERAL: &'static str = "false";

struct Lexer {
    reader: StringReader
}

impl Lexer {
    fn new(s: String) -> Lexer {
        Lexer {
            reader: StringReader { source: s, pos: 0, col: 0, line: 0 }
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        loop {
            let c = match self.reader.read() {
                Some(chr) => chr,
                None => return None
            };

            if      c == '{' { return Some(LBrace) }
            else if c == '}' { return Some(RBrace) }
            else if c == '[' { return Some(LBracket) }
            else if c == ']' { return Some(RBracket) }
            else if c == ',' { return Some(Comma) }
            else if c == ':' { return Some(Colon) }
            
            else if c == '"' { return self.next_str_token() }

            else if is_alpha(c) { return self.next_ident_token(c) }

            else if is_digit(c) { return self.next_num_token(c) }
        }
    }

    // Next string token
    fn next_str_token(&mut self) -> Option<Token> {
        let mut s = String::new();
        loop {
            let c = self.reader.peak();
            match c {
                Some(chr) => {
                    self.reader.read();
                    if chr == '"' { break }
                    else { s.push(chr) }
                },
                None => { break }
            }
        }
        Some(StringTok(s))
    }

    // Next "identifier" token (JsonBool value or null)
    fn next_ident_token(&mut self, first: char) -> Option<Token> {
        let mut res = String::new();
        res.push(first);
        loop {
            let c = self.reader.peak();
            match c {
                Some(chr) => {
                    if is_alpha(chr) {
                        res.push(chr);
                        self.reader.read();
                    }
                    else { break }
                },
                None => break
            }
        }
        let ident_str = res.as_slice();
        if ident_str == NULL_LITERAL {
            Some(JsonNullTok)
        } else if ident_str == TRUE_LITERAL {
            Some(JsonBoolTok(true))
        } else if ident_str == FALSE_LITERAL {
            Some(JsonBoolTok(false))
        } else {
            fail!("Unexpected identifier: {}", ident_str)
        }
    }

    fn next_num_token(&mut self, first: char) -> Option<Token> {
        let mut res = String::new();
        res.push(first);
        let mut has_period = false;
        loop {
            let c = self.reader.peak();
            match c {
                Some(chr) => {
                    if is_digit(chr) || chr == '.' {
                        res.push(chr);
                        self.reader.read();
                        if chr == '.' { has_period = true }
                    }
                    else { break }
                },
                None => break
            }
        }
        let num_str = res.as_slice();
        if has_period {
            match from_str::<f64>(num_str) {
                Some(n) => Some(DecimalNum(n)),
                None    => fail!("Invalid number format") 
            }
        } else {
            match from_str::<i64>(num_str) {
                Some(n) => Some(IntNum(n)),
                None    => fail!("Invalid number format")
            }
        }
    }
}

#[deriving(Show, Clone)]
enum Token {
    LBrace,             // {
    RBrace,             // }
    LBracket,           // [
    RBracket,           // ]
    StringTok(String),  // "..."
    DecimalNum(f64),    // [0-9]+.[0-9]+
    IntNum(i64),        // [0-9]+
    JsonBoolTok(bool),   // true|false
    JsonNullTok,            // null
    Comma,              // ,
    Colon               // :
}

struct Parser {
    tokens: Vec<Token>,
    pos:    uint
}

impl Parser {
    fn new(lex: &mut Lexer) -> Parser {
        let mut parser = Parser { tokens: vec!(), pos: 0 };
        loop {
            match lex.next_token() {
                Some(tok)   => parser.tokens.push(tok),
                None        => break
            }
        }
        parser
    }

    fn peak(&self) -> Option<Token> {
        if self.pos < self.tokens.len() {
            Some(self.tokens[self.pos].clone())
        } else {
            None
        }
    }

    fn next(&mut self) -> Option<Token> {
        let token = self.peak();
        if token.is_some() {
            self.pos += 1
        }
        token
    }
}

impl Parser {
    fn parse(&mut self) -> JsonValue {
        match self.next() {
            Some(first) => {
                match first {
                    JsonBoolTok(b)  => JsonBool(b),
                    JsonNullTok     => JsonNull,
                    StringTok(s)    => JsonString(s),
                    IntNum(i)       => JsonInt(i),
                    DecimalNum(f)   => JsonFloat(f),
                    LBracket        => self.parse_array(),
                    LBrace          => self.parse_object(),
                    other           => fail!("Unexpected token: {}", other)
                }
            }
            _ => {
                fail!("Unexpected end of file");
            }
        }
        
    }

    fn parse_array(&mut self) -> JsonValue {
        let mut vec: Vec<JsonValue> = vec!();
        loop {
            let mut peak = self.peak();

            if peak.is_some() {
                match peak.unwrap() {
                    RBracket    => { self.next(); break },
                    _           => {
                        vec.push(self.parse());
                        peak = self.peak();
                        
                        if peak.is_some() {
                            match peak.unwrap() {
                                Comma   => { self.next(); }, // consume comma
                                _       => {}                // keep more elements or `]`
                            }
                        }
                    }
                }
            } else {
                fail!("Unexpected end of array");
            }
        }
        JsonArray(vec)
    }

    fn parse_object(&mut self) -> JsonValue {
        let mut map = HashMap::new();
        loop {
            let mut peak = self.peak();

            if peak.is_some() {
                match peak.unwrap() {
                    RBrace => {
                        self.next();
                        break;
                    },
                    StringTok(s) => {
                        self.next();
                        peak = self.peak();
                        if peak.is_some() {
                            match peak.unwrap() {
                                Colon => { self.next(); },
                                _     => { fail!("Expected colon") }
                            }
                        } else { fail!("Unexpected end of object") }

                        map.insert(s, self.parse());

                        peak = self.peak();
                        if peak.is_some() {
                            match peak.unwrap() {
                                Comma  => { self.next(); },
                                RBrace => {
                                    self.next();
                                    break;
                                }
                                _      => { break }
                            }
                        }
                    },
                    _   => fail!("Expected right brace or string")
                }
            }
        }
        JsonObject(JsonMap { fields: map })
    }
}

#[deriving(Show, PartialEq)]
struct JsonMap {
    fields: HashMap<String, JsonValue>
}

#[deriving(Show, PartialEq)]
enum JsonValue {
    JsonObject(JsonMap),
    JsonArray(Vec<JsonValue>),
    // Using `String` as enum name would cause naming collisions
    // with std::string::String
    JsonString(String),
    JsonBool(bool),
    JsonInt(i64),
    JsonFloat(f64),
    JsonNull
}

impl JsonValue {
    fn from_string(s: String) -> JsonValue {
        let mut lexer = Lexer::new(s);
        let mut parser = Parser::new(&mut lexer);
        parser.parse()
    }
}

fn main() {
    let src = "{\"test\": [1, 2, 3.0000], \"lol\":1 }".to_string();
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    println!("{}", parser.parse());
}

// tests

#[test]
fn null_literal() {
    let json_string = "null".to_string();
    let json        = JsonValue::from_string(json_string);
    
    match json {
        JsonNull => {}, // ok
        _ => fail!("Expected JsonNull")
    }
}

#[test]
fn true_literal() {
    let json_string = "true".to_string();
    let json        = JsonValue::from_string(json_string);
    
    match json {
        JsonBool(true) => {}, // ok
        _ => fail!("Expected JsonBool(true)")
    }
}

#[test]
fn false_literal() {
    let json_string = "false".to_string();
    let json        = JsonValue::from_string(json_string);
    
    match json {
        JsonBool(false) => {}, // ok
        _ => fail!("Expected JsonBool(false)")
    }
}

#[test]
fn int_literal() {
    let json_string = "42".to_string();
    let json        = JsonValue::from_string(json_string);
    
    match json {
        JsonInt(42) => {}, // ok
        _ => fail!("Expected JsonInt(42)")
    }
}

#[test]
fn float_literal() {
    let json_string = "42.0".to_string();
    let json        = JsonValue::from_string(json_string);
    
    match json {
        JsonFloat(42f64) => {}, // ok
        _ => fail!("Expected JsonFloat(42)")
    }
}

#[test]
#[allow(unused_variables)]
fn empty_arr() {
    let json_string = "[]".to_string();
    let json        = JsonValue::from_string(json_string);

    let empty_arr: Vec<JsonValue> = vec!();

    match json {
        JsonArray(empty_arr) => {}, // ok
        _ => fail!("Expected JsonArray([])")
    }
}

#[test]
fn arr_all_types() {
    let json_string = "[true, false, null, 0, 0.0, [], {}]".to_string();
    let json        = JsonValue::from_string(json_string);
    
    let map = JsonMap { fields: HashMap::new() };
    let arr = vec![
        JsonBool(true), JsonBool(false), JsonNull, JsonInt(0),
        JsonFloat(0.0), JsonArray(vec!()), JsonObject(map)
    ];

    match json {
        JsonArray(vec) => {
            assert_eq!(vec, arr);
        },
        _ => fail!("Expected JsonArray([...])")
    }
}

#[test]
fn empty_object() {
    let json_string = "{}".to_string();
    let json        = JsonValue::from_string(json_string);

    let empty_map: HashMap<String, JsonValue> = HashMap::new();
    match json {
        JsonObject(map) => {
            assert!(map.fields.is_empty())
        }
        _ => fail!("Expected JsonObject")
    }
}

#[test]
fn object_vals() {
    let json_string = "{\"key\":\"value\"}".to_string();
    let json        = JsonValue::from_string(json_string);

    let mut hash_map = HashMap::new();
    hash_map.insert("key".to_string(), JsonString("value".to_string()));
    let json_map = JsonMap { fields: hash_map };

    match json {
        JsonObject(obj) => {
            assert_eq!(obj, json_map);
        },
        _ => fail!("Expected JsonObject")
    }
}


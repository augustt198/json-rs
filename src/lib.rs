use std::fmt;
use std::collections::HashMap;

// scanning

// TODO add line/column tracking
#[deriving(Show)]
struct JsonError {
    msg: String
}

impl JsonError {
    fn new(msg: String) -> JsonError {
        JsonError { msg: msg }
    }
}

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

    fn next_token(&mut self) -> Result<Token, JsonError> {
        loop {
            let c = match self.reader.read() {
                Some(chr) => chr,
                None => return Ok(self.tok(EOF))
            };

            let val =
            if      c == '{' { self.tok(LBrace) }
            else if c == '}' { self.tok(RBrace) }
            else if c == '[' { self.tok(LBracket) }
            else if c == ']' { self.tok(RBracket) }
            else if c == ',' { self.tok(Comma) }
            else if c == ':' { self.tok(Colon) }
            
            else if c == '"'    { try!(self.next_str_token()) }
            else if is_alpha(c) { try!(self.next_ident_token(c)) }
            else if is_digit(c) { try!(self.next_num_token(c)) }
            
            else { continue };
            return Ok(val)
        }
    }

    // create token from current line and column
    fn tok(&self, ty: TokenType) -> Token {
        Token {
            ty:     ty,
            line:   self.reader.line,
            cols:   (self.reader.col, self.reader.col)
        }
    }

    // Next string token
    fn next_str_token(&mut self) -> Result<Token, JsonError> {
        let mut s = String::new();

        let start_pos   = self.reader.col;
        let mut end_pos = start_pos;
        let line        = self.reader.line;

        loop {
            let c = self.reader.peak();
            match c {
                Some(chr) => {
                    self.reader.read();
                    if chr == '"' { break }
                    else {
                        end_pos = self.reader.col;
                        s.push(chr)
                    }
                },
                None => {
                    return Err(JsonError::new(
                        format!("Unexpected end of file in string literal")
                    ));
                }
            }
        }
        Ok(Token {
            ty:     StringTok(s),
            line:   line,
            cols:   (start_pos, end_pos)
        })
    }

    // Next "identifier" token (JsonBool value or null)
    fn next_ident_token(&mut self, first: char) -> Result<Token, JsonError> {
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
            Ok(self.tok(JsonNullTok))
        } else if ident_str == TRUE_LITERAL {
            Ok(self.tok(JsonBoolTok(true)))
        } else if ident_str == FALSE_LITERAL {
            Ok(self.tok(JsonBoolTok(false)))
        } else {
            Err(JsonError::new(format!("Unexpected identifier: {}", ident_str)))
        }
    }

    fn next_num_token(&mut self, first: char) -> Result<Token, JsonError> {
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
                Some(n) => Ok(self.tok(DecimalNum(n))),
                None    => Err(JsonError::new(format!("Invalid number format: {}", num_str)))
            }
        } else {
            match from_str::<i64>(num_str) {
                Some(n) => Ok(self.tok(IntNum(n))),
                None    => Err(JsonError::new(format!("Invalid number format: {}", num_str)))
            }
        }
    }
}

#[deriving(Show, Clone)]
struct Token {
    ty:     TokenType,
    line:   uint,
    cols:   (uint, uint)
}

#[deriving(Show, Clone)]
enum TokenType {
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
    Colon,              // :

    EOF
}

// parsing

struct Parser {
    tokens: Vec<Token>,
    pos:    uint
}

impl Parser {
    fn new(lex: &mut Lexer) -> Result<Parser, JsonError> {
        let mut parser = Parser { tokens: vec!(), pos: 0 };
        loop {
            let token = try!(lex.next_token());
            let done_reading = match token.ty {
                EOF => true,
                _   => false
            };
            parser.tokens.push(token);
            if done_reading { break }
        }
        Ok(parser)
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
    fn parse(&mut self) -> Result<JsonValue, JsonError> {
        match self.next() {
            Some(first) => {
                match first.ty {
                    JsonBoolTok(b)  => Ok(JsonBool(b)),
                    JsonNullTok     => Ok(JsonNull),
                    StringTok(s)    => Ok(JsonString(s)),
                    IntNum(i)       => Ok(JsonInt(i)),
                    DecimalNum(f)   => Ok(JsonFloat(f)),
                    LBracket        => self.parse_array(),
                    LBrace          => self.parse_object(),
                    o               => Err(JsonError::new(format!("Unexpected token: {}", o)))
                }
            }
            None => {
                Err(JsonError::new(format!("Unexpected end of file")))
            }
        }
        
    }

    fn parse_array(&mut self) -> Result<JsonValue, JsonError> {
        let mut vec: Vec<JsonValue> = vec!();
        loop {
            let mut peak = self.peak();

            if peak.is_some() {
                match peak.unwrap().ty {
                    RBracket    => { self.next(); break },
                    _           => {
                        vec.push(try!(self.parse()));
                        peak = self.peak();
                        
                        if peak.is_some() {
                            match peak.unwrap().ty {
                                Comma   => { self.next(); }, // consume comma
                                _       => {}                // keep more elements or `]`
                            }
                        }
                    }
                }
            } else {
                return Err(JsonError::new(
                    format!("Unexpected end of array")
                ))
            }
        }
        Ok(JsonArray(vec))
    }

    fn parse_object(&mut self) -> Result<JsonValue, JsonError> {
        let mut map = HashMap::new();
        loop {
            let mut peak = self.peak();

            if peak.is_some() {
                match peak.unwrap().ty {
                    RBrace => {
                        self.next();
                        break;
                    },
                    StringTok(s) => {
                        self.next();
                        peak = self.peak();
                        if peak.is_some() {
                            match peak.unwrap().ty {
                                Colon => { self.next(); },
                                _     => {
                                    return Err(JsonError::new(format!("Expected colon")))
                                }
                            }
                        } else { panic!("Unexpected end of object") }

                        map.insert(s, try!(self.parse()));

                        peak = self.peak();
                        if peak.is_some() {
                            match peak.unwrap().ty {
                                Comma  => { self.next(); },
                                RBrace => {
                                    self.next();
                                    break;
                                }
                                _      => { break }
                            }
                        }
                    },
                    _   => return Err(JsonError::new(
                        format!("Expected right brace or string")
                    ))
                }
            }
        }
        Ok(JsonObject(map))
    }
}

type JsonMap = HashMap<String, JsonValue>;

#[deriving(PartialEq)]
enum JsonValue {
    JsonObject(JsonMap),
    JsonArray(Vec<JsonValue>),
    JsonString(String),
    JsonBool(bool),
    JsonInt(i64),
    JsonFloat(f64),
    JsonNull
}

impl fmt::Show for JsonValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        match self {
            &JsonObject(ref map)    => map.fmt(f),
            &JsonArray(ref arr)     => arr.fmt(f),
            &JsonString(ref s)      => s.fmt(f),
            &JsonBool(ref b)        => b.fmt(f),
            &JsonInt(ref i)         => i.fmt(f),
            &JsonFloat(ref fl)      => fl.fmt(f),
            &JsonNull               => NULL_LITERAL.fmt(f)
        }
    }
}

impl JsonValue {
    fn from_string(s: String) -> Result<JsonValue, JsonError> {
        let mut lexer = Lexer::new(s);
        let mut parser = try!(Parser::new(&mut lexer));
        parser.parse()
    }
}

// tests

#[test]
fn null_literal() {
    let json_string = "null".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();
    
    match json {
        JsonNull => {}, // ok
        _ => panic!("Expected JsonNull")
    }
}

#[test]
fn true_literal() {
    let json_string = "true".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();
    
    match json {
        JsonBool(true) => {}, // ok
        _ => panic!("Expected JsonBool(true)")
    }
}

#[test]
fn false_literal() {
    let json_string = "false".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();
    
    match json {
        JsonBool(false) => {}, // ok
        _ => panic!("Expected JsonBool(false)")
    }
}

#[test]
fn int_literal() {
    let json_string = "42".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();
    
    match json {
        JsonInt(42) => {}, // ok
        _ => panic!("Expected JsonInt(42)")
    }
}

#[test]
fn float_literal() {
    let json_string = "42.0".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();
    
    match json {
        JsonFloat(42f64) => {}, // ok
        _ => panic!("Expected JsonFloat(42)")
    }
}

#[test]
#[allow(unused_variables)]
fn empty_arr() {
    let json_string = "[]".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();

    let empty_arr: Vec<JsonValue> = vec!();

    match json {
        JsonArray(empty_arr) => {}, // ok
        _ => panic!("Expected JsonArray([])")
    }
}

#[test]
fn arr_all_types() {
    let json_string = "[true, false, null, 0, 0.0, [], {}]".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();
    
    let map = HashMap::new();
    let arr = vec![
        JsonBool(true), JsonBool(false), JsonNull, JsonInt(0),
        JsonFloat(0.0), JsonArray(vec!()), JsonObject(map)
    ];

    match json {
        JsonArray(vec) => {
            assert_eq!(vec, arr);
        },
        _ => panic!("Expected JsonArray([...])")
    }
}

#[test]
fn empty_object() {
    let json_string = "{}".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();

    match json {
        JsonObject(map) => {
            assert!(map.is_empty())
        }
        _ => panic!("Expected JsonObject")
    }
}

#[test]
fn object_vals() {
    let json_string = "{\"key\":\"value\"}".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();

    let mut hash_map = HashMap::new();
    hash_map.insert("key".to_string(), JsonString("value".to_string()));

    match json {
        JsonObject(map) => {
            assert_eq!(map, hash_map);
        },
        _ => panic!("Expected JsonObject")
    }
}


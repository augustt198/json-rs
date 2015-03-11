use std::fmt;
use std::collections::HashMap;
use std::str::FromStr;
use std::fmt::Debug;

// scanning

// TODO add line/column tracking
#[derive(Debug)]
struct JsonError {
    msg: String
}

impl JsonError {
    fn new(msg: String) -> JsonError {
        JsonError { msg: msg }
    }
}

struct StringReader {
    pos:    usize,
    source: String,
    col:    usize,
    line:   usize
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
                None => return Ok(self.tok(TokenType::EOF))
            };

            let val =
            if      c == '{' { self.tok(TokenType::LBrace) }
            else if c == '}' { self.tok(TokenType::RBrace) }
            else if c == '[' { self.tok(TokenType::LBracket) }
            else if c == ']' { self.tok(TokenType::RBracket) }
            else if c == ',' { self.tok(TokenType::Comma) }
            else if c == ':' { self.tok(TokenType::Colon) }
            
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
            ty:     TokenType::StringTok(s),
            line:   line,
            cols:   (start_pos, end_pos)
        })
    }

    // Next "identifier" token (JsonValue::JsonBool value or null)
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
            Ok(self.tok(TokenType::JsonNullTok))
        } else if ident_str == TRUE_LITERAL {
            Ok(self.tok(TokenType::JsonBoolTok(true)))
        } else if ident_str == FALSE_LITERAL {
            Ok(self.tok(TokenType::JsonBoolTok(false)))
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
            match FromStr::from_str(num_str) {
                Ok(n)  => Ok(self.tok(TokenType::DecimalNum(n))),
                Err(e) => Err(JsonError::new(format!("Invalid number format: {}", num_str)))
            }
        } else {
            match FromStr::from_str(num_str) {
                Ok(n)  => Ok(self.tok(TokenType::IntNum(n))),
                Err(e) => Err(JsonError::new(format!("Invalid number format: {}", num_str)))
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Token {
    ty:     TokenType,
    line:   usize,
    cols:   (usize, usize)
}

#[derive(Debug, Clone)]
enum TokenType {
    LBrace,             // {
    RBrace,             // }
    LBracket,           // [
    RBracket,           // ]
    StringTok(String),  // "..."
    DecimalNum(f64),    // [0-9]+.[0-9]+
    IntNum(i64),        // [0-9]+
    JsonBoolTok(bool),  // true|false
    JsonNullTok,        // null
    Comma,              // ,
    Colon,              // :

    EOF
}

// parsing

struct Parser {
    tokens: Vec<Token>,
    pos:    usize
}

impl Parser {
    fn new(lex: &mut Lexer) -> Result<Parser, JsonError> {
        let mut parser = Parser { tokens: vec!(), pos: 0 };
        loop {
            let token = try!(lex.next_token());
            let done_reading = match token.ty {
                TokenType::EOF => true,
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
                    TokenType::JsonBoolTok(b)  => Ok(JsonValue::JsonBool(b)),
                    TokenType::JsonNullTok     => Ok(JsonValue::JsonNull),
                    TokenType::StringTok(s)    => Ok(JsonValue::JsonString(s)),
                    TokenType::IntNum(i)       => Ok(JsonValue::JsonInt(i)),
                    TokenType::DecimalNum(f)   => Ok(JsonValue::JsonFloat(f)),
                    TokenType::LBracket        => self.parse_array(),
                    TokenType::LBrace          => self.parse_object(),
                    o               => Err(JsonError::new(format!("Unexpected token")))
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
                    TokenType::RBracket    => { self.next(); break },
                    _           => {
                        vec.push(try!(self.parse()));
                        peak = self.peak();
                        
                        if peak.is_some() {
                            match peak.unwrap().ty {
                                TokenType::Comma   => { self.next(); }, // consume comma
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
        Ok(JsonValue::JsonArray(vec))
    }

    fn parse_object(&mut self) -> Result<JsonValue, JsonError> {
        let mut map = HashMap::new();
        loop {
            let mut peak = self.peak();

            if peak.is_some() {
                match peak.unwrap().ty {
                    TokenType::RBrace => {
                        self.next();
                        break;
                    },
                    TokenType::StringTok(s) => {
                        self.next();
                        peak = self.peak();
                        if peak.is_some() {
                            match peak.unwrap().ty {
                                TokenType::Colon => { self.next(); },
                                _     => {
                                    return Err(JsonError::new(format!("Expected colon")))
                                }
                            }
                        } else { panic!("Unexpected end of object") }

                        map.insert(s, try!(self.parse()));

                        peak = self.peak();
                        if peak.is_some() {
                            match peak.unwrap().ty {
                                TokenType::Comma  => { self.next(); },
                                TokenType::RBrace => {
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
        Ok(JsonValue::JsonObject(map))
    }
}

type JsonMap = HashMap<String, JsonValue>;

#[derive(PartialEq)]
enum JsonValue {
    JsonObject(JsonMap),
    JsonArray(Vec<JsonValue>),
    JsonString(String),
    JsonBool(bool),
    JsonInt(i64),
    JsonFloat(f64),
    JsonNull
}

impl fmt::Debug for JsonValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &JsonValue::JsonObject(ref map)    => map.fmt(f),
            &JsonValue::JsonArray(ref arr)     => arr.fmt(f),
            &JsonValue::JsonString(ref s)      => s.fmt(f),
            &JsonValue::JsonBool(ref b)        => b.fmt(f),
            &JsonValue::JsonInt(ref i)         => i.fmt(f),
            &JsonValue::JsonFloat(ref fl)      => fl.fmt(f),
            &JsonValue::JsonNull               => NULL_LITERAL.fmt(f)
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
        JsonValue::JsonNull => {}, // ok
        _ => panic!("Expected JsonValue::JsonNull")
    }
}

#[test]
fn true_literal() {
    let json_string = "true".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();
    
    match json {
        JsonValue::JsonBool(true) => {}, // ok
        _ => panic!("Expected JsonValue::JsonBool(true)")
    }
}

#[test]
fn false_literal() {
    let json_string = "false".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();
    
    match json {
        JsonValue::JsonBool(false) => {}, // ok
        _ => panic!("Expected JsonValue::JsonBool(false)")
    }
}

#[test]
fn int_literal() {
    let json_string = "42".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();
    
    match json {
        JsonValue::JsonInt(42) => {}, // ok
        _ => panic!("Expected JsonValue::JsonInt(42)")
    }
}

#[test]
fn float_literal() {
    let json_string = "42.0".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();
    
    match json {
        JsonValue::JsonFloat(42f64) => {}, // ok
        _ => panic!("Expected JsonValue::JsonFloat(42)")
    }
}

#[test]
#[allow(unused_variables)]
fn empty_arr() {
    let json_string = "[]".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();

    let empty_arr: Vec<JsonValue> = vec!();

    match json {
        JsonValue::JsonArray(empty_arr) => {}, // ok
        _ => panic!("Expected JsonValue::JsonArray([])")
    }
}

#[test]
fn arr_all_types() {
    let json_string = "[true, false, null, 0, 0.0, [], {}]".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();
    
    let map = HashMap::new();
    let arr = vec![
        JsonValue::JsonBool(true), JsonValue::JsonBool(false), JsonValue::JsonNull, JsonValue::JsonInt(0),
        JsonValue::JsonFloat(0.0), JsonValue::JsonArray(vec!()), JsonValue::JsonObject(map)
    ];

    match json {
        JsonValue::JsonArray(vec) => {
            assert_eq!(vec, arr);
        },
        _ => panic!("Expected JsonValue::JsonArray([...])")
    }
}

#[test]
fn empty_object() {
    let json_string = "{}".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();

    match json {
        JsonValue::JsonObject(map) => {
            assert!(map.is_empty())
        }
        _ => panic!("Expected JsonValue::JsonObject")
    }
}

#[test]
fn object_vals() {
    let json_string = "{\"key\":\"value\"}".to_string();
    let json        = JsonValue::from_string(json_string).unwrap();

    let mut hash_map = HashMap::new();
    hash_map.insert("key".to_string(), JsonValue::JsonString("value".to_string()));

    match json {
        JsonValue::JsonObject(map) => {
            assert_eq!(map, hash_map);
        },
        _ => panic!("Expected JsonValue::JsonObject")
    }
}


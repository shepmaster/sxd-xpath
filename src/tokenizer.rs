use std::collections::HashMap;
use std::string;

use self::TokenizerErr::*;

use super::token::XPathToken;
use super::token::XPathToken::*;

fn is_digit(c: char) -> bool {
    match c {
        '0'...'9' => true,
        _ => false,
    }
}

pub struct XPathTokenizer {
    xpath: XPathString,
    start: uint,
    prefer_recognition_of_operator_names: bool,
    single_char_tokens: HashMap<char, XPathToken>,
    two_char_tokens: HashMap<&'static str, XPathToken>,
    named_operators: Vec<(&'static str, XPathToken)>,
}

pub type TokenResult = Result<XPathToken, TokenizerErr>;

#[deriving(Show,PartialEq,Clone,Copy)]
pub enum TokenizerErr {
    MissingLocalName,
    MismatchedQuoteCharacters,
    UnableToCreateToken,
}

struct XPathString {
    xpath: Vec<char>,
}

impl XPathString {
    fn new(xpath: &str) -> XPathString {
        XPathString {
            xpath: xpath.chars().collect(),
        }
    }

    fn len(& self) -> uint {
        self.xpath.len()
    }

    fn str_at_is(& self, offset: uint, needle: &[char]) -> bool {
        let s_len = needle.len();

        if self.xpath.len() < offset + s_len { return false; }

        let xpath_chars = self.xpath.slice(offset, offset + s_len);

        needle == xpath_chars
    }


    fn valid_ncname_start_char(& self, offset: uint) -> bool {
        let c = self.xpath[offset];
        if c >= 'A' && c <= 'Z' { return true }
        if c == '_' { return true }
        if c >= 'a' && c <= 'z' { return true }
        // TODO: All non-ASCII codepoints
        return false;
    }

    fn valid_ncname_follow_char(& self, offset: uint) -> bool {
        let c = self.xpath[offset];
        if self.valid_ncname_start_char(offset) { return true }
        if c == '-' { return true }
        if c == '.' { return true }
        if c >= '0' && c <= '9' { return true }
        // TODO: All non-ASCII codepoints
        return false;
    }

    fn while_valid_string(& self, offset: uint) -> uint {
        let mut offset = offset;

        if offset < self.xpath.len() && self.valid_ncname_start_char(offset) {
            offset += 1;

            while offset < self.xpath.len() && self.valid_ncname_follow_char(offset) {
                offset += 1;
            }
        }

        return offset;
    }

    fn while_valid_number(& self, offset: uint) -> uint {
        let mut offset = offset;

        while offset < self.xpath.len() && is_number_char(self.xpath[offset]) {
            offset += 1;
        }

        return offset;
    }

    fn while_not_character(& self, offset: uint, end_char: char) -> uint {
        let mut offset = offset;

        while offset < self.xpath.len() && self.xpath[offset] != end_char {
            offset += 1;
        }

        return offset;
    }


    fn substr(& self, start: uint, end: uint) -> string::String {
        string::String::from_chars(self.xpath.slice(start, end))
    }

    fn safe_substr(& self, start: uint, end: uint) -> Option<string::String> {
        if self.xpath.len() >= end {
            Some(self.substr(start, end))
        } else {
            None
        }
    }

    fn char_at(&self, offset: uint) -> char {
        self.xpath[offset]
    }

    fn char_at_is(&self, offset: uint, c: char) -> bool {
        let has_one_more = self.xpath.len() >= offset + 1;

        has_one_more && self.xpath[offset] == c
    }

    fn char_at_is_not(&self, offset: uint, c: char) -> bool {
        let has_one_more = self.xpath.len() >= offset + 1;

        ! has_one_more || self.xpath[offset] != c
    }

    fn char_at_is_not_digit(& self, offset: uint) -> bool {
        let has_more_chars = self.xpath.len() >= offset + 1;

        ! has_more_chars || ! is_digit(self.xpath[offset])
    }

    fn is_xml_space(&self, offset: uint) -> bool {
        let c = self.xpath[offset];

        return
            c == ' '  ||
            c == '\t' ||
            c == '\n' ||
            c == '\r';
    }

    fn end_of_whitespace(& self, offset: uint) -> uint {
        let mut offset = offset;

        while offset < self.xpath.len() && self.is_xml_space(offset) {
            offset += 1;
        }

        offset
    }
}

static QUOTE_CHARS: [char, .. 2] =  ['\'', '\"'];

impl XPathTokenizer {
    pub fn new(xpath: & str) -> XPathTokenizer {
        let single_char_tokens = {
            let mut m = HashMap::new();
            m.insert('/', XPathToken::Slash);
            m.insert('(', XPathToken::LeftParen);
            m.insert(')', XPathToken::RightParen);
            m.insert('[', XPathToken::LeftBracket);
            m.insert(']', XPathToken::RightBracket);
            m.insert('@', XPathToken::AtSign);
            m.insert('$', XPathToken::DollarSign);
            m.insert('+', XPathToken::PlusSign);
            m.insert('-', XPathToken::MinusSign);
            m.insert('|', XPathToken::Pipe);
            m.insert('=', XPathToken::Equal);
            m.insert('<', XPathToken::LessThan);
            m.insert('>', XPathToken::GreaterThan);
            m
        };

        let two_char_tokens = {
            let mut m = HashMap::new();
            m.insert("<=", XPathToken::LessThanOrEqual);
            m.insert(">=", XPathToken::GreaterThanOrEqual);
            m.insert("!=", XPathToken::NotEqual);
            m.insert("::", XPathToken::DoubleColon);
            m.insert("//", XPathToken::DoubleSlash);
            m.insert("..", XPathToken::ParentNode);
            m
        };

        let named_operators = vec![
            ("and", XPathToken::And),
            ("or",  XPathToken::Or),
            ("mod", XPathToken::Remainder),
            ("div", XPathToken::Divide),
            ("*",   XPathToken::Multiply)
        ];

        XPathTokenizer {
            xpath: XPathString::new(xpath),
            start: 0,
            prefer_recognition_of_operator_names: false,
            single_char_tokens: single_char_tokens,
            two_char_tokens: two_char_tokens,
            named_operators: named_operators,
        }
    }

    pub fn has_more_tokens(& self) -> bool {
        self.xpath.len() > self.start
    }

    fn tokenize_literal(& mut self, quote_char: char) -> TokenResult {
        let mut offset = self.start;

        offset += 1; // Skip over the starting quote
        let start_of_string = offset;

        offset = self.xpath.while_not_character(offset, quote_char);
        let end_of_string = offset;

        if self.xpath.char_at_is_not(offset, quote_char) {
            return Err(MismatchedQuoteCharacters);
        }
        offset += 1; // Skip over ending quote

        self.start = offset;
        return Ok(XPathToken::Literal(self.xpath.substr(start_of_string, end_of_string)));
    }

    fn raw_next_token(& mut self) -> TokenResult {
        if let Some(first_two) = self.xpath.safe_substr(self.start, self.start + 2) {
            if let Some(token) = self.two_char_tokens.get(first_two.as_slice()) {
                self.start += 2;
                return Ok(token.clone());
            }
        }

        let c = self.xpath.char_at(self.start);

        if let Some(token) = self.single_char_tokens.get(&c) {
            self.start += 1;
            return Ok(token.clone());
        }

        for quote_char in QUOTE_CHARS.iter() {
            if *quote_char == c {
                return self.tokenize_literal(*quote_char);
            }
        }

        if '.' == c {
            if self.xpath.char_at_is_not_digit(self.start + 1) {
                // Ugly. Should we use START / FOLLOW constructs?
                self.start += 1;
                return Ok(XPathToken::CurrentNode);
            }
        }

        if is_number_char(c) {
            let mut offset = self.start;
            let current_start = self.start;

            offset = self.xpath.while_valid_number(offset);

            self.start = offset;
            let substr = self.xpath.substr(current_start, offset);
            match from_str(substr.as_slice()) {
                Some(value) => Ok(XPathToken::Number(value)),
                None => panic!("Not really a number!")
            }
        } else {
            let mut offset = self.start;
            let current_start = self.start;

            if self.prefer_recognition_of_operator_names {
                for &(ref name, ref token) in self.named_operators.iter() {
                    let name_chars: Vec<char> = name.chars().collect();
                    let name_chars_slice = name_chars.as_slice();

                    if self.xpath.str_at_is(offset, name_chars_slice) {
                        self.start += name_chars.len();
                        return Ok(token.clone());
                    }
                }
            }

            if self.xpath.char_at_is(offset, '*') {
                self.start = offset + 1;
                return Ok(XPathToken::String("*".to_string()));
            }

            offset = self.xpath.while_valid_string(offset);

            if self.xpath.char_at_is(offset, ':') && self.xpath.char_at_is_not(offset + 1, ':') {
                let prefix = self.xpath.substr(current_start, offset);

                offset += 1;

                let current_start = offset;
                offset = self.xpath.while_valid_string(offset);

                if current_start == offset {
                    return Err(MissingLocalName);
                }

                let name = self.xpath.substr(current_start, offset);

                self.start = offset;
                return Ok(XPathToken::PrefixedName(prefix, name));

            } else {
                self.start = offset;
                return Ok(XPathToken::String(self.xpath.substr(current_start, offset)));
            }
        }
    }

    fn consume_whitespace(& mut self) {
        self.start = self.xpath.end_of_whitespace(self.start);
    }

    fn next_token(& mut self) -> TokenResult {
        self.consume_whitespace();

        let old_start = self.start;
        let token = self.raw_next_token();
        if token.is_err() { return token; }

        let token = token.unwrap();

        if old_start == self.start {
            return Err(UnableToCreateToken);
        }

        self.consume_whitespace();

        if ! (token.precedes_node_test() ||
              token.precedes_expression() ||
              token.is_operator()) {
            // See http://www.w3.org/TR/xpath/#exprlex
            self.prefer_recognition_of_operator_names = true;
        } else {
            self.prefer_recognition_of_operator_names = false;
        }

        return Ok(token);
    }
}

impl Iterator<TokenResult> for XPathTokenizer {
    fn next(&mut self) -> Option<TokenResult> {
        if self.has_more_tokens() {
            Some(self.next_token())
        } else {
            None
        }
    }
}

fn is_number_char(c: char) -> bool {
    return is_digit(c) || '.' == c;
}

pub struct XPathTokenDisambiguator<T, I> {
    source: ::std::iter::Peekable<T, I>,
}

impl<T, I: Iterator<T>> XPathTokenDisambiguator<T, I> {
    pub fn new(source: I) -> XPathTokenDisambiguator<T, I> {
        XPathTokenDisambiguator{
            source: source.peekable(),
        }
    }
}

static NODE_TEST_NAMES : [&'static str, .. 4] =
    [ "comment", "text", "processing-instruction", "node" ];

impl<I: Iterator<TokenResult>> Iterator<TokenResult> for XPathTokenDisambiguator<TokenResult, I> {
    fn next(&mut self) -> Option<TokenResult> {
        let token = self.source.next();
        let next  = self.source.peek();

        match (token, next) {
            (Some(Ok(XPathToken::String(val))), Some(&Ok(XPathToken::LeftParen))) => {
                if NODE_TEST_NAMES.contains(&val.as_slice()) {
                    Some(Ok(XPathToken::NodeTest(val)))
                } else {
                    Some(Ok(XPathToken::Function(val)))
                }
            },
            (Some(Ok(XPathToken::String(val))), Some(&Ok(XPathToken::DoubleColon))) => {
                Some(Ok(XPathToken::Axis(val)))
            },
            (token, _) => token,
        }
    }
}

pub struct XPathTokenDeabbreviator<I> {
    source: I,
    buffer: Vec<XPathToken>,
}

impl<I> XPathTokenDeabbreviator<I> {
    pub fn new(source: I) -> XPathTokenDeabbreviator<I> {
        XPathTokenDeabbreviator {
            source: source,
            buffer: vec!(),
        }
    }

    fn push(&mut self, token: XPathToken) {
        self.buffer.push(token);
    }

    fn expand_token(&mut self, token: XPathToken) {
        match token {
            XPathToken::AtSign => {
                self.push(XPathToken::String("attribute".to_string()));
                self.push(XPathToken::DoubleColon);
            }
            XPathToken::DoubleSlash => {
                self.push(XPathToken::Slash);
                self.push(XPathToken::String("descendant-or-self".to_string()));
                self.push(XPathToken::DoubleColon);
                self.push(XPathToken::String("node".to_string()));
                self.push(XPathToken::LeftParen);
                self.push(XPathToken::RightParen);
                self.push(XPathToken::Slash);
            }
            XPathToken::CurrentNode => {
                self.push(XPathToken::String("self".to_string()));
                self.push(XPathToken::DoubleColon);
                self.push(XPathToken::String("node".to_string()));
                self.push(XPathToken::LeftParen);
                self.push(XPathToken::RightParen);
            }
            XPathToken::ParentNode => {
                self.push(XPathToken::String("parent".to_string()));
                self.push(XPathToken::DoubleColon);
                self.push(XPathToken::String("node".to_string()));
                self.push(XPathToken::LeftParen);
                self.push(XPathToken::RightParen);
            }
            _ => {
                self.push(token);
            }
        }
    }
}

impl<I: Iterator<TokenResult>> Iterator<TokenResult> for XPathTokenDeabbreviator<I> {
    fn next(&mut self) -> Option<TokenResult> {
        if self.buffer.is_empty() {
            let token = self.source.next();

            match token {
                None => return token,
                Some(Err(_)) => return token,
                Some(Ok(token)) => self.expand_token(token),
            }
        }

        match self.buffer.remove(0) {
            Some(t) => Some(Ok(t)),
            None => panic!("No tokens left to return"), // Can't happen, we always add one
        }
    }
}

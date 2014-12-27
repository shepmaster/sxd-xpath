use std::string;

use document::peresil;
use document::peresil::{Point,Identifier};

use self::TokenizerErr::*;

use super::token::Token;
use super::token::Token::*;

fn is_digit(c: char) -> bool {
    match c {
        '0'...'9' => true,
        _ => false,
    }
}

pub struct Tokenizer {
    xpath: XPathString,
    start: uint,
    prefer_recognition_of_operator_names: bool,
    named_operators: Vec<(&'static str, Token)>,
}

pub type TokenResult = Result<Token, TokenizerErr>;

#[deriving(Show,PartialEq,Clone,Copy)]
pub enum TokenizerErr {
    MissingLocalName,
    MismatchedQuoteCharacters,
    UnableToCreateToken,
}

struct XPathString {
    xpath: Vec<char>,
    xpath2: string::String,
}

impl XPathString {
    fn new(xpath: &str) -> XPathString {
        XPathString {
            xpath: xpath.chars().collect(),
            xpath2: xpath.to_string(),
        }
    }

    fn slice_from(&self, offset: uint) -> &str {
        self.xpath2.slice_from(offset)
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

static SINGLE_CHAR_TOKENS: [Identifier<'static, Token>, ..13] = [
    ("/", Token::Slash),
    ("(", Token::LeftParen),
    (")", Token::RightParen),
    ("[", Token::LeftBracket),
    ("]", Token::RightBracket),
    ("@", Token::AtSign),
    ("$", Token::DollarSign),
    ("+", Token::PlusSign),
    ("-", Token::MinusSign),
    ("|", Token::Pipe),
    ("=", Token::Equal),
    ("<", Token::LessThan),
    (">", Token::GreaterThan),
];

static TWO_CHAR_TOKENS: [Identifier<'static, Token>, ..6] = [
    ("<=", Token::LessThanOrEqual),
    (">=", Token::GreaterThanOrEqual),
    ("!=", Token::NotEqual),
    ("::", Token::DoubleColon),
    ("//", Token::DoubleSlash),
    ("..", Token::ParentNode),
];

static QUOTE_CHARS: [char, .. 2] =  ['\'', '\"'];

impl Tokenizer {
    pub fn new(xpath: & str) -> Tokenizer {
        let named_operators = vec![
            ("and", Token::And),
            ("or",  Token::Or),
            ("mod", Token::Remainder),
            ("div", Token::Divide),
            ("*",   Token::Multiply)
        ];

        Tokenizer {
            xpath: XPathString::new(xpath),
            start: 0,
            prefer_recognition_of_operator_names: false,
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
        return Ok(Token::Literal(self.xpath.substr(start_of_string, end_of_string)));
    }

    fn raw_next_token(& mut self) -> TokenResult {
        {
            let p = Point { s: self.xpath.slice_from(self.start), offset: self.start };

            let r = p.consume_identifier::<_, ()>(TWO_CHAR_TOKENS.as_slice())
                .or_else(|| p.consume_identifier(SINGLE_CHAR_TOKENS.as_slice()));

            match r {
                peresil::Result::Success(p) => {
                    self.start = p.point.offset;
                    return Ok(p.data)
                },
                peresil::Result::Partial{ .. } |
                peresil::Result::Failure(..) => {
                    // Continue processing
                }
            }
        }

        let c = self.xpath.char_at(self.start);

        for quote_char in QUOTE_CHARS.iter() {
            if *quote_char == c {
                return self.tokenize_literal(*quote_char);
            }
        }

        if '.' == c {
            if self.xpath.char_at_is_not_digit(self.start + 1) {
                // Ugly. Should we use START / FOLLOW constructs?
                self.start += 1;
                return Ok(Token::CurrentNode);
            }
        }

        if is_number_char(c) {
            let mut offset = self.start;
            let current_start = self.start;

            offset = self.xpath.while_valid_number(offset);

            self.start = offset;
            let substr = self.xpath.substr(current_start, offset);
            match substr.parse() {
                Some(value) => Ok(Token::Number(value)),
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
                return Ok(Token::String("*".to_string()));
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
                return Ok(Token::PrefixedName(prefix, name));

            } else {
                self.start = offset;
                return Ok(Token::String(self.xpath.substr(current_start, offset)));
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

impl Iterator<TokenResult> for Tokenizer {
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

pub struct TokenDisambiguator<T, I> {
    source: ::std::iter::Peekable<T, I>,
}

impl<T, I: Iterator<T>> TokenDisambiguator<T, I> {
    pub fn new(source: I) -> TokenDisambiguator<T, I> {
        TokenDisambiguator{
            source: source.peekable(),
        }
    }
}

static NODE_TEST_NAMES : [&'static str, .. 4] =
    [ "comment", "text", "processing-instruction", "node" ];

impl<I: Iterator<TokenResult>> Iterator<TokenResult> for TokenDisambiguator<TokenResult, I> {
    fn next(&mut self) -> Option<TokenResult> {
        let token = self.source.next();
        let next  = self.source.peek();

        match (token, next) {
            (Some(Ok(Token::String(val))), Some(&Ok(Token::LeftParen))) => {
                if NODE_TEST_NAMES.contains(&val.as_slice()) {
                    Some(Ok(Token::NodeTest(val)))
                } else {
                    Some(Ok(Token::Function(val)))
                }
            },
            (Some(Ok(Token::String(val))), Some(&Ok(Token::DoubleColon))) => {
                Some(Ok(Token::Axis(val)))
            },
            (token, _) => token,
        }
    }
}

pub struct TokenDeabbreviator<I> {
    source: I,
    buffer: Vec<Token>,
}

impl<I> TokenDeabbreviator<I> {
    pub fn new(source: I) -> TokenDeabbreviator<I> {
        TokenDeabbreviator {
            source: source,
            buffer: vec!(),
        }
    }

    fn push(&mut self, token: Token) {
        self.buffer.push(token);
    }

    fn expand_token(&mut self, token: Token) {
        match token {
            Token::AtSign => {
                self.push(Token::String("attribute".to_string()));
                self.push(Token::DoubleColon);
            }
            Token::DoubleSlash => {
                self.push(Token::Slash);
                self.push(Token::String("descendant-or-self".to_string()));
                self.push(Token::DoubleColon);
                self.push(Token::String("node".to_string()));
                self.push(Token::LeftParen);
                self.push(Token::RightParen);
                self.push(Token::Slash);
            }
            Token::CurrentNode => {
                self.push(Token::String("self".to_string()));
                self.push(Token::DoubleColon);
                self.push(Token::String("node".to_string()));
                self.push(Token::LeftParen);
                self.push(Token::RightParen);
            }
            Token::ParentNode => {
                self.push(Token::String("parent".to_string()));
                self.push(Token::DoubleColon);
                self.push(Token::String("node".to_string()));
                self.push(Token::LeftParen);
                self.push(Token::RightParen);
            }
            _ => {
                self.push(token);
            }
        }
    }
}

impl<I: Iterator<TokenResult>> Iterator<TokenResult> for TokenDeabbreviator<I> {
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

#[cfg(test)]
mod test {
    use super::super::token::Token;

    use super::Tokenizer;
    use super::{TokenResult,TokenizerErr};
    use super::TokenizerErr::{
        MismatchedQuoteCharacters,
        MissingLocalName,
        UnableToCreateToken,
    };

    use super::TokenDisambiguator;
    use super::TokenDeabbreviator;

    fn is_finished(tokenizer: & Tokenizer) -> bool {
        ! tokenizer.has_more_tokens()
    }

    fn all_tokens_raw<I: Iterator<TokenResult>>(tokenizer: I) -> Result<Vec<Token>, TokenizerErr> {
        tokenizer.collect()
    }

    fn all_tokens<I: Iterator<TokenResult>>(tokenizer: I) -> Vec<Token> {
        match all_tokens_raw(tokenizer) {
            Ok(toks) => toks,
            Err(msg) => panic!(msg),
        }
    }

    #[test]
    fn empty_string_has_no_tokens()
    {
        let tokenizer = Tokenizer::new("");
        assert!(is_finished(&tokenizer));
    }

    #[test]
    fn tokenizes_simple_string()
    {
        let tokenizer = Tokenizer::new("hello");

        assert_eq!(all_tokens(tokenizer), vec!(Token::String("hello".to_string())));
    }

    #[test]
    fn tokenizes_grandchild_selector()
    {
        let tokenizer = Tokenizer::new("hello/world");

        assert_eq!(all_tokens(tokenizer), vec!(Token::String("hello".to_string()),
                                               Token::Slash,
                                               Token::String("world".to_string())));
    }

    #[test]
    fn tokenizes_great_grandchild_selector()
    {
        let tokenizer = Tokenizer::new("hello/there/world");

        assert_eq!(all_tokens(tokenizer), vec!(Token::String("hello".to_string()),
                                               Token::Slash,
                                               Token::String("there".to_string()),
                                               Token::Slash,
                                               Token::String("world".to_string())));
    }

    #[test]
    fn tokenizes_qualified_names()
    {
        let tokenizer = Tokenizer::new("ns:foo");

        assert_eq!(all_tokens(tokenizer), vec!(Token::PrefixedName("ns".to_string(), "foo".to_string())));
    }

    #[test]
    fn ignores_whitespace_around_tokens()
    {
        let tokenizer = Tokenizer::new(" @\t@\n@\r");

        assert_eq!(all_tokens(tokenizer), vec!(Token::AtSign,
                                               Token::AtSign,
                                               Token::AtSign));
    }

    #[test]
    fn tokenizes_wildcard_name_test()
    {
        let tokenizer = Tokenizer::new("*");

        assert_eq!(all_tokens(tokenizer), vec!(Token::String("*".to_string())));
    }

    #[test]
    fn tokenizes_axis_separator()
    {
        let tokenizer = Tokenizer::new("::");

        assert_eq!(all_tokens(tokenizer), vec!(Token::DoubleColon));
    }

    #[test]
    fn tokenizes_axis_selector()
    {
        let tokenizer = Tokenizer::new("hello::world");

        assert_eq!(all_tokens(tokenizer), vec!(Token::String("hello".to_string()),
                                               Token::DoubleColon,
                                               Token::String("world".to_string())));
    }

    #[test]
    fn tokenizes_single_slash()
    {
        let tokenizer = Tokenizer::new("/");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Slash));
    }

    #[test]
    fn tokenizes_double_slash()
    {
        let tokenizer = Tokenizer::new("//");

        assert_eq!(all_tokens(tokenizer), vec!(Token::DoubleSlash));
    }

    #[test]
    fn tokenizes_double_slash_separator()
    {
        let tokenizer = Tokenizer::new("hello//world");

        assert_eq!(all_tokens(tokenizer), vec!(Token::String("hello".to_string()),
                                               Token::DoubleSlash,
                                               Token::String("world".to_string())));
    }

    #[test]
    fn tokenizes_left_paren()
    {
        let tokenizer = Tokenizer::new("(");

        assert_eq!(all_tokens(tokenizer), vec!(Token::LeftParen));
    }

    #[test]
    fn tokenizes_right_paren()
    {
        let tokenizer = Tokenizer::new(")");

        assert_eq!(all_tokens(tokenizer), vec!(Token::RightParen));
    }

    #[test]
    fn tokenizes_at_sign()
    {
        let tokenizer = Tokenizer::new("@");

        assert_eq!(all_tokens(tokenizer), vec!(Token::AtSign));
    }

    #[test]
    fn tokenizes_single_dot()
    {
        let tokenizer = Tokenizer::new(".");

        assert_eq!(all_tokens(tokenizer), vec!(Token::CurrentNode));
    }

    #[test]
    fn tokenizes_double_dot()
    {
        let tokenizer = Tokenizer::new("..");

        assert_eq!(all_tokens(tokenizer), vec!(Token::ParentNode));
    }

    #[test]
    fn tokenizes_integral_number()
    {
        let tokenizer = Tokenizer::new("42");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Number(42.0)));
    }

    #[test]
    fn tokenizes_decimal_number()
    {
        let tokenizer = Tokenizer::new("42.42");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Number(42.42)));
    }

    #[test]
    fn tokenizes_decimal_number_without_integral_part()
    {
        let tokenizer = Tokenizer::new(".40");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Number(0.40)));
    }

    #[test]
    fn tokenizes_left_bracket()
    {
        let tokenizer = Tokenizer::new("[");

        assert_eq!(all_tokens(tokenizer), vec!(Token::LeftBracket));
    }

    #[test]
    fn tokenizes_right_bracket()
    {
        let tokenizer = Tokenizer::new("]");

        assert_eq!(all_tokens(tokenizer), vec!(Token::RightBracket));
    }

    #[test]
    fn tokenizes_apostrophe_literal()
    {
        let tokenizer = Tokenizer::new("'hello!'");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Literal("hello!".to_string())));
    }

    #[test]
    fn tokenizes_double_quote_literal()
    {
        let tokenizer = Tokenizer::new("\"1.23\"");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Literal("1.23".to_string())));
    }

    #[test]
    fn tokenizes_dollar_sign()
    {
        let tokenizer = Tokenizer::new("$");

        assert_eq!(all_tokens(tokenizer), vec!(Token::DollarSign));
    }

    #[test]
    fn tokenizes_plus_sign()
    {
        let tokenizer = Tokenizer::new("+");

        assert_eq!(all_tokens(tokenizer), vec!(Token::PlusSign));
    }

    #[test]
    fn tokenizes_minus_sign()
    {
        let tokenizer = Tokenizer::new("-");

        assert_eq!(all_tokens(tokenizer), vec!(Token::MinusSign));
    }

    #[test]
    fn tokenizes_pipe()
    {
        let tokenizer = Tokenizer::new("|");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Pipe));
    }

    #[test]
    fn tokenizes_equal_sign()
    {
        let tokenizer = Tokenizer::new("=");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Equal));
    }

    #[test]
    fn tokenizes_not_equal_sign()
    {
        let tokenizer = Tokenizer::new("!=");

        assert_eq!(all_tokens(tokenizer), vec!(Token::NotEqual));
    }

    #[test]
    fn tokenizes_less_than()
    {
        let tokenizer = Tokenizer::new("<");

        assert_eq!(all_tokens(tokenizer), vec!(Token::LessThan));
    }

    #[test]
    fn tokenizes_less_than_or_equal()
    {
        let tokenizer = Tokenizer::new("<=");

        assert_eq!(all_tokens(tokenizer), vec!(Token::LessThanOrEqual));
    }

    #[test]
    fn tokenizes_greater_than()
    {
        let tokenizer = Tokenizer::new(">");

        assert_eq!(all_tokens(tokenizer), vec!(Token::GreaterThan));
    }

    #[test]
    fn tokenizes_greater_than_or_equal()
    {
        let tokenizer = Tokenizer::new(">=");

        assert_eq!(all_tokens(tokenizer), vec!(Token::GreaterThanOrEqual));
    }

    #[test]
    fn special_preceding_token_forces_named_operator_and()
    {
        let tokenizer = Tokenizer::new("1andz2");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Number(1.0),
                                               Token::And,
                                               Token::String("z2".to_string())));
    }

    #[test]
    fn special_preceding_token_forces_named_operator_or()
    {
        let tokenizer = Tokenizer::new("2oror");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Number(2.0),
                                               Token::Or,
                                               Token::String("or".to_string())));
    }

    #[test]
    fn special_preceding_token_forces_named_operator_mod()
    {
        let tokenizer = Tokenizer::new("3moddiv");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Number(3.0),
                                               Token::Remainder,
                                               Token::String("div".to_string())));
    }

    #[test]
    fn special_preceding_token_forces_named_operator_div()
    {
        let tokenizer = Tokenizer::new("1divz2");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Number(1.0),
                                               Token::Divide,
                                               Token::String("z2".to_string())));
    }

    #[test]
    fn special_preceding_token_forces_named_operator_multiply()
    {
        let tokenizer = Tokenizer::new("1*2");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Number(1.0),
                                               Token::Multiply,
                                               Token::Number(2.0)));
    }

    #[test]
    fn exception_thrown_when_nothing_was_tokenized()
    {
        let tokenizer = Tokenizer::new("!");
        let res = all_tokens_raw(tokenizer);

        assert_eq!(Err(UnableToCreateToken), res);
    }

    #[test]
    fn exception_thrown_when_name_test_has_no_local_name()
    {
        let tokenizer = Tokenizer::new("ns:");
        let res = all_tokens_raw(tokenizer);

        assert_eq!(Err(MissingLocalName), res);
    }

    #[test]
    fn exception_thrown_when_quote_characters_mismatched()
    {
        let tokenizer = Tokenizer::new("'hello\"");
        let res = all_tokens_raw(tokenizer);

        assert_eq!(Err(MismatchedQuoteCharacters), res);
    }

    #[test]
    fn disambiguates_node_test_functions() {
        // Would prefer parametric tests
        for name in ["comment", "text", "processing-instruction", "node"].iter() {
            let input_tokens: Vec<TokenResult> = vec!(
                Ok(Token::String(name.to_string())),
                Ok(Token::LeftParen),
            );

            let disambig = TokenDisambiguator::new(input_tokens.into_iter());

            assert_eq!(all_tokens(disambig),
                       vec!(Token::NodeTest(name.to_string()),
                            Token::LeftParen));
        }
    }

    #[test]
    fn name_followed_by_left_paren_becomes_function_name() {
        let input_tokens: Vec<TokenResult> = vec!(
            Ok(Token::String("test".to_string())),
            Ok(Token::LeftParen),
         );

        let disambig = TokenDisambiguator::new(input_tokens.into_iter());

        assert_eq!(all_tokens(disambig),
                   vec!(Token::Function("test".to_string()),
                        Token::LeftParen));
    }

    #[test]
    fn name_followed_by_double_colon_becomes_axis_name() {
        let input_tokens: Vec<TokenResult> = vec!(
            Ok(Token::String("test".to_string())),
            Ok(Token::DoubleColon),
        );

        let disambig = TokenDisambiguator::new(input_tokens.into_iter());

        assert_eq!(all_tokens(disambig),
                   vec!(Token::Axis("test".to_string()),
                        Token::DoubleColon));
    }

    #[test]
    fn converts_at_sign_to_attribute_axis() {
        let input_tokens: Vec<TokenResult> = vec!(Ok(Token::AtSign));
        // let iter: &Iterator<TokenResult> = &input_tokens.into_iter();

        let deabbrv = TokenDeabbreviator::new(input_tokens.into_iter());
        // let a: () = deabbrv.next();
        // println!("{}",a );

        assert_eq!(all_tokens(deabbrv), vec!(Token::String("attribute".to_string()),
                                             Token::DoubleColon));
    }

    #[test]
    fn converts_double_slash_to_descendant_or_self() {
        let input_tokens: Vec<TokenResult> = vec!(Ok(Token::DoubleSlash));

        let deabbrv = TokenDeabbreviator::new(input_tokens.into_iter());

        assert_eq!(all_tokens(deabbrv), vec!(Token::Slash,
                                             Token::String("descendant-or-self".to_string()),
                                             Token::DoubleColon,
                                             Token::String("node".to_string()),
                                             Token::LeftParen,
                                             Token::RightParen,
                                             Token::Slash));
    }

    #[test]
    fn converts_current_node_to_self_node() {
        let input_tokens: Vec<TokenResult> = vec!(Ok(Token::CurrentNode));

        let deabbrv = TokenDeabbreviator::new(input_tokens.into_iter());

        assert_eq!(all_tokens(deabbrv), vec!(Token::String("self".to_string()),
                                             Token::DoubleColon,
                                             Token::String("node".to_string()),
                                             Token::LeftParen,
                                             Token::RightParen));
    }

    #[test]
    fn converts_parent_node_to_parent_node() {
        let input_tokens: Vec<TokenResult> = vec!(Ok(Token::ParentNode));

        let deabbrv = TokenDeabbreviator::new(input_tokens.into_iter());

        assert_eq!(all_tokens(deabbrv), vec!(Token::String("parent".to_string()),
                                             Token::DoubleColon,
                                             Token::String("node".to_string()),
                                             Token::LeftParen,
                                             Token::RightParen));
    }
}

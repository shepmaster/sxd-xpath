use std::string;

use document::peresil;
use document::peresil::{Point,Identifier};
use document::parser::XmlParseExt;

use self::TokenizerErr::*;

use super::token::Token;
use super::token::Token::*;

pub struct Tokenizer {
    xpath: XPathString,
    start: uint,
    prefer_recognition_of_operator_names: bool,
}

pub type TokenResult = Result<Token, TokenizerErr>;

#[deriving(Show,PartialEq,Clone,Copy)]
pub enum TokenizerErr {
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
}

trait XPathParseExt<'a> {
    fn consume_quoted_string<E>(&self, quote: &str) -> peresil::Result<'a, &'a str, E>;
}

impl<'a> XPathParseExt<'a> for Point<'a> {
    fn consume_quoted_string<E>(&self, quote: &str) -> peresil::Result<'a, &'a str, E> {
        let end_of_str = self.s.find_str(quote).or(Some(self.s.len()));
        self.consume_to(end_of_str)
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

static NAMED_OPERATORS: [Identifier<'static, Token>, ..5] = [
    ("and", Token::And),
    ("or",  Token::Or),
    ("mod", Token::Remainder),
    ("div", Token::Divide),
    ("*",   Token::Multiply),
];

fn parse_quoted_literal<'a>(p: Point<'a>, quote: &str)
                            -> peresil::Result<'a, Token, TokenizerErr>
{
    let (_, p) = try_parse!(p.consume_literal(quote));
    let (v, p) = try_parse!(p.consume_quoted_string(quote));
    let (_, p) = try_parse!(p.consume_literal(quote), MismatchedQuoteCharacters);

    let tok = Token::Literal(v.to_string());
    peresil::Result::success(tok, p)
}

fn parse_number<'a>(p: Point<'a>) -> peresil::Result<'a, Token, TokenizerErr> {
    fn fractional_part<'a, E>(p: Point<'a>) -> peresil::Result<'a, (), E> {
        let (_, p) = try_parse!(p.consume_literal("."));
        let (_, p) = p.consume_decimal_chars::<E>().optional(p);

        peresil::Result::success((), p)
    }

    fn with_integer<'a, E>(p: Point<'a>) -> peresil::Result<'a, (), E> {
        let (_, p) = try_parse!(p.consume_decimal_chars());
        let (_, p) = fractional_part::<E>(p).optional(p);

        peresil::Result::success((), p)
    }

    fn without_integer<'a, E>(p: Point<'a>) -> peresil::Result<'a, (), E> {
        let (_, p) = try_parse!(p.consume_literal("."));
        let (_, p) = try_parse!(p.consume_decimal_chars());

        peresil::Result::success((), p)
    }

    let before_p = p;

    let (_, p) = try_parse!(with_integer(p)
                            .or_else(|| without_integer(p)));

    let num = before_p.to(p);
    let num = num.parse().expect("Unable to parse number");

    peresil::Result::success(Token::Number(num), p)
}

fn parse_current_node<'a>(p: Point<'a>) -> peresil::Result<'a, Token, TokenizerErr> {
    let (_, p) = try_parse!(p.consume_literal("."));

    peresil::Result::success(Token::CurrentNode, p)
}

fn parse_named_operators<'a>(p: Point<'a>, prefer_named_ops: bool)
                          -> peresil::Result<'a, Token, TokenizerErr>
{
    if prefer_named_ops {
        p.consume_identifier(NAMED_OPERATORS.as_slice())
    } else {
        peresil::Result::failure(None, p)
    }
}

fn parse_name_test<'a>(p: Point<'a>) -> peresil::Result<'a, Token, TokenizerErr> {
    fn wildcard<'a, E>(p: Point<'a>) -> peresil::Result<'a, Token, E> {
        let (wc, p) = try_parse!(p.consume_literal("*"));

        let tok = Token::String(wc.to_string());
        peresil::Result::success(tok, p)
    }

    fn prefixed_wildcard<'a, E>(p: Point<'a>) -> peresil::Result<'a, Token, E> {
        let (prefix, p) = try_parse!(p.consume_ncname());
        let (_, p) = try_parse!(p.consume_literal(":"));
        let (wc, p) = try_parse!(p.consume_literal("*"));

        let tok = Token::PrefixedName(prefix.to_string(), wc.to_string());
        peresil::Result::success(tok, p)
    }

    fn prefixed_name<'a, E>(p: Point<'a>) -> peresil::Result<'a, Token, E> {
        p.consume_prefixed_name().map(|name| {
            let local = name.local_part.to_string();
            match name.prefix {
                Some(prefix) => Token::PrefixedName(prefix.to_string(), local),
                None         => Token::String(local),
            }
        })
    }

    wildcard(p)
        .or_else(|| prefixed_wildcard(p))
        .or_else(|| prefixed_name(p))
}

impl Tokenizer {
    pub fn new(xpath: & str) -> Tokenizer {
        Tokenizer {
            xpath: XPathString::new(xpath),
            start: 0,
            prefer_recognition_of_operator_names: false,
        }
    }

    pub fn has_more_tokens(& self) -> bool {
        self.xpath.len() > self.start
    }

    fn parse_token<'a>(&self, p: Point<'a>) -> peresil::Result<'a, Token, TokenizerErr> {
        let (_, p) = p.consume_space().optional(p);

        let (tok, p) = try_parse!({
            p.consume_identifier(TWO_CHAR_TOKENS.as_slice())
                .or_else(|| p.consume_identifier(SINGLE_CHAR_TOKENS.as_slice()))
                .or_else(|| parse_quoted_literal(p, "\x22")) // "
                .or_else(|| parse_quoted_literal(p, "\x27")) // '
                .or_else(|| parse_number(p))
                .or_else(|| parse_current_node(p))
                .or_else(|| parse_named_operators(p, self.prefer_recognition_of_operator_names))
                .or_else(|| parse_name_test(p))
        });

        let (_, p) = p.consume_space().optional(p);

        peresil::Result::success(tok, p)
    }

    fn raw_next_token(& mut self) -> TokenResult {
        let p = Point { s: self.xpath.slice_from(self.start), offset: self.start };

        match self.parse_token(p) {
            peresil::Result::Success(p) => {
                self.start = p.point.offset;
                return Ok(p.data)
            },
            peresil::Result::Partial{ failure: p, .. } |
            peresil::Result::Failure(p) => {
                match p.data {
                    Some(e) => Err(e),
                    None    => Err(UnableToCreateToken),
                }
            }
        }
    }

    fn next_token(& mut self) -> TokenResult {
        let old_start = self.start;
        let token = self.raw_next_token();
        if token.is_err() { return token; }

        let token = token.unwrap();

        if old_start == self.start {
            return Err(UnableToCreateToken);
        }

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

        assert_eq!(Err(UnableToCreateToken), res);
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

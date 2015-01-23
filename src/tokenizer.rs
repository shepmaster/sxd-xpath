use std::{error,fmt,string};

use document::peresil;
use document::peresil::{Point,Identifier};
use document::parser::XmlParseExt;

use self::TokenizerErr::*;

use super::node_test;
use super::token::{Token,AxisName,NodeTestName};
use super::token::Token::*;

pub struct Tokenizer {
    xpath: string::String,
    start: usize,
    prefer_recognition_of_operator_names: bool,
}

pub type TokenResult = Result<Token, TokenizerErr>;

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum TokenizerErr {
    MismatchedQuoteCharacters,
    UnableToCreateToken,
}

impl error::Error for TokenizerErr {
    fn description(&self) -> &str {
        use self::TokenizerErr::*;
        match self {
            &MismatchedQuoteCharacters => "mismatched quote character",
            &UnableToCreateToken       => "unable to create token",
        }
    }
}

impl fmt::Display for TokenizerErr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::TokenizerErr::*;
        let as_err = self as &error::Error;
        match self {
            &MismatchedQuoteCharacters |
            &UnableToCreateToken       => {
                as_err.description().fmt(fmt)
            },
        }
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

static SINGLE_CHAR_TOKENS: [Identifier<'static, Token>; 13] = [
    ("/", Token::Slash),
    ("(", Token::LeftParen),
    (")", Token::RightParen),
    ("[", Token::LeftBracket),
    ("]", Token::RightBracket),
    ("@", Token::AtSign),
    ("+", Token::PlusSign),
    ("-", Token::MinusSign),
    ("|", Token::Pipe),
    ("=", Token::Equal),
    ("<", Token::LessThan),
    (">", Token::GreaterThan),
    (",", Token::Comma),
];

static TWO_CHAR_TOKENS: [Identifier<'static, Token>; 5] = [
    ("<=", Token::LessThanOrEqual),
    (">=", Token::GreaterThanOrEqual),
    ("!=", Token::NotEqual),
    ("//", Token::DoubleSlash),
    ("..", Token::ParentNode),
];

static NAMED_OPERATORS: [Identifier<'static, Token>; 5] = [
    ("and", Token::And),
    ("or",  Token::Or),
    ("mod", Token::Remainder),
    ("div", Token::Divide),
    ("*",   Token::Multiply),
];

// These will be matched in order, so substrings should come later.
static AXES: [Identifier<'static, AxisName>; 13] = [
    ("ancestor-or-self", AxisName::AncestorOrSelf),
    ("ancestor", AxisName::Ancestor),
    ("attribute", AxisName::Attribute),
    ("child", AxisName::Child),
    ("descendant-or-self", AxisName::DescendantOrSelf),
    ("descendant", AxisName::Descendant),
    ("following-sibling", AxisName::FollowingSibling),
    ("following", AxisName::Following),
    ("namespace", AxisName::Namespace),
    ("parent", AxisName::Parent),
    ("preceding-sibling", AxisName::PrecedingSibling),
    ("preceding", AxisName::Preceding),
    ("self", AxisName::Self),
];

static NODE_TESTS_WITHOUT_ARG : [Identifier<'static, NodeTestName>; 3] = [
    ("comment", NodeTestName::Comment),
    ("text", NodeTestName::Text),
    ("node", NodeTestName::Node),
];

fn parse_literal<'a>(p: Point<'a>) -> peresil::Result<'a, &'a str, TokenizerErr> {
    fn with_quote<'a>(p: Point<'a>, quote: &str)
                     -> peresil::Result<'a, &'a str, TokenizerErr>
    {
        let (_, p) = try_parse!(p.consume_literal(quote));
        let (v, p) = try_parse!(p.consume_quoted_string(quote));
        let (_, p) = try_parse!(p.consume_literal(quote), MismatchedQuoteCharacters);

        peresil::Result::success(v, p)
    }

    with_quote(p, "\x22") // "
        .or_else(|| with_quote(p, "\x27")) // '
}

fn parse_quoted_literal<'a>(p: Point<'a>) -> peresil::Result<'a, Token, TokenizerErr> {
    parse_literal(p).map(|v| Token::Literal(v.to_string()))
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

fn parse_axis_specifier<'a>(p: Point<'a>) -> peresil::Result<'a, Token, TokenizerErr> {
    // Ideally, we would check for the pair of the name and the ::,
    // then loop. This would prevent us from having to order AXES.
    let (axis, p) = try_parse!(p.consume_identifier(AXES.as_slice()));
    let (_, p) = try_parse!(p.consume_literal("::"));

    peresil::Result::success(Token::Axis(axis), p)
}

fn parse_node_type<'a>(p: Point<'a>) -> peresil::Result<'a, Token, TokenizerErr> {
    fn without_arg<'a, E>(p: Point<'a>) -> peresil::Result<'a, Token, E> {
        let (node_type, p) = try_parse!(p.consume_identifier(NODE_TESTS_WITHOUT_ARG.as_slice()));
        let (_, p) = try_parse!(p.consume_literal("()"));

        peresil::Result::success(Token::NodeTest(node_type), p)
    }

    fn with_arg<'a>(p: Point<'a>) -> peresil::Result<'a, Token, TokenizerErr> {
        let (_, p) = try_parse!(p.consume_literal("processing-instruction("));
        let (arg, p) = try_parse!(parse_literal(p));
        let (_, p) = try_parse!(p.consume_literal(")"));

        let name = NodeTestName::ProcessingInstruction(arg.to_string());
        peresil::Result::success(Token::NodeTest(name), p)
    }

    without_arg(p)
        .or_else(|| with_arg(p))
}

fn parse_function_call<'a>(p: Point<'a>) -> peresil::Result<'a, Token, TokenizerErr> {
    let (name, p) = try_parse!(p.consume_prefixed_name());
    // Do not advance the point here. We want to know if there *is* a
    // left-paren, but do not want to actually consume it here.
    try_parse!(p.consume_literal("("));

    // TODO: We should be using the prefix here!
    let name = name.local_part.to_string();
    peresil::Result::success(Token::Function(name), p)
}

fn parse_name_test<'a>(p: Point<'a>) -> peresil::Result<'a, Token, TokenizerErr> {
    fn wildcard<'a, E>(p: Point<'a>) -> peresil::Result<'a, Token, E> {
        let (wc, p) = try_parse!(p.consume_literal("*"));

        let name = node_test::NameTest {
            prefix: None,
            local_part: wc.to_string(),
        };
        peresil::Result::success(Token::NameTest(name), p)
    }

    fn prefixed_wildcard<'a, E>(p: Point<'a>) -> peresil::Result<'a, Token, E> {
        let (prefix, p) = try_parse!(p.consume_ncname());
        let (_, p) = try_parse!(p.consume_literal(":"));
        let (wc, p) = try_parse!(p.consume_literal("*"));

        let name = node_test::NameTest {
            prefix: Some(prefix.to_string()),
            local_part: wc.to_string(),
        };
        peresil::Result::success(Token::NameTest(name), p)
    }

    fn prefixed_name<'a, E>(p: Point<'a>) -> peresil::Result<'a, Token, E> {
        p.consume_prefixed_name().map(|name| {
            Token::NameTest(node_test::NameTest {
                prefix: name.prefix.map(|p| p.to_string()),
                local_part: name.local_part.to_string()
            })
        })
    }

    wildcard(p)
        .or_else(|| prefixed_wildcard(p))
        .or_else(|| prefixed_name(p))
}

fn parse_variable_reference<'a>(p: Point<'a>) -> peresil::Result<'a, Token, TokenizerErr> {
    let (_, p) = try_parse!(p.consume_literal("$"));
    let (name, p) = try_parse!(p.consume_prefixed_name());

    // TODO: We should be using the prefix here!
    let name = name.local_part.to_string();
    peresil::Result::success(Token::Variable(name), p)
}

impl Tokenizer {
    pub fn new(xpath: &str) -> Tokenizer {
        Tokenizer {
            xpath: xpath.to_string(),
            start: 0,
            prefer_recognition_of_operator_names: false,
        }
    }

    pub fn has_more_tokens(&self) -> bool {
        self.xpath.len() > self.start
    }

    fn parse_token<'a>(&self, p: Point<'a>) -> peresil::Result<'a, Token, TokenizerErr> {
        let (_, p) = p.consume_space().optional(p);

        let (tok, p) = try_parse!({
            p.consume_identifier(TWO_CHAR_TOKENS.as_slice())
                .or_else(|| p.consume_identifier(SINGLE_CHAR_TOKENS.as_slice()))
                .or_else(|| parse_quoted_literal(p))
                .or_else(|| parse_number(p))
                .or_else(|| parse_current_node(p))
                .or_else(|| parse_named_operators(p, self.prefer_recognition_of_operator_names))
                .or_else(|| parse_axis_specifier(p))
                .or_else(|| parse_node_type(p))
                .or_else(|| parse_function_call(p))
                .or_else(|| parse_name_test(p))
                .or_else(|| parse_variable_reference(p))
        });

        let (_, p) = p.consume_space().optional(p);

        peresil::Result::success(tok, p)
    }

    fn raw_next_token(&mut self) -> TokenResult {
        let p = Point { s: self.xpath.slice_from(self.start), offset: self.start };

        match self.parse_token(p) {
            peresil::Result::Success(p) => {
                self.start = p.point.offset;
                Ok(p.data)
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

    fn next_token(&mut self) -> TokenResult {
        let token = try!(self.raw_next_token());

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

impl Iterator for Tokenizer {
    type Item = TokenResult;

    fn next(&mut self) -> Option<TokenResult> {
        if self.has_more_tokens() {
            Some(self.next_token())
        } else {
            None
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
                self.push(Token::Axis(AxisName::Attribute));
            }
            Token::DoubleSlash => {
                self.push(Token::Slash);
                self.push(Token::Axis(AxisName::DescendantOrSelf));
                self.push(Token::NodeTest(NodeTestName::Node));
                self.push(Token::Slash);
            }
            Token::CurrentNode => {
                self.push(Token::Axis(AxisName::Self));
                self.push(Token::NodeTest(NodeTestName::Node));
            }
            Token::ParentNode => {
                self.push(Token::Axis(AxisName::Parent));
                self.push(Token::NodeTest(NodeTestName::Node));
            }
            _ => {
                self.push(token);
            }
        }
    }
}

impl<I> Iterator for TokenDeabbreviator<I>
    where I: Iterator<Item=TokenResult>
{
    type Item = TokenResult;

    fn next(&mut self) -> Option<TokenResult> {
        if self.buffer.is_empty() {
            let token = self.source.next();

            match token {
                None => return token,
                Some(Err(_)) => return token,
                Some(Ok(token)) => self.expand_token(token),
            }
        }

        Some(Ok(self.buffer.remove(0)))
    }
}

#[cfg(test)]
mod test {
    use super::super::node_test;
    use super::super::token::{Token,AxisName,NodeTestName};

    use super::Tokenizer;
    use super::{TokenResult,TokenizerErr};
    use super::TokenizerErr::{
        MismatchedQuoteCharacters,
        UnableToCreateToken,
    };

    use super::TokenDeabbreviator;

    fn is_finished(tokenizer: &Tokenizer) -> bool {
        ! tokenizer.has_more_tokens()
    }

    fn all_tokens_raw<I>(tokenizer: I) -> Result<Vec<Token>, TokenizerErr>
        where I: Iterator<Item=TokenResult>
    {
        tokenizer.collect()
    }

    fn all_tokens<I>(tokenizer: I) -> Vec<Token>
        where I: Iterator<Item=TokenResult>
    {
        match all_tokens_raw(tokenizer) {
            Ok(toks) => toks,
            Err(msg) => panic!("{:?}", msg),
        }
    }

    fn name_test(local_part: &str) -> Token {
        Token::NameTest(node_test::NameTest {
            prefix: None,
            local_part: local_part.to_string()
        })
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

        assert_eq!(all_tokens(tokenizer), vec!(name_test("hello")));
    }

    #[test]
    fn tokenizes_grandchild_selector()
    {
        let tokenizer = Tokenizer::new("hello/world");

        assert_eq!(all_tokens(tokenizer), vec!(name_test("hello"),
                                               Token::Slash,
                                               name_test("world")));
    }

    #[test]
    fn tokenizes_great_grandchild_selector()
    {
        let tokenizer = Tokenizer::new("hello/there/world");

        assert_eq!(all_tokens(tokenizer), vec!(name_test("hello"),
                                               Token::Slash,
                                               name_test("there"),
                                               Token::Slash,
                                               name_test("world")));
    }

    #[test]
    fn tokenizes_qualified_names()
    {
        let tokenizer = Tokenizer::new("ns:foo");

        let name = node_test::NameTest {
            prefix: Some("ns".to_string()),
            local_part: "foo".to_string()
        };
        assert_eq!(all_tokens(tokenizer), vec![Token::NameTest(name)]);
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

        assert_eq!(all_tokens(tokenizer), vec!(name_test("*")));
    }

    #[test]
    fn tokenizes_axis_selector()
    {
        let tokenizer = Tokenizer::new("ancestor::world");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Axis(AxisName::Ancestor),
                                               name_test("world")));
    }

    #[test]
    fn tokenizes_axis_selector_that_contains_another_axis()
    {
        let tokenizer = Tokenizer::new("ancestor-or-self::world");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Axis(AxisName::AncestorOrSelf),
                                               name_test("world")));
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

        assert_eq!(all_tokens(tokenizer), vec!(name_test("hello"),
                                               Token::DoubleSlash,
                                               name_test("world")));
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
    fn tokenizes_variable_reference()
    {
        let tokenizer = Tokenizer::new("$yo");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Variable("yo".to_string())));
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
                                               name_test("z2")));
    }

    #[test]
    fn special_preceding_token_forces_named_operator_or()
    {
        let tokenizer = Tokenizer::new("2oror");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Number(2.0),
                                               Token::Or,
                                               name_test("or")));
    }

    #[test]
    fn special_preceding_token_forces_named_operator_mod()
    {
        let tokenizer = Tokenizer::new("3moddiv");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Number(3.0),
                                               Token::Remainder,
                                               name_test("div")));
    }

    #[test]
    fn special_preceding_token_forces_named_operator_div()
    {
        let tokenizer = Tokenizer::new("1divz2");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Number(1.0),
                                               Token::Divide,
                                               name_test("z2")));
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
    fn tokenizes_node_test_without_args() {
        let tokenizer = Tokenizer::new("text()");

        assert_eq!(all_tokens(tokenizer), vec![Token::NodeTest(NodeTestName::Text)]);
    }

    #[test]
    fn tokenizes_node_test_with_args() {
        let tokenizer = Tokenizer::new("processing-instruction('hi')");

        assert_eq!(
            all_tokens(tokenizer),
            vec![Token::NodeTest(NodeTestName::ProcessingInstruction("hi".to_string()))]
        );
    }

    #[test]
    fn tokenizes_function_call() {
        let tokenizer = Tokenizer::new("hello()");

        assert_eq!(all_tokens(tokenizer), vec![Token::Function("hello".to_string()),
                                               Token::LeftParen,
                                               Token::RightParen]);
    }

    #[test]
    fn tokenizes_function_call_with_argument() {
        let tokenizer = Tokenizer::new("hello(1)");

        assert_eq!(all_tokens(tokenizer), vec![Token::Function("hello".to_string()),
                                               Token::LeftParen,
                                               Token::Number(1.0),
                                               Token::RightParen]);
    }

    #[test]
    fn tokenizes_function_call_with_multiple_arguments() {
        let tokenizer = Tokenizer::new("hello(1, 2)");

        assert_eq!(all_tokens(tokenizer), vec![Token::Function("hello".to_string()),
                                               Token::LeftParen,
                                               Token::Number(1.0),
                                               Token::Comma,
                                               Token::Number(2.0),
                                               Token::RightParen]);
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
    fn converts_at_sign_to_attribute_axis() {
        let input_tokens: Vec<TokenResult> = vec!(Ok(Token::AtSign));
        // let iter: &Iterator<TokenResult> = &input_tokens.into_iter();

        let deabbrv = TokenDeabbreviator::new(input_tokens.into_iter());
        // let a: () = deabbrv.next();
        // println!("{}",a );

        assert_eq!(all_tokens(deabbrv), vec![Token::Axis(AxisName::Attribute)]);
    }

    #[test]
    fn converts_double_slash_to_descendant_or_self() {
        let input_tokens: Vec<TokenResult> = vec!(Ok(Token::DoubleSlash));

        let deabbrv = TokenDeabbreviator::new(input_tokens.into_iter());

        assert_eq!(all_tokens(deabbrv), vec!(Token::Slash,
                                             Token::Axis(AxisName::DescendantOrSelf),
                                             Token::NodeTest(NodeTestName::Node),
                                             Token::Slash));
    }

    #[test]
    fn converts_current_node_to_self_node() {
        let input_tokens: Vec<TokenResult> = vec!(Ok(Token::CurrentNode));

        let deabbrv = TokenDeabbreviator::new(input_tokens.into_iter());

        assert_eq!(all_tokens(deabbrv), vec!(Token::Axis(AxisName::Self),
                                             Token::NodeTest(NodeTestName::Node)));
    }

    #[test]
    fn converts_parent_node_to_parent_node() {
        let input_tokens: Vec<TokenResult> = vec!(Ok(Token::ParentNode));

        let deabbrv = TokenDeabbreviator::new(input_tokens.into_iter());

        assert_eq!(all_tokens(deabbrv), vec!(Token::Axis(AxisName::Parent),
                                             Token::NodeTest(NodeTestName::Node)));
    }
}

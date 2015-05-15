use std::borrow::ToOwned;
use std::{error,fmt,string};

use peresil::{self,StringPoint,ParseMaster,Identifier,Recoverable};
use sxd_document::parser::XmlParseExt;

use super::node_test;
use super::token::{Token,AxisName,NodeTestName};
use super::token::Token::*;

use self::TokenizerErr::*;

pub struct Tokenizer {
    xpath: string::String,
    start: usize,
    prefer_recognition_of_operator_names: bool,
}

type XPathMaster<'a> = ParseMaster<StringPoint<'a>, TokenizerErr>;
type XPathProgress<'a, T, E> = peresil::Progress<StringPoint<'a>, T, E>;

pub type TokenResult = Result<Token, TokenizerErr>;

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum TokenizerErr {
    ExpectedQuote,
    ExpectedNumber,
    ExpectedCurrentNode,
    ExpectedNamedOperator,
    ExpectedAxis,
    ExpectedAxisSeparator,
    ExpectedNodeTest,
    ExpectedPrefixedName,
    ExpectedNameTest,
    ExpectedVariableReference,
    ExpectedToken,
    ExpectedLeftParenthesis,
    NotTokenizingNamedOperators,
    MismatchedQuoteCharacters,
    UnableToCreateToken,
}

impl error::Error for TokenizerErr {
    fn description(&self) -> &str {
        use self::TokenizerErr::*;
        match *self {
            ExpectedQuote =>
                "expected a single or double quote",
            ExpectedNumber =>
                "expected a number",
            ExpectedCurrentNode =>
                "Expected the current node token",
            ExpectedNamedOperator =>
                "expected a named operator",
            ExpectedAxis =>
                "expected an axis name",
            ExpectedAxisSeparator =>
                "expected an axis separator",
            ExpectedNodeTest =>
                "expected a node test",
            ExpectedPrefixedName =>
                "expected an optionally prefixed name",
            ExpectedNameTest =>
                "expected a name test",
            ExpectedVariableReference =>
                "expected a variable reference",
            ExpectedToken =>
                "expected a token",
            ExpectedLeftParenthesis =>
                "expected a left parenthesis",
            NotTokenizingNamedOperators =>
                "internal error",
            MismatchedQuoteCharacters =>
                "mismatched quote character",
            UnableToCreateToken =>
                "unable to create token",
        }
    }
}

impl fmt::Display for TokenizerErr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let as_err = self as &error::Error;
        as_err.description().fmt(fmt)
    }
}

impl Recoverable for TokenizerErr {
    fn recoverable(&self) -> bool {
        use self::TokenizerErr::*;
        match *self {
            MismatchedQuoteCharacters |
            UnableToCreateToken       => false,
            _ => true,
        }
    }
}

trait XPathParseExt<'a> {
    fn consume_quoted_string(&self, quote: &str) -> XPathProgress<'a, &'a str, ()>;
}

impl<'a> XPathParseExt<'a> for StringPoint<'a> {
    fn consume_quoted_string(&self, quote: &str) -> XPathProgress<'a, &'a str, ()> {
        let end_of_str = self.s.find(quote).or(Some(self.s.len()));
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
    ("self", AxisName::SelfAxis),
];


static NODE_TESTS: [Identifier<'static, NodeTestName>; 4] = [
    ("comment", NodeTestName::Comment),
    ("text", NodeTestName::Text),
    ("processing-instruction", NodeTestName::ProcessingInstruction(None)),
    ("node", NodeTestName::Node),
];

fn parse_literal<'a>(pm: &mut XPathMaster<'a>, p: StringPoint<'a>) -> XPathProgress<'a, &'a str, TokenizerErr> {
    fn with_quote<'a>(p: StringPoint<'a>, quote: &str)
                     -> XPathProgress<'a, &'a str, TokenizerErr>
    {
        let (p, _) = try_parse!(p.consume_literal(quote).map_err(|_| ExpectedQuote));
        let (p, v) = try_parse!(p.consume_quoted_string(quote).map_err(|_| unreachable!()));
        let (p, _) = try_parse!(p.consume_literal(quote).map_err(|_| MismatchedQuoteCharacters));

        peresil::Progress::success(p, v)
    }

    pm.alternate()
        .one(|_| with_quote(p, "\x22")) // "
        .one(|_| with_quote(p, "\x27")) // '
        .finish()
}

fn parse_quoted_literal<'a>(pm: &mut XPathMaster<'a>, p: StringPoint<'a>) -> XPathProgress<'a, Token, TokenizerErr> {
    parse_literal(pm, p).map(|v| Token::Literal(v.to_owned()))
}

fn parse_number<'a>(pm: &mut XPathMaster<'a>, p: StringPoint<'a>) -> XPathProgress<'a, Token, TokenizerErr> {
    fn fractional_part<'a>(p: StringPoint<'a>) -> XPathProgress<'a, (), ()> {
        let (p, _) = try_parse!(p.consume_literal("."));
        let (p, _) = p.consume_decimal_chars().optional(p);

        peresil::Progress::success(p, ())
    }

    fn with_integer<'a>(p: StringPoint<'a>) -> XPathProgress<'a, (), ()> {
        let (p, _) = try_parse!(p.consume_decimal_chars());
        let (p, _) = fractional_part(p).optional(p);

        peresil::Progress::success(p, ())
    }

    fn without_integer<'a>(p: StringPoint<'a>) -> XPathProgress<'a, (), ()> {
        let (p, _) = try_parse!(p.consume_literal("."));
        let (p, _) = try_parse!(p.consume_decimal_chars());

        peresil::Progress::success(p, ())
    }

    let before_p = p;

    let (p, _) = try_parse!({
        pm.alternate()
            .one(|_| with_integer(p).map_err(|_| ExpectedNumber))
            .one(|_| without_integer(p).map_err(|_| ExpectedNumber))
            .finish()
    });

    let num = before_p.to(p);
    let num = num.parse().unwrap();

    peresil::Progress::success(p, Token::Number(num))
}

fn parse_current_node<'a>(p: StringPoint<'a>) -> XPathProgress<'a, Token, TokenizerErr> {
    let (p, _) = try_parse!(p.consume_literal(".").map_err(|_| ExpectedCurrentNode));

    peresil::Progress::success(p, Token::CurrentNode)
}

fn parse_named_operators<'a>(p: StringPoint<'a>, prefer_named_ops: bool)
                          -> XPathProgress<'a, Token, TokenizerErr>
{
    if prefer_named_ops {
        p.consume_identifier(&NAMED_OPERATORS).map_err(|_| ExpectedNamedOperator)
    } else {
        // This is ugly, but we have to return some error
        peresil::Progress::failure(p, NotTokenizingNamedOperators)
    }
}

fn parse_axis_specifier<'a>(p: StringPoint<'a>) -> XPathProgress<'a, Token, TokenizerErr> {
    // Ideally, we would check for the pair of the name and the ::,
    // then loop. This would prevent us from having to order AXES.
    let (p, axis) = try_parse!(p.consume_identifier(&AXES).map_err(|_| ExpectedAxis));
    let (p, _) = try_parse!(p.consume_literal("::").map_err(|_| ExpectedAxisSeparator));

    peresil::Progress::success(p, Token::Axis(axis))
}

fn parse_node_type<'a>(pm: &mut XPathMaster<'a>, p: StringPoint<'a>) -> XPathProgress<'a, Token, TokenizerErr> {
    fn without_arg<'a>(p: StringPoint<'a>) -> XPathProgress<'a, Token, ()> {
        let (p, node_type) = try_parse!(p.consume_identifier(&NODE_TESTS));
        let (p, _) = try_parse!(p.consume_literal("()"));

        peresil::Progress::success(p, Token::NodeTest(node_type))
    }

    fn with_arg<'a>(pm: &mut XPathMaster<'a>, p: StringPoint<'a>) -> XPathProgress<'a, Token, ()> {
        let (p, _) = try_parse!(p.consume_literal("processing-instruction("));
        let (p, arg) = try_parse!(parse_literal(pm, p).map_err(|_| ()));
        let (p, _) = try_parse!(p.consume_literal(")"));

        let name = NodeTestName::ProcessingInstruction(Some(arg.to_owned()));
        peresil::Progress::success(p, Token::NodeTest(name))
    }

    pm.alternate()
        .one(|_| without_arg(p).map_err(|_| ExpectedNodeTest))
        .one(|pm| with_arg(pm, p).map_err(|_| ExpectedNodeTest))
        .finish()
}

fn parse_function_call<'a>(p: StringPoint<'a>) -> XPathProgress<'a, Token, TokenizerErr> {
    let (p, name) = try_parse!(p.consume_prefixed_name().map_err(|_| ExpectedPrefixedName));
    // Do not advance the point here. We want to know if there *is* a
    // left-paren, but do not want to actually consume it here.
    try_parse!(p.consume_literal("(").map_err(|_| ExpectedLeftParenthesis));

    // TODO: We should be using the prefix here!
    let name = name.local_part().to_owned();
    peresil::Progress::success(p, Token::Function(name))
}

fn parse_name_test<'a>(pm: &mut XPathMaster<'a>, p: StringPoint<'a>) -> XPathProgress<'a, Token, TokenizerErr> {
    fn wildcard<'a>(p: StringPoint<'a>) -> XPathProgress<'a, Token, ()> {
        let (p, wc) = try_parse!(p.consume_literal("*"));

        let name = node_test::NameTest {
            prefix: None,
            local_part: wc.to_owned(),
        };
        peresil::Progress::success(p, Token::NameTest(name))
    }

    fn prefixed_wildcard<'a>(p: StringPoint<'a>) -> XPathProgress<'a, Token, ()> {
        let (p, prefix) = try_parse!(p.consume_ncname());
        let (p, _) = try_parse!(p.consume_literal(":"));
        let (p, wc) = try_parse!(p.consume_literal("*"));

        let name = node_test::NameTest {
            prefix: Some(prefix.to_owned()),
            local_part: wc.to_owned(),
        };
        peresil::Progress::success(p, Token::NameTest(name))
    }

    fn prefixed_name<'a>(p: StringPoint<'a>) -> XPathProgress<'a, Token, ()> {
        p.consume_prefixed_name().map(|name| {
            Token::NameTest(node_test::NameTest {
                prefix: name.prefix().map(|p| p.to_owned()),
                local_part: name.local_part().to_owned()
            })
        })
    }

    pm.alternate()
        .one(|_| wildcard(p).map_err(|_| ExpectedNameTest))
        .one(|_| prefixed_wildcard(p).map_err(|_| ExpectedNameTest))
        .one(|_| prefixed_name(p).map_err(|_| ExpectedNameTest))
        .finish()
}

fn parse_variable_reference<'a>(p: StringPoint<'a>) -> XPathProgress<'a, Token, TokenizerErr> {
    let (p, _) = try_parse!(p.consume_literal("$").map_err(|_| ExpectedVariableReference));
    let (p, name) = try_parse!(p.consume_prefixed_name().map_err(|_| ExpectedPrefixedName));

    // TODO: We should be using the prefix here!
    let name = name.local_part().to_owned();
    peresil::Progress::success(p, Token::Variable(name))
}

impl Tokenizer {
    pub fn new(xpath: &str) -> Tokenizer {
        Tokenizer {
            xpath: xpath.to_owned(),
            start: 0,
            prefer_recognition_of_operator_names: false,
        }
    }

    pub fn has_more_tokens(&self) -> bool {
        self.xpath.len() > self.start
    }

    fn parse_token<'a>(&self, pm: &mut XPathMaster<'a>, p: StringPoint<'a>) -> XPathProgress<'a, Token, TokenizerErr> {
        let (p, _) = p.consume_space().optional(p);

        let (p, tok) = try_parse!({
            pm.alternate()
                .one(|_| p.consume_identifier(&TWO_CHAR_TOKENS).map_err(|_| ExpectedToken))
                .one(|_| p.consume_identifier(&SINGLE_CHAR_TOKENS).map_err(|_| ExpectedToken))
                .one(|pm| parse_quoted_literal(pm, p))
                .one(|pm| parse_number(pm, p))
                .one(|_| parse_current_node(p))
                .one(|_| parse_named_operators(p, self.prefer_recognition_of_operator_names))
                .one(|_| parse_axis_specifier(p))
                .one(|pm| parse_node_type(pm, p))
                .one(|_| parse_function_call(p))
                .one(|pm| parse_name_test(pm, p))
                .one(|_| parse_variable_reference(p))
                .finish()
        });

        let (p, _) = p.consume_space().optional(p);

        peresil::Progress::success(p, tok)
    }

    fn raw_next_token(&mut self) -> TokenResult {
        let mut pm = ParseMaster::new();
        let p = StringPoint { s: &self.xpath[self.start..], offset: self.start };

        let r = self.parse_token(&mut pm, p);
        match pm.finish(r) {
            peresil::Progress { status: peresil::Status::Success(data), point } => {
                self.start = point.offset;
                Ok(data)
            },
            peresil::Progress { status: peresil::Status::Failure(mut e), point } => {
                if point.offset == self.start {
                    Err(UnableToCreateToken)
                } else {
                    Err(e.pop().unwrap())
                }
            },
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
                self.push(Token::Axis(AxisName::SelfAxis));
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
    use std::borrow::ToOwned;

    use super::super::node_test;
    use super::super::token::{Token,AxisName,NodeTestName};

    use super::{Tokenizer,TokenDeabbreviator,TokenResult,TokenizerErr};
    use super::TokenizerErr::{
        MismatchedQuoteCharacters,
        UnableToCreateToken,
    };

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
            local_part: local_part.to_owned()
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
            prefix: Some("ns".to_owned()),
            local_part: "foo".to_owned()
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

        assert_eq!(all_tokens(tokenizer), vec!(Token::Literal("hello!".to_owned())));
    }

    #[test]
    fn tokenizes_double_quote_literal()
    {
        let tokenizer = Tokenizer::new("\"1.23\"");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Literal("1.23".to_owned())));
    }

    #[test]
    fn tokenizes_variable_reference()
    {
        let tokenizer = Tokenizer::new("$yo");

        assert_eq!(all_tokens(tokenizer), vec!(Token::Variable("yo".to_owned())));
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
    fn tokenizes_processing_instruction_node_test_without_args() {
        let tokenizer = Tokenizer::new("processing-instruction()");

        assert_eq!(
            all_tokens(tokenizer),
            vec![Token::NodeTest(NodeTestName::ProcessingInstruction(None))]
        );
    }

    #[test]
    fn tokenizes_processing_instruction_node_test_with_args() {
        let tokenizer = Tokenizer::new("processing-instruction('hi')");

        assert_eq!(
            all_tokens(tokenizer),
            vec![Token::NodeTest(NodeTestName::ProcessingInstruction(Some("hi".to_owned())))]
        );
    }

    #[test]
    fn tokenizes_function_call() {
        let tokenizer = Tokenizer::new("hello()");

        assert_eq!(all_tokens(tokenizer), vec![Token::Function("hello".to_owned()),
                                               Token::LeftParen,
                                               Token::RightParen]);
    }

    #[test]
    fn tokenizes_function_call_with_argument() {
        let tokenizer = Tokenizer::new("hello(1)");

        assert_eq!(all_tokens(tokenizer), vec![Token::Function("hello".to_owned()),
                                               Token::LeftParen,
                                               Token::Number(1.0),
                                               Token::RightParen]);
    }

    #[test]
    fn tokenizes_function_call_with_multiple_arguments() {
        let tokenizer = Tokenizer::new("hello(1, 2)");

        assert_eq!(all_tokens(tokenizer), vec![Token::Function("hello".to_owned()),
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

        let deabbrv = TokenDeabbreviator::new(input_tokens.into_iter());

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

        assert_eq!(all_tokens(deabbrv), vec!(Token::Axis(AxisName::SelfAxis),
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

extern crate xpath;

use xpath::token;
use xpath::token::XPathToken;

use xpath::tokenizer::XPathTokenizer;
use xpath::tokenizer::{TokenResult,TokenizerErr};
use xpath::tokenizer::TokenizerErr::{
    MismatchedQuoteCharacters,
    MissingLocalName,
    UnableToCreateToken,
};

use xpath::tokenizer::XPathTokenDisambiguator;
use xpath::tokenizer::XPathTokenDeabbreviator;

fn is_finished(tokenizer: & XPathTokenizer) -> bool {
    ! tokenizer.has_more_tokens()
}

fn all_tokens_raw<I: Iterator<TokenResult>>(mut tokenizer: I) -> Result<Vec<XPathToken>, TokenizerErr> {
    tokenizer.collect()
}

fn all_tokens<I: Iterator<TokenResult>>(tokenizer: I) -> Vec<XPathToken> {
    match all_tokens_raw(tokenizer) {
        Ok(toks) => toks,
        Err(msg) => panic!(msg),
    }
}

#[test]
fn empty_string_has_no_tokens()
{
    let tokenizer = xpath::tokenizer::XPathTokenizer::new("");
    assert!(is_finished(&tokenizer));
}

#[test]
fn tokenizes_simple_string()
{
    let tokenizer = XPathTokenizer::new("hello");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::String("hello".to_string())));
}

#[test]
fn tokenizes_grandchild_selector()
{
    let tokenizer = XPathTokenizer::new("hello/world");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::String("hello".to_string()),
                                           XPathToken::Slash,
                                           XPathToken::String("world".to_string())));
}

#[test]
fn tokenizes_great_grandchild_selector()
{
    let tokenizer = XPathTokenizer::new("hello/there/world");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::String("hello".to_string()),
                                           XPathToken::Slash,
                                           XPathToken::String("there".to_string()),
                                           XPathToken::Slash,
                                           XPathToken::String("world".to_string())));
}

#[test]
fn tokenizes_qualified_names()
{
    let tokenizer = XPathTokenizer::new("ns:foo");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::PrefixedName("ns".to_string(), "foo".to_string())));
}

#[test]
fn ignores_whitespace_around_tokens()
{
    let tokenizer = XPathTokenizer::new(" @\t@\n@\r");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::AtSign,
                                           XPathToken::AtSign,
                                           XPathToken::AtSign));
}

#[test]
fn tokenizes_wildcard_name_test()
{
    let tokenizer = XPathTokenizer::new("*");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::String("*".to_string())));
}

#[test]
fn tokenizes_axis_separator()
{
    let tokenizer = XPathTokenizer::new("::");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::DoubleColon));
}

#[test]
fn tokenizes_axis_selector()
{
    let tokenizer = XPathTokenizer::new("hello::world");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::String("hello".to_string()),
                                           XPathToken::DoubleColon,
                                           XPathToken::String("world".to_string())));
}

#[test]
fn tokenizes_single_slash()
{
    let tokenizer = XPathTokenizer::new("/");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Slash));
}

#[test]
fn tokenizes_double_slash()
{
    let tokenizer = XPathTokenizer::new("//");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::DoubleSlash));
}

#[test]
fn tokenizes_double_slash_separator()
{
    let tokenizer = XPathTokenizer::new("hello//world");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::String("hello".to_string()),
                                           XPathToken::DoubleSlash,
                                           XPathToken::String("world".to_string())));
}

#[test]
fn tokenizes_left_paren()
{
    let tokenizer = XPathTokenizer::new("(");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::LeftParen));
}

#[test]
fn tokenizes_right_paren()
{
    let tokenizer = XPathTokenizer::new(")");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::RightParen));
}

#[test]
fn tokenizes_at_sign()
{
    let tokenizer = XPathTokenizer::new("@");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::AtSign));
}

#[test]
fn tokenizes_single_dot()
{
    let tokenizer = XPathTokenizer::new(".");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::CurrentNode));
}

#[test]
fn tokenizes_double_dot()
{
    let tokenizer = XPathTokenizer::new("..");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::ParentNode));
}

#[test]
fn tokenizes_integral_number()
{
    let tokenizer = XPathTokenizer::new("42");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Number(42.0)));
}

#[test]
fn tokenizes_decimal_number()
{
    let tokenizer = XPathTokenizer::new("42.42");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Number(42.42)));
}

#[test]
fn tokenizes_decimal_number_without_integral_part()
{
    let tokenizer = XPathTokenizer::new(".40");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Number(0.40)));
}

#[test]
fn tokenizes_left_bracket()
{
    let tokenizer = XPathTokenizer::new("[");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::LeftBracket));
}

#[test]
fn tokenizes_right_bracket()
{
    let tokenizer = XPathTokenizer::new("]");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::RightBracket));
}

#[test]
fn tokenizes_apostrophe_literal()
{
    let tokenizer = XPathTokenizer::new("'hello!'");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Literal("hello!".to_string())));
}

#[test]
fn tokenizes_double_quote_literal()
{
    let tokenizer = XPathTokenizer::new("\"1.23\"");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Literal("1.23".to_string())));
}

#[test]
fn tokenizes_dollar_sign()
{
    let tokenizer = XPathTokenizer::new("$");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::DollarSign));
}

#[test]
fn tokenizes_plus_sign()
{
    let tokenizer = XPathTokenizer::new("+");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::PlusSign));
}

#[test]
fn tokenizes_minus_sign()
{
    let tokenizer = XPathTokenizer::new("-");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::MinusSign));
}

#[test]
fn tokenizes_pipe()
{
    let tokenizer = XPathTokenizer::new("|");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Pipe));
}

#[test]
fn tokenizes_equal_sign()
{
    let tokenizer = XPathTokenizer::new("=");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Equal));
}

#[test]
fn tokenizes_not_equal_sign()
{
    let tokenizer = XPathTokenizer::new("!=");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::NotEqual));
}

#[test]
fn tokenizes_less_than()
{
    let tokenizer = XPathTokenizer::new("<");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::LessThan));
}

#[test]
fn tokenizes_less_than_or_equal()
{
    let tokenizer = XPathTokenizer::new("<=");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::LessThanOrEqual));
}

#[test]
fn tokenizes_greater_than()
{
    let tokenizer = XPathTokenizer::new(">");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::GreaterThan));
}

#[test]
fn tokenizes_greater_than_or_equal()
{
    let tokenizer = XPathTokenizer::new(">=");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::GreaterThanOrEqual));
}

#[test]
fn special_preceding_token_forces_named_operator_and()
{
    let tokenizer = XPathTokenizer::new("1andz2");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Number(1.0),
                                           XPathToken::And,
                                           XPathToken::String("z2".to_string())));
}

#[test]
fn special_preceding_token_forces_named_operator_or()
{
    let tokenizer = XPathTokenizer::new("2oror");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Number(2.0),
                                           XPathToken::Or,
                                           XPathToken::String("or".to_string())));
}

#[test]
fn special_preceding_token_forces_named_operator_mod()
{
    let tokenizer = XPathTokenizer::new("3moddiv");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Number(3.0),
                                           XPathToken::Remainder,
                                           XPathToken::String("div".to_string())));
}

#[test]
fn special_preceding_token_forces_named_operator_div()
{
    let tokenizer = XPathTokenizer::new("1divz2");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Number(1.0),
                                           XPathToken::Divide,
                                           XPathToken::String("z2".to_string())));
}

#[test]
fn special_preceding_token_forces_named_operator_multiply()
{
    let tokenizer = XPathTokenizer::new("1*2");

    assert_eq!(all_tokens(tokenizer), vec!(XPathToken::Number(1.0),
                                           XPathToken::Multiply,
                                           XPathToken::Number(2.0)));
}

#[test]
fn exception_thrown_when_nothing_was_tokenized()
{
    let tokenizer = XPathTokenizer::new("!");
    let res = all_tokens_raw(tokenizer);

    assert_eq!(Err(UnableToCreateToken), res);
}

#[test]
fn exception_thrown_when_name_test_has_no_local_name()
{
    let tokenizer = XPathTokenizer::new("ns:");
    let res = all_tokens_raw(tokenizer);

    assert_eq!(Err(MissingLocalName), res);
}

#[test]
fn exception_thrown_when_quote_characters_mismatched()
{
    let tokenizer = XPathTokenizer::new("'hello\"");
    let res = all_tokens_raw(tokenizer);

    assert_eq!(Err(MismatchedQuoteCharacters), res);
}

#[test]
fn disambiguates_node_test_functions() {
    // Would prefer parametric tests
    for name in ["comment", "text", "processing-instruction", "node"].iter() {
        let input_tokens: Vec<TokenResult> = vec!(
            Ok(XPathToken::String(name.to_string())),
            Ok(XPathToken::LeftParen),
        );

        let disambig = XPathTokenDisambiguator::new(input_tokens.into_iter());

        assert_eq!(all_tokens(disambig),
                   vec!(XPathToken::NodeTest(name.to_string()),
                        XPathToken::LeftParen));
    }
}

#[test]
fn name_followed_by_left_paren_becomes_function_name() {
    let input_tokens: Vec<TokenResult> = vec!(
        Ok(XPathToken::String("test".to_string())),
        Ok(XPathToken::LeftParen),
     );

    let disambig = XPathTokenDisambiguator::new(input_tokens.into_iter());

    assert_eq!(all_tokens(disambig),
               vec!(XPathToken::Function("test".to_string()),
                    XPathToken::LeftParen));
}

#[test]
fn name_followed_by_double_colon_becomes_axis_name() {
    let input_tokens: Vec<TokenResult> = vec!(
        Ok(XPathToken::String("test".to_string())),
        Ok(XPathToken::DoubleColon),
    );

    let disambig = XPathTokenDisambiguator::new(input_tokens.into_iter());

    assert_eq!(all_tokens(disambig),
               vec!(XPathToken::Axis("test".to_string()),
                    XPathToken::DoubleColon));
}

#[test]
fn converts_at_sign_to_attribute_axis() {
    let input_tokens: Vec<TokenResult> = vec!(Ok(XPathToken::AtSign));
    // let iter: &Iterator<TokenResult> = &input_tokens.into_iter();

    let deabbrv = XPathTokenDeabbreviator::new(input_tokens.into_iter());
    // let a: () = deabbrv.next();
    // println!("{}",a );

    assert_eq!(all_tokens(deabbrv), vec!(XPathToken::String("attribute".to_string()),
                                         XPathToken::DoubleColon));
}

#[test]
fn converts_double_slash_to_descendant_or_self() {
    let input_tokens: Vec<TokenResult> = vec!(Ok(XPathToken::DoubleSlash));

    let deabbrv = XPathTokenDeabbreviator::new(input_tokens.into_iter());

    assert_eq!(all_tokens(deabbrv), vec!(XPathToken::Slash,
                                         XPathToken::String("descendant-or-self".to_string()),
                                         XPathToken::DoubleColon,
                                         XPathToken::String("node".to_string()),
                                         XPathToken::LeftParen,
                                         XPathToken::RightParen,
                                         XPathToken::Slash));
}

#[test]
fn converts_current_node_to_self_node() {
    let input_tokens: Vec<TokenResult> = vec!(Ok(XPathToken::CurrentNode));

    let deabbrv = XPathTokenDeabbreviator::new(input_tokens.into_iter());

    assert_eq!(all_tokens(deabbrv), vec!(XPathToken::String("self".to_string()),
                                         XPathToken::DoubleColon,
                                         XPathToken::String("node".to_string()),
                                         XPathToken::LeftParen,
                                         XPathToken::RightParen));
}

#[test]
fn converts_parent_node_to_parent_node() {
    let input_tokens: Vec<TokenResult> = vec!(Ok(XPathToken::ParentNode));

    let deabbrv = XPathTokenDeabbreviator::new(input_tokens.into_iter());

    assert_eq!(all_tokens(deabbrv), vec!(XPathToken::String("parent".to_string()),
                                         XPathToken::DoubleColon,
                                         XPathToken::String("node".to_string()),
                                         XPathToken::LeftParen,
                                         XPathToken::RightParen));
}

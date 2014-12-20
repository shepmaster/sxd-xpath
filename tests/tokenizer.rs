extern crate xpath;

use xpath::token::Token;

use xpath::tokenizer::Tokenizer;
use xpath::tokenizer::{TokenResult,TokenizerErr};
use xpath::tokenizer::TokenizerErr::{
    MismatchedQuoteCharacters,
    MissingLocalName,
    UnableToCreateToken,
};

use xpath::tokenizer::TokenDisambiguator;
use xpath::tokenizer::TokenDeabbreviator;

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
    let tokenizer = xpath::tokenizer::Tokenizer::new("");
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

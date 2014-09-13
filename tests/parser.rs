#![feature(phase)]
#![feature(macro_rules)]

#[phase(plugin, link)]
extern crate document;
extern crate xpath;

use std::collections::hashmap::HashMap;

use document::{Document,Element,Attribute,Text,ToAny};

use xpath::{Boolean,Number,String,Nodes};
use xpath::{Functions,Variables};
use xpath::{XPathValue,XPathEvaluationContext};

use xpath::token;
use xpath::tokenizer::TokenResult;

use xpath::expression::{XPathExpression,SubExpression};

use xpath::parser::{XPathParser,ParseResult};
use xpath::parser::{
    EmptyPredicate,
    ExtraUnparsedTokens,
    InvalidNodeTest,
    InvalidXPathAxis,
    RanOutOfInput,
    RightHandSideExpressionMissing,
    TokenizerError,
    TrailingSlash,
    UnexpectedToken,
};

macro_rules! tokens(
    ($($e:expr),*) => ({
        // leading _ to allow empty construction without a warning.
        let mut _temp: Vec<TokenResult> = ::std::vec::Vec::new();
        $(_temp.push(Ok($e));)*
        _temp
    });
    ($($e:expr),+,) => (tokens!($($e),+))
)

trait ApproxEq {
    fn is_approx_eq(&self, other: &Self) -> bool;
}

impl ApproxEq for f64 {
    fn is_approx_eq(&self, other: &f64) -> bool {
        (*self - *other).abs() < 1.0e-6
    }
}

impl ApproxEq for XPathValue {
    fn is_approx_eq(&self, other: &XPathValue) -> bool {
        match (self, other) {
            (&Number(ref x), &Number(ref y)) => x.is_approx_eq(y),
            _ => fail!("It's nonsensical to compare these quantities"),
        }
    }
}

macro_rules! assert_approx_eq(
    ($a:expr, $b:expr) => ({
        let (a, b) = (&$a, &$b);
        assert!(a.is_approx_eq(b),
                "{} is not approximately equal to {}", *a, *b);
    })
)

struct Setup {
    doc: Document,
    top_node: Element,
    functions: Functions,
    variables: Variables,
    parser: XPathParser,
}

impl Setup {
    fn new() -> Setup {
        let d = Document::new();
        let e = d.new_element("the-top-node".to_string());
        d.root().append_child(e.clone());

        let mut functions = HashMap::new();
        xpath::function::register_core_functions(& mut functions);

        Setup {
            doc: d,
            top_node: e,
            functions: functions,
            variables: HashMap::new(),
            parser: XPathParser::new(),
        }
    }

    fn add_child(&self, parent: &Element, name: &str) -> Element {
        let n = self.doc.new_element(name.to_string());
        parent.append_child(n.clone());
        n
    }

    fn add_attribute(&self, element: Element, name: &str, value: &str) -> Attribute {
        element.set_attribute(name.to_string(), value.to_string())
    }

    fn add_text(&self, parent: Element, value: &str) -> Text {
        let tn = self.doc.new_text(value.to_string());
        parent.append_child(tn.clone());
        tn
    }

    fn add_var(&mut self, name: &str, value: XPathValue) {
        self.variables.insert(name.to_string(), value);
    }

    fn parse_raw(&self, tokens: Vec<TokenResult>) -> ParseResult {
        self.parser.parse(tokens.move_iter())
    }

    fn parse(&self, tokens: Vec<TokenResult>) -> SubExpression {
        self.parse_raw(tokens).unwrap().unwrap()
    }

    fn evaluate(&self, expr: &XPathExpression) -> XPathValue {
        self.evaluate_on(expr, self.top_node.clone())
    }

    fn evaluate_on<A : ToAny>(&self, expr: &XPathExpression, node: A) -> XPathValue {
        let mut context = XPathEvaluationContext::new(node.to_any(),
                                                      &self.functions,
                                                      &self.variables);
        context.next(node.to_any());
        expr.evaluate(&context)
    }
}

#[test]
fn parses_string_as_child() {
    let setup = Setup::new();
    let tokens = tokens![token::String("hello".to_string())];

    let expr = setup.parse(tokens);

    let hello = setup.add_child(&setup.top_node, "hello");

    assert_eq!(Nodes(nodeset![hello]), setup.evaluate_on(&*expr, setup.top_node.clone()));
}

#[test]
fn parses_two_strings_as_grandchild() {
    let setup = Setup::new();
    let tokens = tokens![
        token::String("hello".to_string()),
        token::Slash,
        token::String("world".to_string())
    ];

    let expr = setup.parse(tokens);

    let hello = setup.add_child(&setup.top_node, "hello");
    let world = setup.add_child(&hello, "world");

    assert_eq!(Nodes(nodeset![world]), setup.evaluate_on(&*expr, setup.top_node.clone()));
}

#[test]
fn parses_self_axis() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Axis("self".to_string()),
        token::DoubleColon,
        token::String("the-top-node".to_string())
    ];

    let expr = setup.parse(tokens);

    assert_eq!(Nodes(nodeset![setup.top_node.clone()]), setup.evaluate_on(&*expr, setup.top_node.clone()));
}

#[test]
fn parses_parent_axis() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Axis("parent".to_string()),
        token::DoubleColon,
        token::String("the-top-node".to_string())
    ];

    let expr = setup.parse(tokens);

    let hello = setup.add_child(&setup.top_node, "hello");
    assert_eq!(Nodes(nodeset![setup.top_node.clone()]), setup.evaluate_on(&*expr, hello));
}

#[test]
fn parses_descendant_axis() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Axis("descendant".to_string()),
        token::DoubleColon,
        token::String("two".to_string())
    ];

    let expr = setup.parse(tokens);

    let one = setup.add_child(&setup.top_node, "one");
    let two = setup.add_child(&one, "two");

    assert_eq!(Nodes(nodeset![two]), setup.evaluate_on(&*expr, setup.top_node.clone()));
}

#[test]
fn parses_descendant_or_self_axis() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Axis("descendant-or-self".to_string()),
        token::DoubleColon,
        token::String("*".to_string())
    ];

    let expr = setup.parse(tokens);

    let one = setup.add_child(&setup.top_node, "one");
    let two = setup.add_child(&one, "two");

    assert_eq!(Nodes(nodeset![one.clone(), two]), setup.evaluate_on(&*expr, one));
}

#[test]
fn parses_attribute_axis() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Axis("attribute".to_string()),
        token::DoubleColon,
        token::String("*".to_string())
    ];

    let expr = setup.parse(tokens);

    let one = setup.add_child(&setup.top_node, "one");
    let attr = setup.add_attribute(one.clone(), "hello", "world");

    assert_eq!(Nodes(nodeset![attr]), setup.evaluate_on(&*expr, one));
}

#[test]
fn parses_child_with_same_name_as_an_axis() {
    let setup = Setup::new();
    let tokens = tokens![token::String("self".to_string())];

    let expr = setup.parse(tokens);

    let element = setup.add_child(&setup.top_node, "self");
    assert_eq!(Nodes(nodeset![element]), setup.evaluate_on(&*expr, setup.top_node.clone()));
}

#[test]
fn parses_node_node_test() {
    let setup = Setup::new();
    let tokens = tokens![
        token::NodeTest("node".to_string()),
        token::LeftParen,
        token::RightParen
    ];

    let expr = setup.parse(tokens);

    let one = setup.add_child(&setup.top_node, "one");
    let two = setup.add_child(&one, "two");

    assert_eq!(Nodes(nodeset![two]), setup.evaluate_on(&*expr, one));
}

#[test]
fn parses_text_node_test() {
    let setup = Setup::new();
    let tokens = tokens![
        token::NodeTest("text".to_string()),
        token::LeftParen,
        token::RightParen
    ];

    let expr = setup.parse(tokens);

    let one = setup.add_child(&setup.top_node, "one");
    let text = setup.add_text(one.clone(), "text");

    assert_eq!(Nodes(nodeset![text]), setup.evaluate_on(&*expr, one));
}

#[test]
fn parses_axis_and_node_test() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Axis("self".to_string()),
        token::DoubleColon,
        token::NodeTest("text".to_string()),
        token::LeftParen,
        token::RightParen
    ];

    let expr = setup.parse(tokens);

    let one = setup.add_child(&setup.top_node, "one");
    let text = setup.add_text(one, "text");

    assert_eq!(Nodes(nodeset![text.clone()]), setup.evaluate_on(&*expr, text));
}

#[test]
fn numeric_predicate_selects_indexed_node() {
    let setup = Setup::new();
    let tokens = tokens![
        token::String("*".to_string()),
        token::LeftBracket,
        token::Number(2.0),
        token::RightBracket
    ];

    let expr = setup.parse(tokens);

    setup.add_child(&setup.top_node, "first");
    let second = setup.add_child(&setup.top_node, "second");

    assert_eq!(Nodes(nodeset![second]), setup.evaluate_on(&*expr, setup.top_node.clone()));
}

#[test]
fn string_literal() {
    let setup = Setup::new();
    let tokens = tokens![token::Literal("string".to_string())];

    let expr = setup.parse(tokens);

    assert_eq!(String("string".to_string()), setup.evaluate(&*expr));
}

#[test]
fn predicate_accepts_any_expression() {
    let setup = Setup::new();
    let tokens = tokens![
        token::String("*".to_string()),
        token::LeftBracket,
        token::Function("true".to_string()),
        token::LeftParen,
        token::RightParen,
        token::Or,
        token::Function("false".to_string()),
        token::LeftParen,
        token::RightParen,
        token::RightBracket
    ];

    let expr = setup.parse(tokens);

    let first = setup.add_child(&setup.top_node, "first");
    let second = setup.add_child(&setup.top_node, "second");

    assert_eq!(Nodes(nodeset![first, second]), setup.evaluate_on(&*expr, setup.top_node.clone()));
}

#[test]
fn true_function_predicate_selects_all_nodes() {
    let setup = Setup::new();
    let tokens = tokens![
        token::String("*".to_string()),
        token::LeftBracket,
        token::Function("true".to_string()),
        token::LeftParen,
        token::RightParen,
        token::RightBracket
    ];

    let expr = setup.parse(tokens);

    let first = setup.add_child(&setup.top_node, "first");
    let second = setup.add_child(&setup.top_node, "second");

    assert_eq!(Nodes(nodeset![first, second]), setup.evaluate_on(&*expr, setup.top_node.clone()));
}

#[test]
fn false_function_predicate_selects_no_nodes() {
    let setup = Setup::new();
    let tokens = tokens![
        token::String("*".to_string()),
        token::LeftBracket,
        token::Function("false".to_string()),
        token::LeftParen,
        token::RightParen,
        token::RightBracket
    ];

    let expr = setup.parse(tokens);

    setup.add_child(&setup.top_node, "first");
    setup.add_child(&setup.top_node, "second");

    assert_eq!(Nodes(nodeset![]), setup.evaluate_on(&*expr, setup.top_node.clone()));
}

#[test]
fn multiple_predicates() {
    let setup = Setup::new();
    let tokens = tokens![
        token::String("*".to_string()),
        token::LeftBracket,
        token::Number(2.0),
        token::RightBracket,
        token::LeftBracket,
        token::Number(1.0),
        token::RightBracket
    ];

    let expr = setup.parse(tokens);

    setup.add_child(&setup.top_node, "first");
    let second = setup.add_child(&setup.top_node, "second");

    assert_eq!(Nodes(nodeset![second]), setup.evaluate_on(&*expr, setup.top_node.clone()));
}

#[test]
fn functions_accept_arguments() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Function("not".to_string()),
        token::LeftParen,
        token::Function("true".to_string()),
        token::LeftParen,
        token::RightParen,
        token::RightParen,
    ];

    let expr = setup.parse(tokens);

    assert_eq!(Boolean(false), setup.evaluate(&*expr));
}

#[test]
fn functions_accept_any_expression_as_an_argument() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Function("not".to_string()),
        token::LeftParen,
        token::Function("true".to_string()),
        token::LeftParen,
        token::RightParen,
        token::Or,
        token::Function("false".to_string()),
        token::LeftParen,
        token::RightParen,
        token::RightParen,
    ];

    let expr = setup.parse(tokens);

    assert_eq!(Boolean(false), setup.evaluate(&*expr));
}

#[test]
fn numeric_literal() {
    let setup = Setup::new();
    let tokens = tokens![token::Number(3.2)];

    let expr = setup.parse(tokens);

    assert_approx_eq!(Number(3.2), setup.evaluate(&*expr));
}

#[test]
fn addition_of_two_numbers() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(1.1),
        token::PlusSign,
        token::Number(2.2)
    ];

    let expr = setup.parse(tokens);

    assert_approx_eq!(Number(3.3), setup.evaluate(&*expr));
}

#[test]
fn addition_of_multiple_numbers() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(1.1),
        token::PlusSign,
        token::Number(2.2),
        token::PlusSign,
        token::Number(3.3)
    ];

    let expr = setup.parse(tokens);

    assert_approx_eq!(Number(6.6), setup.evaluate(&*expr));
}

#[test]
fn subtraction_of_two_numbers() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(1.1),
        token::MinusSign,
        token::Number(2.2),
    ];

    let expr = setup.parse(tokens);

    assert_approx_eq!(Number(-1.1), setup.evaluate(&*expr));
}

#[test]
fn additive_expression_is_left_associative() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(1.1),
        token::MinusSign,
        token::Number(2.2),
        token::MinusSign,
        token::Number(3.3),
    ];

    let expr = setup.parse(tokens);

    assert_approx_eq!(Number(-4.4), setup.evaluate(&*expr));
}

#[test]
fn multiplication_of_two_numbers() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(1.1),
        token::Multiply,
        token::Number(2.2),
    ];

    let expr = setup.parse(tokens);

    assert_approx_eq!(Number(2.42), setup.evaluate(&*expr));
}

#[test]
fn division_of_two_numbers() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(7.1),
        token::Divide,
        token::Number(0.1),
    ];

    let expr = setup.parse(tokens);

    assert_approx_eq!(Number(71.0), setup.evaluate(&*expr));
}

#[test]
fn remainder_of_two_numbers() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(7.1),
        token::Remainder,
        token::Number(3.0),
    ];

    let expr = setup.parse(tokens);

    assert_approx_eq!(Number(1.1), setup.evaluate(&*expr));
}

#[test]
fn unary_negation() {
    let setup = Setup::new();
    let tokens = tokens![
        token::MinusSign,
        token::Number(7.2),
    ];

    let expr = setup.parse(tokens);

    assert_approx_eq!(Number(-7.2), setup.evaluate(&*expr));
}

#[test]
fn repeated_unary_negation() {
    let setup = Setup::new();
    let tokens = tokens![
        token::MinusSign,
        token::MinusSign,
        token::MinusSign,
        token::Number(7.2),
    ];

    let expr = setup.parse(tokens);

    assert_approx_eq!(Number(-7.2), setup.evaluate(&*expr));
}

#[test]
fn top_level_function_call() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Function("true".to_string()),
        token::LeftParen,
        token::RightParen,
    ];

    let expr = setup.parse(tokens);

    assert_eq!(Boolean(true), setup.evaluate(&*expr));
}

#[test]
fn or_expression() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Function("true".to_string()),
        token::LeftParen,
        token::RightParen,
        token::Or,
        token::Function("false".to_string()),
        token::LeftParen,
        token::RightParen,
    ];

    let expr = setup.parse(tokens);

    assert_eq!(Boolean(true), setup.evaluate(&*expr));
}

#[test]
fn and_expression() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(1.2),
        token::And,
        token::Number(0.0),
    ];

    let expr = setup.parse(tokens);

    assert_eq!(Boolean(false), setup.evaluate(&*expr));
}

#[test]
fn equality_expression() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(1.2),
        token::Equal,
        token::Number(1.1),
    ];

    let expr = setup.parse(tokens);

    assert_eq!(Boolean(false), setup.evaluate(&*expr));
}

#[test]
fn inequality_expression() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(1.2),
        token::NotEqual,
        token::Number(1.2),
    ];

    let expr = setup.parse(tokens);

    assert_eq!(Boolean(false), setup.evaluate(&*expr));
}

#[test]
fn less_than_expression() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(1.2),
        token::LessThan,
        token::Number(1.2),
    ];

    let expr = setup.parse(tokens);

    assert_eq!(Boolean(false), setup.evaluate(&*expr));
}

#[test]
fn less_than_or_equal_expression() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(1.2),
        token::LessThanOrEqual,
        token::Number(1.2),
    ];

    let expr = setup.parse(tokens);

    assert_eq!(Boolean(true), setup.evaluate(&*expr));
}

#[test]
fn greater_than_expression() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(1.2),
        token::GreaterThan,
        token::Number(1.2),
    ];

    let expr = setup.parse(tokens);

    assert_eq!(Boolean(false), setup.evaluate(&*expr));
}

#[test]
fn greater_than_or_equal_expression() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Number(1.2),
        token::GreaterThanOrEqual,
        token::Number(1.2),
    ];

    let expr = setup.parse(tokens);

    assert_eq!(Boolean(true), setup.evaluate(&*expr));
}

#[test]
fn variable_reference() {
    let mut setup = Setup::new();
    let tokens = tokens![
        token::DollarSign,
        token::String("variable-name".to_string()),
    ];

    setup.add_var("variable-name", Number(12.3));
    let expr = setup.parse(tokens);

    assert_approx_eq!(Number(12.3), setup.evaluate(&*expr));
}

#[test]
fn filter_expression() {
    let mut setup = Setup::new();
    let tokens = tokens![
        token::DollarSign,
        token::String("variable".to_string()),
        token::LeftBracket,
        token::Number(0.0),
        token::RightBracket,
    ];

    let value = nodeset![
        setup.add_child(&setup.top_node, "first-node"),
        setup.add_child(&setup.top_node, "second-node"),
    ];
    setup.add_var("variable", Nodes(value));

    let expr = setup.parse(tokens);

    assert_eq!(Nodes(nodeset![]), setup.evaluate(&*expr));
}

#[test]
fn filter_expression_and_relative_path() {
    let mut setup = Setup::new();
    let tokens = tokens![
        token::DollarSign,
        token::String("variable".to_string()),
        token::Slash,
        token::String("child".to_string()),
    ];

    let parent = setup.add_child(&setup.top_node, "parent");
    let child = setup.add_child(&parent, "child");

    let value = nodeset![parent];
    setup.add_var("variable", Nodes(value));

    let expr = setup.parse(tokens);

    assert_eq!(Nodes(nodeset![child]), setup.evaluate(&*expr));
}

#[test]
fn union_expression() {
    let mut setup = Setup::new();
    let tokens = tokens![
        token::DollarSign,
        token::String("variable1".to_string()),
        token::Pipe,
        token::DollarSign,
        token::String("variable2".to_string()),
    ];

    let node1 = setup.add_child(&setup.top_node, "first-node");
    let value1 = nodeset![node1.clone()];
    setup.add_var("variable1", Nodes(value1));

    let node2 = setup.add_child(&setup.top_node, "second-node");
    let value2 = nodeset![node2.clone()];
    setup.add_var("variable2", Nodes(value2));

    let expr = setup.parse(tokens);

    assert_eq!(Nodes(nodeset![node1, node2]), setup.evaluate(&*expr));
}

#[test]
fn absolute_path_expression() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Slash,
    ];

    let node1 = setup.add_child(&setup.top_node, "first-node");
    let node2 = setup.add_child(&node1, "second-node");

    let expr = setup.parse(tokens);

    assert_eq!(Nodes(nodeset![setup.doc.root()]), setup.evaluate_on(&*expr, node2));
}

#[test]
fn absolute_path_with_child_expression() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Slash,
        token::String("*".to_string()),
    ];

    let node1 = setup.add_child(&setup.top_node, "first-node");
    let node2 = setup.add_child(&node1, "second-node");

    let expr = setup.parse(tokens);

    assert_eq!(Nodes(nodeset![setup.top_node.clone()]), setup.evaluate_on(&*expr, node2));
}

#[test]
fn unknown_axis_is_reported_as_an_error() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Axis("bad-axis".to_string()),
        token::DoubleColon,
        token::String("*".to_string())
    ];

    let res = setup.parse_raw(tokens);
    assert_eq!(Some(InvalidXPathAxis("bad-axis".to_string())), res.err());
}

#[test]
fn unknown_node_test_is_reported_as_an_error() {
    let setup = Setup::new();
    let tokens = tokens![
        token::NodeTest("bad-node-test".to_string()),
        token::LeftParen,
        token::RightParen
    ];

    let res = setup.parse_raw(tokens);
    assert_eq!(Some(InvalidNodeTest("bad-node-test".to_string())), res.err());
}

#[test]
fn unexpected_token_is_reported_as_an_error() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Function("does-not-matter".to_string()),
        token::RightParen
    ];

    let res = setup.parser.parse(tokens.move_iter());
    assert_eq!(Some(UnexpectedToken(token::RightParen)), res.err());
}

#[test]
fn binary_operator_without_right_hand_side_is_reported_as_an_error() {
    let setup = Setup::new();
    let tokens = tokens![
        token::Literal("left".to_string()),
        token::And
    ];

    let res = setup.parse_raw(tokens);
    assert_eq!(Some(RightHandSideExpressionMissing), res.err());
}

#[test]
fn unary_operator_without_right_hand_side_is_reported_as_an_error() {
    let setup = Setup::new();
    let tokens = tokens![
        token::MinusSign,
    ];

    let res = setup.parser.parse(tokens.move_iter());
    assert_eq!(Some(RightHandSideExpressionMissing), res.err());
}

#[test]
fn empty_predicate_is_reported_as_an_error() {
    let setup = Setup::new();
    let tokens = tokens![
        token::String("*".to_string()),
        token::LeftBracket,
        token::RightBracket,
    ];

    let res = setup.parse_raw(tokens);
    assert_eq!(Some(EmptyPredicate), res.err());
}

#[test]
fn relative_path_with_trailing_slash_is_reported_as_an_error() {
    let setup = Setup::new();
    let tokens = tokens![
        token::String("*".to_string()),
        token::Slash,
    ];

    let res = setup.parse_raw(tokens);
    assert_eq!(Some(TrailingSlash), res.err());
}

#[test]
fn filter_expression_with_trailing_slash_is_reported_as_an_error() {
    let setup = Setup::new();
    let tokens = tokens![
        token::DollarSign,
        token::String("variable".to_string()),
        token::Slash,
    ];

    let res = setup.parse_raw(tokens);
    assert_eq!(Some(TrailingSlash), res.err());
}

#[test]
fn running_out_of_input_is_reported_as_an_error() {
    let setup = Setup::new();
    let tokens = tokens![token::Function("func".to_string())];

    let res = setup.parse_raw(tokens);
    assert_eq!(Some(RanOutOfInput), res.err());
}

#[test]
fn having_extra_tokens_is_reported_as_an_error() {
    let setup = Setup::new();
    let tokens = tokens![token::LeftBracket];

    let res = setup.parse_raw(tokens);
    assert_eq!(Some(ExtraUnparsedTokens), res.err());
}

#[test]
fn a_tokenizer_error_is_reported_as_an_error() {
    let setup = Setup::new();
    let tokens = vec![
        Ok(token::Function("func".to_string())),
        Err(xpath::tokenizer::UnableToCreateToken)
    ];

    let res = setup.parse_raw(tokens);
    assert_eq!(Some(TokenizerError(xpath::tokenizer::UnableToCreateToken)), res.err());
}

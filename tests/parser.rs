#![feature(phase)]
#![feature(macro_rules)]

#[phase(plugin, link)]
extern crate document;
extern crate xpath;

use std::collections::HashMap;
use std::num::Float;

use document::Package;
use document::dom4::{Document,Root,Element,Text,ToNode};

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

impl<'d> ApproxEq for XPathValue<'d> {
    fn is_approx_eq(&self, other: &XPathValue<'d>) -> bool {
        match (self, other) {
            (&Number(ref x), &Number(ref y)) => x.is_approx_eq(y),
            _ => panic!("It's nonsensical to compare these quantities"),
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

struct TestDoc<'d>(Document<'d>);

impl<'d> TestDoc<'d> {
    fn root(&'d self) -> Root<'d> {
        let &TestDoc(ref doc) = self;
        doc.root()
    }

    fn top_node(&'d self) -> Element<'d> {
        let &TestDoc(ref doc) = self;

        let kids = doc.root().children();
        match kids.len() {
            0 => {
                let n = doc.create_element("the-top-node");
                doc.root().append_child(n);
                n
            },
            1 => {
                kids[0].element().expect("not an element")
            },
            _ => panic!("Too many top nodes"),
        }
    }

    fn add_top_child(&'d self, name: &str) -> Element<'d> {
        self.add_child(self.top_node(), name)
    }

    fn add_child(&'d self, parent: Element<'d>, name: &str) -> Element<'d> {
        let &TestDoc(ref doc) = self;

        let n = doc.create_element(name);
        parent.append_child(n);
        n
    }

    fn add_text(&'d self, parent: Element<'d>, value: &str) -> Text<'d> {
        let &TestDoc(ref doc) = self;

        let tn = doc.create_text(value);
        parent.append_child(tn);
        tn
    }
}

struct Exercise<'d> {
    doc: &'d TestDoc<'d>,
    functions: Functions,
    variables: Variables<'d>,
    parser: XPathParser,
}

impl<'d> Exercise<'d> {
    fn new(doc: &'d TestDoc<'d>) -> Exercise<'d> {
        let mut functions = HashMap::new();
        xpath::function::register_core_functions(&mut functions);

        Exercise {
            doc: doc,
            functions: functions,
            variables: HashMap::new(),
            parser: XPathParser::new(),
        }
    }


    fn add_var(&mut self, name: &str, value: XPathValue<'d>) {
        self.variables.insert(name.to_string(), value);
    }

    fn parse_raw(&self, tokens: Vec<TokenResult>) -> ParseResult {
        self.parser.parse(tokens.into_iter())
    }

    fn parse(&self, tokens: Vec<TokenResult>) -> SubExpression {
        self.parse_raw(tokens).unwrap().unwrap()
    }

    fn evaluate(&'d self, expr: &XPathExpression) -> XPathValue<'d> {
        self.evaluate_on(expr, self.doc.top_node())
    }

    fn evaluate_on<N : ToNode<'d>>(&self, expr: &XPathExpression, node: N) -> XPathValue<'d> {
        let node = node.to_node();
        let mut context = XPathEvaluationContext::new(node,
                                                      &self.functions,
                                                      &self.variables);
        context.next(node);
        expr.evaluate(&context)
    }
}

#[test]
fn parses_string_as_child() {
    let tokens = tokens![token::String("hello".to_string())];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let hello = doc.add_top_child("hello");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![hello]), ex.evaluate_on(&*expr, doc.top_node()));
}

#[test]
fn parses_two_strings_as_grandchild() {
    let tokens = tokens![
        token::String("hello".to_string()),
        token::Slash,
        token::String("world".to_string())
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let hello = doc.add_top_child("hello");
    let world = doc.add_child(hello, "world");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![world]), ex.evaluate_on(&*expr, doc.top_node()));
}

#[test]
fn parses_self_axis() {
    let tokens = tokens![
        token::Axis("self".to_string()),
        token::DoubleColon,
        token::String("the-top-node".to_string())
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![doc.top_node()]), ex.evaluate_on(&*expr, doc.top_node()));
}

#[test]
fn parses_parent_axis() {
    let tokens = tokens![
        token::Axis("parent".to_string()),
        token::DoubleColon,
        token::String("the-top-node".to_string())
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let hello = doc.add_top_child("hello");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![doc.top_node()]), ex.evaluate_on(&*expr, hello));
}

#[test]
fn parses_descendant_axis() {
    let tokens = tokens![
        token::Axis("descendant".to_string()),
        token::DoubleColon,
        token::String("two".to_string())
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let one = doc.add_top_child("one");
    let two = doc.add_child(one, "two");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![two]), ex.evaluate_on(&*expr, doc.top_node()));
}

#[test]
fn parses_descendant_or_self_axis() {
    let tokens = tokens![
        token::Axis("descendant-or-self".to_string()),
        token::DoubleColon,
        token::String("*".to_string())
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let one = doc.add_top_child("one");
    let two = doc.add_child(one, "two");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![one, two]), ex.evaluate_on(&*expr, one));
}

#[test]
fn parses_attribute_axis() {
    let tokens = tokens![
        token::Axis("attribute".to_string()),
        token::DoubleColon,
        token::String("*".to_string())
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let one = doc.add_top_child("one");
    let attr = one.set_attribute_value("hello", "world");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![attr]), ex.evaluate_on(&*expr, one));
}

#[test]
fn parses_child_with_same_name_as_an_axis() {
    let tokens = tokens![token::String("self".to_string())];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let element = doc.add_top_child("self");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![element]), ex.evaluate_on(&*expr, doc.top_node()));
}

#[test]
fn parses_node_node_test() {
    let tokens = tokens![
        token::NodeTest("node".to_string()),
        token::LeftParen,
        token::RightParen
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let one = doc.add_top_child("one");
    let two = doc.add_child(one, "two");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![two]), ex.evaluate_on(&*expr, one));
}

#[test]
fn parses_text_node_test() {
    let tokens = tokens![
        token::NodeTest("text".to_string()),
        token::LeftParen,
        token::RightParen
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let one = doc.add_top_child("one");
    let text = doc.add_text(one, "text");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![text]), ex.evaluate_on(&*expr, one));
}

#[test]
fn parses_axis_and_node_test() {
    let tokens = tokens![
        token::Axis("self".to_string()),
        token::DoubleColon,
        token::NodeTest("text".to_string()),
        token::LeftParen,
        token::RightParen
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let one = doc.add_top_child("one");
    let text = doc.add_text(one, "text");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![text]), ex.evaluate_on(&*expr, text));
}

#[test]
fn numeric_predicate_selects_indexed_node() {
    let tokens = tokens![
        token::String("*".to_string()),
        token::LeftBracket,
        token::Number(2.0),
        token::RightBracket
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    doc.add_top_child("first");
    let second = doc.add_top_child("second");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![second]), ex.evaluate_on(&*expr, doc.top_node()));
}

#[test]
fn string_literal() {
    let tokens = tokens![token::Literal("string".to_string())];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(String("string".to_string()), ex.evaluate(&*expr));
}

#[test]
fn predicate_accepts_any_expression() {
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

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let first = doc.add_top_child("first");
    let second = doc.add_top_child("second");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![first, second]), ex.evaluate_on(&*expr, doc.top_node()));
}

#[test]
fn true_function_predicate_selects_all_nodes() {
    let tokens = tokens![
        token::String("*".to_string()),
        token::LeftBracket,
        token::Function("true".to_string()),
        token::LeftParen,
        token::RightParen,
        token::RightBracket
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let first = doc.add_top_child("first");
    let second = doc.add_top_child("second");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![first, second]), ex.evaluate_on(&*expr, doc.top_node()));
}

#[test]
fn false_function_predicate_selects_no_nodes() {
    let tokens = tokens![
        token::String("*".to_string()),
        token::LeftBracket,
        token::Function("false".to_string()),
        token::LeftParen,
        token::RightParen,
        token::RightBracket
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    doc.add_top_child("first");
    doc.add_top_child("second");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![]), ex.evaluate_on(&*expr, doc.top_node()));
}

#[test]
fn multiple_predicates() {
    let tokens = tokens![
        token::String("*".to_string()),
        token::LeftBracket,
        token::Number(2.0),
        token::RightBracket,
        token::LeftBracket,
        token::Number(1.0),
        token::RightBracket
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    doc.add_top_child("first");
    let second = doc.add_top_child("second");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![second]), ex.evaluate_on(&*expr, doc.top_node()));
}

#[test]
fn functions_accept_arguments() {
    let tokens = tokens![
        token::Function("not".to_string()),
        token::LeftParen,
        token::Function("true".to_string()),
        token::LeftParen,
        token::RightParen,
        token::RightParen,
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Boolean(false), ex.evaluate(&*expr));
}

#[test]
fn functions_accept_any_expression_as_an_argument() {
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

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Boolean(false), ex.evaluate(&*expr));
}

#[test]
fn numeric_literal() {
    let tokens = tokens![token::Number(3.2)];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_approx_eq!(Number(3.2), ex.evaluate(&*expr));
}

#[test]
fn addition_of_two_numbers() {
    let tokens = tokens![
        token::Number(1.1),
        token::PlusSign,
        token::Number(2.2)
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_approx_eq!(Number(3.3), ex.evaluate(&*expr));
}

#[test]
fn addition_of_multiple_numbers() {
    let tokens = tokens![
        token::Number(1.1),
        token::PlusSign,
        token::Number(2.2),
        token::PlusSign,
        token::Number(3.3)
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_approx_eq!(Number(6.6), ex.evaluate(&*expr));
}

#[test]
fn subtraction_of_two_numbers() {
    let tokens = tokens![
        token::Number(1.1),
        token::MinusSign,
        token::Number(2.2),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_approx_eq!(Number(-1.1), ex.evaluate(&*expr));
}

#[test]
fn additive_expression_is_left_associative() {
    let tokens = tokens![
        token::Number(1.1),
        token::MinusSign,
        token::Number(2.2),
        token::MinusSign,
        token::Number(3.3),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_approx_eq!(Number(-4.4), ex.evaluate(&*expr));
}

#[test]
fn multiplication_of_two_numbers() {
    let tokens = tokens![
        token::Number(1.1),
        token::Multiply,
        token::Number(2.2),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_approx_eq!(Number(2.42), ex.evaluate(&*expr));
}

#[test]
fn division_of_two_numbers() {
    let tokens = tokens![
        token::Number(7.1),
        token::Divide,
        token::Number(0.1),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_approx_eq!(Number(71.0), ex.evaluate(&*expr));
}

#[test]
fn remainder_of_two_numbers() {
    let tokens = tokens![
        token::Number(7.1),
        token::Remainder,
        token::Number(3.0),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_approx_eq!(Number(1.1), ex.evaluate(&*expr));
}

#[test]
fn unary_negation() {
    let tokens = tokens![
        token::MinusSign,
        token::Number(7.2),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_approx_eq!(Number(-7.2), ex.evaluate(&*expr));
}

#[test]
fn repeated_unary_negation() {
    let tokens = tokens![
        token::MinusSign,
        token::MinusSign,
        token::MinusSign,
        token::Number(7.2),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_approx_eq!(Number(-7.2), ex.evaluate(&*expr));
}

#[test]
fn top_level_function_call() {
    let tokens = tokens![
        token::Function("true".to_string()),
        token::LeftParen,
        token::RightParen,
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Boolean(true), ex.evaluate(&*expr));
}

#[test]
fn or_expression() {
    let tokens = tokens![
        token::Function("true".to_string()),
        token::LeftParen,
        token::RightParen,
        token::Or,
        token::Function("false".to_string()),
        token::LeftParen,
        token::RightParen,
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Boolean(true), ex.evaluate(&*expr));
}

#[test]
fn and_expression() {
    let tokens = tokens![
        token::Number(1.2),
        token::And,
        token::Number(0.0),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Boolean(false), ex.evaluate(&*expr));
}

#[test]
fn equality_expression() {
    let tokens = tokens![
        token::Number(1.2),
        token::Equal,
        token::Number(1.1),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Boolean(false), ex.evaluate(&*expr));
}

#[test]
fn inequality_expression() {
    let tokens = tokens![
        token::Number(1.2),
        token::NotEqual,
        token::Number(1.2),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Boolean(false), ex.evaluate(&*expr));
}

#[test]
fn less_than_expression() {
    let tokens = tokens![
        token::Number(1.2),
        token::LessThan,
        token::Number(1.2),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Boolean(false), ex.evaluate(&*expr));
}

#[test]
fn less_than_or_equal_expression() {
    let tokens = tokens![
        token::Number(1.2),
        token::LessThanOrEqual,
        token::Number(1.2),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Boolean(true), ex.evaluate(&*expr));
}

#[test]
fn greater_than_expression() {
    let tokens = tokens![
        token::Number(1.2),
        token::GreaterThan,
        token::Number(1.2),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Boolean(false), ex.evaluate(&*expr));
}

#[test]
fn greater_than_or_equal_expression() {
    let tokens = tokens![
        token::Number(1.2),
        token::GreaterThanOrEqual,
        token::Number(1.2),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Boolean(true), ex.evaluate(&*expr));
}

#[test]
fn variable_reference() {
    let tokens = tokens![
        token::DollarSign,
        token::String("variable-name".to_string()),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let mut ex = Exercise::new(&doc);
    ex.add_var("variable-name", Number(12.3));
    let expr = ex.parse(tokens);

    assert_approx_eq!(Number(12.3), ex.evaluate(&*expr));
}

#[test]
fn filter_expression() {
    let tokens = tokens![
        token::DollarSign,
        token::String("variable".to_string()),
        token::LeftBracket,
        token::Number(0.0),
        token::RightBracket,
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let value = nodeset![
        doc.add_top_child("first-node"),
        doc.add_top_child("second-node"),
    ];

    let mut ex = Exercise::new(&doc);
    ex.add_var("variable", Nodes(value));

    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![]), ex.evaluate(&*expr));
}

#[test]
fn filter_expression_and_relative_path() {
    let tokens = tokens![
        token::DollarSign,
        token::String("variable".to_string()),
        token::Slash,
        token::String("child".to_string()),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let parent = doc.add_top_child("parent");
    let child = doc.add_child(parent, "child");

    let value = nodeset![parent];

    let mut ex = Exercise::new(&doc);
    ex.add_var("variable", Nodes(value));

    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![child]), ex.evaluate(&*expr));
}

#[test]
fn union_expression() {
    let tokens = tokens![
        token::DollarSign,
        token::String("variable1".to_string()),
        token::Pipe,
        token::DollarSign,
        token::String("variable2".to_string()),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let node1 = doc.add_top_child("first-node");
    let node2 = doc.add_top_child("second-node");

    let mut ex = Exercise::new(&doc);
    ex.add_var("variable1", Nodes(nodeset![node1]));
    ex.add_var("variable2", Nodes(nodeset![node2]));

    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![node1, node2]), ex.evaluate(&*expr));
}

#[test]
fn absolute_path_expression() {
    let tokens = tokens![
        token::Slash,
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let node1 = doc.add_top_child("first-node");
    let node2 = doc.add_child(node1, "second-node");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![doc.root()]), ex.evaluate_on(&*expr, node2));
}

#[test]
fn absolute_path_with_child_expression() {
    let tokens = tokens![
        token::Slash,
        token::String("*".to_string()),
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());
    let node1 = doc.add_top_child("first-node");
    let node2 = doc.add_child(node1, "second-node");

    let ex = Exercise::new(&doc);
    let expr = ex.parse(tokens);

    assert_eq!(Nodes(nodeset![doc.top_node()]), ex.evaluate_on(&*expr, node2));
}

#[test]
fn unknown_axis_is_reported_as_an_error() {
    let tokens = tokens![
        token::Axis("bad-axis".to_string()),
        token::DoubleColon,
        token::String("*".to_string())
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let res = ex.parse_raw(tokens);
    assert_eq!(Some(InvalidXPathAxis("bad-axis".to_string())), res.err());
}

#[test]
fn unknown_node_test_is_reported_as_an_error() {
    let tokens = tokens![
        token::NodeTest("bad-node-test".to_string()),
        token::LeftParen,
        token::RightParen
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let res = ex.parse_raw(tokens);
    assert_eq!(Some(InvalidNodeTest("bad-node-test".to_string())), res.err());
}

#[test]
fn unexpected_token_is_reported_as_an_error() {
    let tokens = tokens![
        token::Function("does-not-matter".to_string()),
        token::RightParen
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let res = ex.parser.parse(tokens.into_iter());
    assert_eq!(Some(UnexpectedToken(token::RightParen)), res.err());
}

#[test]
fn binary_operator_without_right_hand_side_is_reported_as_an_error() {
    let tokens = tokens![
        token::Literal("left".to_string()),
        token::And
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let res = ex.parse_raw(tokens);
    assert_eq!(Some(RightHandSideExpressionMissing), res.err());
}

#[test]
fn unary_operator_without_right_hand_side_is_reported_as_an_error() {
    let tokens = tokens![
        token::MinusSign,
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let res = ex.parser.parse(tokens.into_iter());
    assert_eq!(Some(RightHandSideExpressionMissing), res.err());
}

#[test]
fn empty_predicate_is_reported_as_an_error() {
    let tokens = tokens![
        token::String("*".to_string()),
        token::LeftBracket,
        token::RightBracket,
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let res = ex.parse_raw(tokens);
    assert_eq!(Some(EmptyPredicate), res.err());
}

#[test]
fn relative_path_with_trailing_slash_is_reported_as_an_error() {
    let tokens = tokens![
        token::String("*".to_string()),
        token::Slash,
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let res = ex.parse_raw(tokens);
    assert_eq!(Some(TrailingSlash), res.err());
}

#[test]
fn filter_expression_with_trailing_slash_is_reported_as_an_error() {
    let tokens = tokens![
        token::DollarSign,
        token::String("variable".to_string()),
        token::Slash,
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let res = ex.parse_raw(tokens);
    assert_eq!(Some(TrailingSlash), res.err());
}

#[test]
fn running_out_of_input_is_reported_as_an_error() {
    let tokens = tokens![token::Function("func".to_string())];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let res = ex.parse_raw(tokens);
    assert_eq!(Some(RanOutOfInput), res.err());
}

#[test]
fn having_extra_tokens_is_reported_as_an_error() {
    let tokens = tokens![token::LeftBracket];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let res = ex.parse_raw(tokens);
    assert_eq!(Some(ExtraUnparsedTokens), res.err());
}

#[test]
fn a_tokenizer_error_is_reported_as_an_error() {
    let tokens = vec![
        Ok(token::Function("func".to_string())),
        Err(xpath::tokenizer::UnableToCreateToken)
    ];

    let package = Package::new();
    let doc = TestDoc(package.as_document());

    let ex = Exercise::new(&doc);
    let res = ex.parse_raw(tokens);
    assert_eq!(Some(TokenizerError(xpath::tokenizer::UnableToCreateToken)), res.err());
}

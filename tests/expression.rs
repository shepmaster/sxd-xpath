#![feature(phase)]

extern crate document;
#[phase(plugin, link)]
extern crate xpath;

use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

use document::Package;
use document::dom4::Document;

use xpath::Value;
use xpath::Value::{Boolean, Number, String, Nodes};
use xpath::{Functions,Variables};
use xpath::Function;
use xpath::EvaluationContext;
use xpath::nodeset::Nodeset;

use xpath::expression;
use xpath::expression::Expression;
use xpath::expression::{And,
                        Equal,
                        ExpressionNotEqual,
                        Literal,
                        Math,
                        ExpressionPredicate,
                        ExpressionRelational,
                        ExpressionRootNode,
                        ExpressionStep,
                        ExpressionUnion,
                        ExpressionVariable};
use xpath::expression::LiteralValue::{BooleanLiteral,NumberLiteral,StringLiteral};

use xpath::axis::Axis;
use xpath::node_test::NodeTest;

struct FailExpression;

impl Expression for FailExpression {
    fn evaluate<'a, 'd>(&self, _: &EvaluationContext<'a, 'd>) -> Value<'d> {
        panic!("Should never be called");
    }
}

struct Setup<'d> {
    doc: Document<'d>,
    funs: Functions,
    vars: Variables<'d>,
}

impl<'d> Setup<'d> {
    fn new(package: &'d Package) -> Setup<'d> {
        Setup {
            doc: package.as_document(),
            funs: HashMap::new(),
            vars: HashMap::new(),
        }
    }

    fn context(&'d self) -> EvaluationContext<'d, 'd> {
        let node = self.doc.create_element("test");
        EvaluationContext::new(node, &self.funs, &self.vars)
    }
}

#[test]
fn expression_and_returns_logical_and() {
    let package = Package::new();
    let setup = Setup::new(&package);

    let left  = box Literal{value: BooleanLiteral(true)};
    let right = box Literal{value: BooleanLiteral(true)};

    let expr = And{left: left, right: right};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(true));
}

#[test]
fn expression_and_short_circuits_when_left_argument_is_false() {
    let package = Package::new();
    let setup = Setup::new(&package);

    let left  = box Literal{value: BooleanLiteral(false)};
    let right = box FailExpression;

    let expr = And{left: left, right: right};

    let context = setup.context();
    expr.evaluate(&context);
    // assert_not_fail
}

#[test]
fn expression_equal_intersects_string_values_of_two_nodesets() {
    let package = Package::new();
    let mut setup = Setup::new(&package);

    let string_value_1 = setup.doc.create_text("same");
    let string_value_2 = setup.doc.create_text("same");

    setup.vars.insert("left".to_string(), Nodes(nodeset![string_value_1]));
    setup.vars.insert("right".to_string(), Nodes(nodeset![string_value_2]));

    let left  = box ExpressionVariable{name: "left".to_string()};
    let right = box ExpressionVariable{name: "right".to_string()};

    let expr = Equal{left: left, right: right};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(true));
}

#[test]
fn expression_equal_compares_number_value_of_nodeset_to_number() {
    let package = Package::new();
    let mut setup = Setup::new(&package);

    let string_value = setup.doc.create_text("3.14");
    setup.vars.insert("left".to_string(), Nodes(nodeset![string_value]));

    let left  = box ExpressionVariable{name: "left".to_string()};
    let right = box Literal{value: NumberLiteral(6.28)};

    let expr = Equal{left: left, right: right};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(false));
}

#[test]
fn expression_equal_compares_string_value_of_nodeset_to_string() {
    let package = Package::new();
    let mut setup = Setup::new(&package);

    let string_value_1 = setup.doc.create_text("gravy");
    let string_value_2 = setup.doc.create_text("boat");
    setup.vars.insert("left".to_string(), Nodes(nodeset![string_value_1, string_value_2]));

    let left  = box ExpressionVariable{name: "left".to_string()};
    let right = box Literal{value: StringLiteral("boat".to_string())};

    let expr = Equal{left: left, right: right};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(true));
}

#[test]
fn expression_equal_compares_as_boolean_if_one_argument_is_a_boolean() {
    let package = Package::new();
    let setup = Setup::new(&package);

    let actual_bool = box Literal{value: BooleanLiteral(false)};
    let truthy_str = box Literal{value: StringLiteral("hello".to_string())};

    let expr = Equal{left: actual_bool, right: truthy_str};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(false));
}

#[test]
fn expression_equal_compares_as_number_if_one_argument_is_a_number() {
    let package = Package::new();
    let setup = Setup::new(&package);

    let actual_number = box Literal{value: NumberLiteral(-42.0)};
    let number_str = box Literal{value: StringLiteral("-42.0".to_string())};

    let expr = Equal{left: number_str, right: actual_number};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(true));
}

#[test]
fn expression_equal_compares_as_string_otherwise() {
    let package = Package::new();
    let setup = Setup::new(&package);

    let a_str = box Literal{value: StringLiteral("hello".to_string())};
    let b_str = box Literal{value: StringLiteral("World".to_string())};

    let expr = Equal{left: a_str, right: b_str};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(false));
}

#[test]
fn expression_not_equal_negates_equality() {
    let package = Package::new();
    let setup = Setup::new(&package);

    let a_str = box Literal{value: BooleanLiteral(true)};
    let b_str = box Literal{value: BooleanLiteral(false)};

    let expr = ExpressionNotEqual::new(a_str, b_str);

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(true));
}

struct StubFunction {
    value: &'static str,
}

impl Function for StubFunction {
    fn evaluate<'a, 'd>(&self,
                        _: &EvaluationContext<'a, 'd>,
                        _: Vec<Value<'d>>) -> Value<'d>
    {
        String(self.value.to_string())
    }
}

#[test]
fn expression_function_evaluates_input_arguments() {
    let package = Package::new();
    let mut setup = Setup::new(&package);

    let arg_expr: Box<Expression> = box Literal{value: BooleanLiteral(true)};
    let fun = box StubFunction{value: "the function ran"};
    setup.funs.insert("test-fn".to_string(), fun);

    let expr = expression::Function{name: "test-fn".to_string(), arguments: vec!(arg_expr)};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, String("the function ran".to_string()));
}

#[ignore]
#[test]
fn expression_function_unknown_function_is_reported_as_an_error() {
    let package = Package::new();
    let setup = Setup::new(&package);

    let expr = expression::Function{name: "unknown-fn".to_string(), arguments: vec!()};

    let context = setup.context();
    expr.evaluate(&context);
    // TODO: report errors better
}

#[test]
fn expression_math_does_basic_math() {
    let package = Package::new();
    let setup = Setup::new(&package);

    let left  = box Literal{value: NumberLiteral(10.0)};
    let right = box Literal{value: NumberLiteral(5.0)};

    let expr = Math::multiplication(left, right);

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Number(50.0));
}

#[test]
fn expression_step_numeric_predicate_selects_that_node() {
    let package = Package::new();
    let mut setup = Setup::new(&package);

    let input_node_1 = setup.doc.create_element("one");
    let input_node_2 = setup.doc.create_element("two");
    let input_nodeset = nodeset![input_node_1, input_node_2];

    setup.vars.insert("nodes".to_string(), Nodes(input_nodeset));

    let selected_nodes = box ExpressionVariable{name: "nodes".to_string()};
    let predicate = box Literal{value: NumberLiteral(1.0)};

    let expr = ExpressionPredicate::new(selected_nodes, predicate);

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Nodes(nodeset![input_node_1]));
}

#[test]
fn expression_step_false_predicate_selects_no_nodes() {
    let package = Package::new();
    let mut setup = Setup::new(&package);

    let input_node_1 = setup.doc.create_element("one");
    let input_node_2 = setup.doc.create_element("two");
    let input_nodeset = nodeset![input_node_1, input_node_2];

    setup.vars.insert("nodes".to_string(), Nodes(input_nodeset));

    let selected_nodes = box ExpressionVariable{name: "nodes".to_string()};
    let predicate = box Literal{value: BooleanLiteral(false)};

    let expr = ExpressionPredicate::new(selected_nodes, predicate);

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Nodes(nodeset![]));
}

#[test]
fn expression_relational_does_basic_comparisons() {
    let package = Package::new();
    let setup = Setup::new(&package);

    let left  = box Literal{value: NumberLiteral(10.0)};
    let right = box Literal{value: NumberLiteral(5.0)};

    let expr = ExpressionRelational::less_than(left, right);

    let context = setup.context();
    let res = expr.evaluate(&context);
    assert_eq!(res, Boolean(false));
}

#[test]
fn expression_root_node_finds_the_root() {
    let package = Package::new();
    let setup = Setup::new(&package);

    let expr = ExpressionRootNode;

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Nodes(nodeset![setup.doc.root()]));
}

#[deriving(Clone)]
struct MockAxis {
    calls: Rc<RefCell<uint>>,
}

impl MockAxis {
    fn new() -> MockAxis {
        MockAxis{ calls: Rc::new(RefCell::new(0)) }
    }

    fn calls(&self) -> uint {
        *self.calls.borrow()
    }
}

impl Axis for MockAxis {
    fn select_nodes(&self,
                    _context:   &EvaluationContext,
                    _node_test: &NodeTest,
                    _result:    &mut Nodeset)
    {
        *self.calls.borrow_mut() += 1;
    }
}

struct DummyNodeTest;

impl NodeTest for DummyNodeTest {
    fn test(&self, _context: &EvaluationContext, _result: &mut Nodeset) {
    }
}

#[test]
fn expression_step_delegates_to_the_axis() {
    let package = Package::new();
    let setup = Setup::new(&package);

    let axis = MockAxis::new();
    let node_test = DummyNodeTest;

    let expr = ExpressionStep::new(box axis.clone(), box node_test);

    let context = setup.context();
    expr.evaluate(&context);

    assert_eq!(1, axis.calls());
}

#[test]
fn expression_union_combines_nodesets() {
    let package = Package::new();
    let mut setup = Setup::new(&package);

    let left_node = setup.doc.create_element("left");
    let nodes = nodeset![left_node];
    setup.vars.insert("left".to_string(), Nodes(nodes));
    let left = box ExpressionVariable{name: "left".to_string()};

    let right_node = setup.doc.create_element("right");
    let nodes = nodeset![right_node];
    setup.vars.insert("right".to_string(), Nodes(nodes));
    let right = box ExpressionVariable{name: "right".to_string()};

    let expr = ExpressionUnion{left: left, right: right};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(Nodes(nodeset![left_node, right_node]), res);
}

#[test]
fn expression_variable_looks_up_the_variable() {
    let package = Package::new();
    let mut setup = Setup::new(&package);
    setup.vars.insert("foo".to_string(), Boolean(true));

    let expr = ExpressionVariable{name: "foo".to_string()};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(Boolean(true), res);
}

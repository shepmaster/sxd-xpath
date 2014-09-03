extern crate document;
extern crate xpath;

use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

use document::{Document,Element,Nodeset};

use xpath::XPathValue;
use xpath::{Boolean, Number, String, Nodes};
use xpath::{Functions,Variables};
use xpath::XPathFunction;
use xpath::XPathEvaluationContext;

use xpath::expression::XPathExpression;
use xpath::expression::{ExpressionAnd,
                        ExpressionEqual,
                        ExpressionNotEqual,
                        ExpressionFunction,
                        ExpressionLiteral,
                        ExpressionMath,
                        ExpressionPredicate,
                        ExpressionRelational,
                        ExpressionRootNode,
                        ExpressionStep,
                        ExpressionUnion,
                        ExpressionVariable};

use xpath::axis::XPathAxis;
use xpath::node_test::XPathNodeTest;

struct FailExpression;

impl XPathExpression for FailExpression {
    fn evaluate(&self, _: &XPathEvaluationContext) -> XPathValue {
        fail!("Should never be called");
    }
}

struct Setup {
    doc: Document,
    node: Element,
    funs: Functions,
    vars: Variables,
}

impl Setup {
    fn new() -> Setup {
        let d = Document::new();
        let n = d.new_element("test".to_string());
        Setup {
            doc: d,
            node: n,
            funs: HashMap::new(),
            vars: HashMap::new(),
        }
    }

    fn context(&self) -> XPathEvaluationContext {
        XPathEvaluationContext::new(self.node.clone(), &self.funs, &self.vars)
    }
}

#[test]
fn expression_and_returns_logical_and() {
    let setup = Setup::new();

    let left  = box ExpressionLiteral{value: Boolean(true)};
    let right = box ExpressionLiteral{value: Boolean(true)};

    let expr = ExpressionAnd{left: left, right: right};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(true));
}

#[test]
fn expression_and_short_circuits_when_left_argument_is_false() {
    let setup = Setup::new();

    let left  = box ExpressionLiteral{value: Boolean(false)};
    let right = box FailExpression;

    let expr = ExpressionAnd{left: left, right: right};

    let context = setup.context();
    expr.evaluate(&context);
    // assert_not_fail
}

#[test]
fn expression_equal_compares_as_boolean_if_one_argument_is_a_boolean() {
    let setup = Setup::new();

    let actual_bool = box ExpressionLiteral{value: Boolean(false)};
    let truthy_str = box ExpressionLiteral{value: String("hello".to_string())};

    let expr = ExpressionEqual{left: actual_bool, right: truthy_str};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(false));
}

#[test]
fn expression_equal_compares_as_number_if_one_argument_is_a_number() {
    let setup = Setup::new();

    let actual_number = box ExpressionLiteral{value: Number(-42.0)};
    let number_str = box ExpressionLiteral{value: String("-42.0".to_string())};

    let expr = ExpressionEqual{left: number_str, right: actual_number};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(true));
}

#[test]
fn expression_equal_compares_as_string_otherwise() {
    let setup = Setup::new();

    let a_str = box ExpressionLiteral{value: String("hello".to_string())};
    let b_str = box ExpressionLiteral{value: String("World".to_string())};

    let expr = ExpressionEqual{left: a_str, right: b_str};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(false));
}

#[test]
fn expression_not_equal_negates_equality() {
    let setup = Setup::new();

    let a_str = box ExpressionLiteral{value: Boolean(true)};
    let b_str = box ExpressionLiteral{value: Boolean(false)};

    let expr = ExpressionNotEqual::new(a_str, b_str);

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Boolean(true));
}

struct StubFunction {
    value: XPathValue,
}

impl XPathFunction for StubFunction {
    fn evaluate(&self,
                _: &XPathEvaluationContext,
                _: Vec<XPathValue>) -> XPathValue
    {
        self.value.clone()
    }
}

#[test]
fn expression_function_evaluates_input_arguments() {
    let mut setup = Setup::new();

    let arg_expr: Box<XPathExpression> = box ExpressionLiteral{value: Boolean(true)};
    let fun = box StubFunction{value: String("the function ran".to_string())};
    setup.funs.insert("test-fn".to_string(), fun);

    let expr = ExpressionFunction{name: "test-fn".to_string(), arguments: vec!(arg_expr)};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, String("the function ran".to_string()));
}

#[test]
fn expression_function_unknown_function_is_reported_as_an_error() {
    let setup = Setup::new();

    let expr = ExpressionFunction{name: "unknown-fn".to_string(), arguments: vec!()};

    let context = setup.context();
    expr.evaluate(&context);
    // TODO: report errors better
}

#[test]
fn expression_math_does_basic_math() {
    let setup = Setup::new();

    let left  = box ExpressionLiteral{value: Number(10.0)};
    let right = box ExpressionLiteral{value: Number(5.0)};

    let expr = ExpressionMath::multiplication(left, right);

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(res, Number(50.0));
}

#[test]
fn expression_step_numeric_predicate_selects_that_node() {
    let setup = Setup::new();

    let input_node_1 = setup.doc.new_element("one".to_string());
    let input_node_2 = setup.doc.new_element("two".to_string());
    let mut input_nodeset = Nodeset::new();
    input_nodeset.add(input_node_1.clone());
    input_nodeset.add(input_node_2);

    let selected_nodes = box ExpressionLiteral{value: Nodes(input_nodeset)};
    let predicate = box ExpressionLiteral{value: Number(1.0)};

    let expr = ExpressionPredicate::new(selected_nodes, predicate);

    let context = setup.context();
    let res = expr.evaluate(&context);

    let mut expected = Nodeset::new();
    expected.add(input_node_1);

    assert_eq!(res, Nodes(expected));
}

#[test]
fn expression_step_false_predicate_selects_no_nodes() {
    let setup = Setup::new();

    let input_node_1 = setup.doc.new_element("one".to_string());
    let input_node_2 = setup.doc.new_element("two".to_string());
    let mut input_nodeset = Nodeset::new();
    input_nodeset.add(input_node_1);
    input_nodeset.add(input_node_2);

    let selected_nodes = box ExpressionLiteral{value: Nodes(input_nodeset)};
    let predicate = box ExpressionLiteral{value: Boolean(false)};

    let expr = ExpressionPredicate::new(selected_nodes, predicate);

    let context = setup.context();
    let res = expr.evaluate(&context);

    let expected = Nodeset::new();
    assert_eq!(res, Nodes(expected));
}

#[test]
fn expression_relational_does_basic_comparisons() {
    let setup = Setup::new();

    let left  = box ExpressionLiteral{value: Number(10.0)};
    let right = box ExpressionLiteral{value: Number(5.0)};

    let expr = ExpressionRelational::less_than(left, right);

    let context = setup.context();
    let res = expr.evaluate(&context);
    assert_eq!(res, Boolean(false));
}

#[test]
fn expression_root_node_finds_the_root() {
    let setup = Setup::new();

    let expr = ExpressionRootNode;

    let context = setup.context();
    let res = expr.evaluate(&context);

    let mut expected = Nodeset::new();
    expected.add(setup.doc.root());

    assert_eq!(res, Nodes(expected));
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

impl XPathAxis for MockAxis {
    fn select_nodes(&self,
                    _context:   &XPathEvaluationContext,
                    _node_test: &XPathNodeTest,
                    _result:    &mut Nodeset)
    {
        *self.calls.borrow_mut() += 1;
    }
}

struct DummyNodeTest;

impl XPathNodeTest for DummyNodeTest {
    fn test(&self, _context: &XPathEvaluationContext, _result: &mut Nodeset) {
    }
}

#[test]
fn expression_step_delegates_to_the_axis() {
    let setup = Setup::new();

    let axis = MockAxis::new();
    let node_test = DummyNodeTest;

    let expr = ExpressionStep::new(box axis.clone(), box node_test);

    let context = setup.context();
    expr.evaluate(&context);

    assert_eq!(1, axis.calls());
}

#[test]
fn expression_union_combines_nodesets() {
    let setup = Setup::new();

    let build = |name: &str| {
        let node = setup.doc.new_element(name.to_string());
        let mut nodes = Nodeset::new();
        nodes.add(node.clone());
        (node, box ExpressionLiteral{value: Nodes(nodes)})
    };

    let (left_node, left) = build("left");
    let (right_node, right) = build("right");

    let expr = ExpressionUnion{left: left, right: right};

    let context = setup.context();
    let res = expr.evaluate(&context);

    let mut expected = Nodeset::new();
    expected.add(left_node);
    expected.add(right_node);

    assert_eq!(Nodes(expected), res);
}

#[test]
fn expression_variable_looks_up_the_variable() {
    let mut setup = Setup::new();
    setup.vars.insert("foo".to_string(), Boolean(true));

    let expr = ExpressionVariable{name: "foo".to_string()};

    let context = setup.context();
    let res = expr.evaluate(&context);

    assert_eq!(Boolean(true), res);
}

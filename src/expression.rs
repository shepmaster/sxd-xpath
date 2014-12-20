use std::collections::HashSet;

use super::nodeset::Nodeset;

use self::LiteralValue::*;

use super::EvaluationContext;
use super::Value;
use super::Value::{Boolean,Number,Nodes};
use super::StringValue;

use super::axis::Axis;
use super::node_test::NodeTest;

// TODO: Figure out how to use HOFs to get rid of returning a Box here
// all the time.

pub trait Expression {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d>;
}

pub type SubExpression = Box<Expression + 'static>;

macro_rules! binary_constructor(
    ($t:ident) => (
        impl $t {
            pub fn new(left: SubExpression, right: SubExpression) -> SubExpression {
                box $t{left: left, right: right} as SubExpression
            }
        }
    );
);

pub struct And {
    pub left:  SubExpression,
    pub right: SubExpression,
}

binary_constructor!(And);

impl Expression for And {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        Boolean(self.left.evaluate(context).boolean() &&
                self.right.evaluate(context).boolean())
    }
}

#[allow(missing_copy_implementations)]
pub struct ContextNode;

impl Expression for ContextNode {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        Nodes(nodeset![context.node])
    }
}

pub struct Equal {
    pub left:  SubExpression,
    pub right: SubExpression,
}

binary_constructor!(Equal);

impl Equal {
    fn boolean_evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> bool {
        let left_val = self.left.evaluate(context);
        let right_val = self.right.evaluate(context);

        fn str_vals(nodes: &Nodeset) -> HashSet<String> {
            nodes.iter().map(|n| n.string_value()).collect()
        }

        fn num_vals(nodes: &Nodeset) -> Vec<f64> {
            // f64 isn't hashable...
            nodes
                .iter()
                .map(|n| Value::String(n.string_value()).number())
                .collect()
        }

        match (&left_val, &right_val) {
            (&Nodes(ref left_nodes), &Nodes(ref right_nodes)) => {
                let left_strings = str_vals(left_nodes);
                let right_strings = str_vals(right_nodes);
                !left_strings.is_disjoint(&right_strings)
            },
            (&Nodes(ref nodes), &Number(val)) |
            (&Number(val), &Nodes(ref nodes)) => {
                let numbers = num_vals(nodes);
                numbers.iter().any(|n| *n == val)
            },
            (&Nodes(ref nodes), &Value::String(ref val)) |
            (&Value::String(ref val), &Nodes(ref nodes)) => {
                let strings = str_vals(nodes);
                strings.contains(val)
            },
            (&Boolean(_), _) |
            (_, &Boolean(_)) => left_val.boolean() == right_val.boolean(),
            (&Number(_), _) |
            (_, &Number(_)) => left_val.number() == right_val.number(),
            _ => left_val.string() == right_val.string()
        }
    }
}

impl Expression for Equal {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        Boolean(self.boolean_evaluate(context))
    }
}

pub struct NotEqual {
    equal: Equal,
}

impl NotEqual {
    pub fn new(left: SubExpression, right: SubExpression) -> SubExpression {
        box NotEqual {
            equal: Equal{left: left, right: right}
        }
    }
}

impl Expression for NotEqual {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        Boolean(!self.equal.boolean_evaluate(context))
    }
}

pub struct Function {
    pub name: String,
    pub arguments: Vec<SubExpression>,
}

impl Expression for Function {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        match context.function_for_name(self.name.as_slice()) {
            Some(fun) => {
                // TODO: Error when argument count mismatch
                let args = self.arguments.iter().map(|ref arg| arg.evaluate(context)).collect();

                fun.evaluate(context, args)
            },
            None => panic!("throw UnknownFunctionException(_name)"),
        }
    }
}

pub enum LiteralValue {
    BooleanLiteral(bool),
    NumberLiteral(f64),
    StringLiteral(String),
}

pub struct Literal {
    pub value: LiteralValue,
}

impl Expression for Literal {
    fn evaluate<'a, 'd>(&self, _: &EvaluationContext<'a, 'd>) -> Value<'d> {
        match &self.value {
            &BooleanLiteral(b) => Boolean(b),
            &NumberLiteral(b) => Number(b),
            &StringLiteral(ref b) => Value::String(b.clone()),
        }
    }
}

pub struct Math {
    left:  SubExpression,
    right: SubExpression,
    operation: fn(f64, f64) -> f64,
}

fn      add(a: f64, b: f64) -> f64 {a + b}
fn subtract(a: f64, b: f64) -> f64 {a - b}
fn multiply(a: f64, b: f64) -> f64 {a * b}
fn   divide(a: f64, b: f64) -> f64 {a / b}
fn  modulus(a: f64, b: f64) -> f64 {a % b}

impl Math {
    pub fn addition(left: SubExpression, right: SubExpression) -> SubExpression {
        box Math{left: left, right: right, operation: add}
    }

    pub fn subtraction(left: SubExpression, right: SubExpression) -> SubExpression {
        box Math{left: left, right: right, operation: subtract}
    }

    pub fn multiplication(left: SubExpression, right: SubExpression) -> SubExpression {
        box Math{left: left, right: right, operation: multiply}
    }

    pub fn division(left: SubExpression, right: SubExpression) -> SubExpression {
        box Math{left: left, right: right, operation: divide}
    }

    pub fn remainder(left: SubExpression, right: SubExpression) -> SubExpression {
        box Math{left: left, right: right, operation: modulus}
    }
}

impl Expression for Math {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        let left = self.left.evaluate(context);
        let right = self.right.evaluate(context);
        let op = self.operation;
        return Number(op(left.number(), right.number()));
    }
}

pub struct Negation {
    pub expression: SubExpression,
}

impl Expression for Negation {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        let result = self.expression.evaluate(context);
        return Number(-result.number());
    }
}

pub struct Or {
    left:  SubExpression,
    right: SubExpression,
}

binary_constructor!(Or);

impl Expression for Or {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        return Boolean(self.left.evaluate(context).boolean() ||
                       self.right.evaluate(context).boolean())
    }
}

pub struct Path {
    start_point: SubExpression,
    steps: Vec<SubExpression>,
}

impl Path {
    pub fn new(start_point: SubExpression, steps: Vec<SubExpression>) -> SubExpression {
        box Path {start_point: start_point, steps: steps}
    }
}

impl Expression for Path {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        let mut result = self.start_point.evaluate(context).nodeset();

        for step in self.steps.iter() {
            let mut step_result = Nodeset::new();

            let mut sub_context = context.new_context_for(result.size());

            for current_node in result.iter() {
                sub_context.next(*current_node);
                let selected = step.evaluate(&sub_context);
                // TODO: What if it is not a nodeset?
                step_result.add_nodeset(&selected.nodeset());
            }

            result = step_result;
        }

        Nodes(result)
    }
}

pub struct Predicate {
    node_selector: SubExpression,
    predicate: SubExpression,
}

impl Predicate {
    pub fn new(node_selector: SubExpression, predicate: SubExpression) -> SubExpression {
        box Predicate { node_selector: node_selector, predicate: predicate }
    }

    fn include<'a, 'd>(value: &Value, context: &EvaluationContext<'a, 'd>) -> bool {
        match value {
            &Number(v) => context.position() == v as uint,
            _ => value.boolean()
        }
    }
}

impl Expression for Predicate {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        let mut selected = Nodeset::new();

        let nodes = self.node_selector.evaluate(context).nodeset();

        let mut sub_context = context.new_context_for(nodes.size());

        for current_node in nodes.iter() {
            sub_context.next(*current_node);

            let value = self.predicate.evaluate(&sub_context);

            if Predicate::include(&value, &sub_context) {
                selected.add(*current_node);
            }
        }

        Nodes(selected)
    }
}

pub struct Relational {
    pub  left: SubExpression,
    pub right: SubExpression,
    pub operation: fn(f64, f64) -> bool,
}

fn             less_than(left: f64, right: f64) -> bool { left <  right }
fn    less_than_or_equal(left: f64, right: f64) -> bool { left <= right }
fn          greater_than(left: f64, right: f64) -> bool { left >  right }
fn greater_than_or_equal(left: f64, right: f64) -> bool { left >= right }

impl Relational {
    pub fn less_than(left: SubExpression, right: SubExpression) -> SubExpression
    {
        box Relational{left: left, right: right, operation: less_than}
    }

    pub fn less_than_or_equal(left: SubExpression, right: SubExpression) -> SubExpression
    {
        box Relational{left: left, right: right, operation: less_than_or_equal}
    }

    pub fn greater_than(left: SubExpression, right: SubExpression) -> SubExpression
    {
        box Relational{left: left, right: right, operation: greater_than}
    }

    pub fn greater_than_or_equal(left: SubExpression, right: SubExpression) -> SubExpression
    {
        box Relational{left: left, right: right, operation: greater_than_or_equal}
    }
}

impl Expression for Relational {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        let left_val = self.left.evaluate(context);
        let right_val = self.right.evaluate(context);
        let op = self.operation;
        Boolean(op(left_val.number(), right_val.number()))
    }
}

#[allow(missing_copy_implementations)]
pub struct RootNode;

impl Expression for RootNode {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        Nodes(nodeset![context.node.document().root()])
    }
}

pub type StepAxis = Box<Axis + 'static>;
pub type StepTest = Box<NodeTest + 'static>;

pub struct Step {
    axis: StepAxis,
    node_test: StepTest,
}

impl Step {
    pub fn new(axis: StepAxis, node_test: StepTest) -> SubExpression {
        box Step {axis: axis, node_test: node_test}
    }
}

impl Expression for Step {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        let mut result = Nodeset::new();
        self.axis.select_nodes(context, &*self.node_test, &mut result);
        Nodes(result)
    }
}

pub struct Union {
    pub left:  SubExpression,
    pub right: SubExpression,
}

binary_constructor!(Union);

impl Expression for Union {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        let mut left_val = self.left.evaluate(context).nodeset();
        let right_val = self.right.evaluate(context).nodeset();
        left_val.add_nodeset(&right_val);
        Nodes(left_val)
    }
}

pub struct Variable {
    pub name: String,
}

impl Expression for Variable {
    fn evaluate<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>) -> Value<'d> {
        match context.value_of(self.name.as_slice()) {
            Some(v) => v.clone(),
            None => panic!("throw UnknownVariableException(_name)"),
        }
    }
}

#[cfg(test)]
    mod test {
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::collections::HashMap;

    use document::Package;
    use document::dom4::Document;

    use super::super::Value;
    use super::super::Value::{Boolean, Number, String, Nodes};
    use super::super::{Functions,Variables};
    use super::super::Function;
    use super::super::EvaluationContext;
    use super::super::nodeset::Nodeset;
    use super::super::axis::Axis;
    use super::super::node_test::NodeTest;

    use super::super::expression;
    use super::Expression;
        use super::{
            And,
            Equal,
            NotEqual,
            Literal,
            Math,
            Predicate,
            Relational,
            RootNode,
            Step,
            Union,
            Variable
        };
    use super::LiteralValue::{BooleanLiteral,NumberLiteral,StringLiteral};

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

        let left  = box Variable{name: "left".to_string()};
        let right = box Variable{name: "right".to_string()};

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

        let left  = box Variable{name: "left".to_string()};
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

        let left  = box Variable{name: "left".to_string()};
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

        let expr = NotEqual::new(a_str, b_str);

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

        let selected_nodes = box Variable{name: "nodes".to_string()};
        let predicate = box Literal{value: NumberLiteral(1.0)};

        let expr = Predicate::new(selected_nodes, predicate);

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

        let selected_nodes = box Variable{name: "nodes".to_string()};
        let predicate = box Literal{value: BooleanLiteral(false)};

        let expr = Predicate::new(selected_nodes, predicate);

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

        let expr = Relational::less_than(left, right);

        let context = setup.context();
        let res = expr.evaluate(&context);
        assert_eq!(res, Boolean(false));
    }

    #[test]
    fn expression_root_node_finds_the_root() {
        let package = Package::new();
        let setup = Setup::new(&package);

        let expr = RootNode;

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

        let expr = Step::new(box axis.clone(), box node_test);

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
        let left = box Variable{name: "left".to_string()};

        let right_node = setup.doc.create_element("right");
        let nodes = nodeset![right_node];
        setup.vars.insert("right".to_string(), Nodes(nodes));
        let right = box Variable{name: "right".to_string()};

        let expr = Union{left: left, right: right};

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(Nodes(nodeset![left_node, right_node]), res);
    }

    #[test]
    fn expression_variable_looks_up_the_variable() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        setup.vars.insert("foo".to_string(), Boolean(true));

        let expr = Variable{name: "foo".to_string()};

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(Boolean(true), res);
    }
}

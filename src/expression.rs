use std::collections::HashSet;
use std::fmt;

use ::{LiteralValue, Value};
use ::Value::{Boolean, Number};
use ::axis::{Axis, AxisLike};
use ::context;
use ::function;
use ::node_test::NodeTest;
use ::nodeset::Nodeset;

quick_error! {
    #[derive(Debug, Clone, PartialEq, Hash)]
    pub enum Error {
        NotANodeset {
            description("expression did not evaluate to a nodeset")
        }
        UnknownFunction(name: String) {
            description("unknown function")
            display("unknown function {}", name)
        }
        UnknownVariable(name: String) {
            description("unknown variable")
            display("unknown variable {}", name)
        }
        FunctionEvaluation(err: function::Error) {
            from()
            cause(err)
            description(err.description())
            display("error while evaluating function: {}", err)
        }
    }
}

fn value_into_nodeset(v: Value) -> Result<Nodeset, Error> {
    match v {
        Value::Nodeset(ns) => Ok(ns),
        _ => Err(Error::NotANodeset),
    }
}

/// The interface of a compiled XPath.
pub trait Expression: fmt::Debug {
    /// Evaluate this expression in the given context.
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error>;
}

pub type SubExpression = Box<Expression + 'static>;

macro_rules! binary_constructor(
    ($t:ident) => (
        impl $t {
            pub fn new(left: SubExpression, right: SubExpression) -> SubExpression {
                Box::new($t{left: left, right: right})
            }
        }
    );
);

#[derive(Debug)]
pub struct And {
    pub left:  SubExpression,
    pub right: SubExpression,
}

binary_constructor!(And);

impl Expression for And {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        let left = try!(self.left.evaluate(context)).boolean();
        let v = left && try!(self.right.evaluate(context)).boolean();
        Ok(Boolean(v))
    }
}

#[allow(missing_copy_implementations)]
#[derive(Debug)]
pub struct ContextNode;

impl Expression for ContextNode {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        Ok(Value::Nodeset(nodeset![context.node]))
    }
}

#[derive(Debug)]
pub struct Equal {
    pub left:  SubExpression,
    pub right: SubExpression,
}

binary_constructor!(Equal);

impl Equal {
    fn boolean_evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<bool, Error> {
        let left_val = try!(self.left.evaluate(context));
        let right_val = try!(self.right.evaluate(context));

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

        let v = match (&left_val, &right_val) {
            (&Value::Nodeset(ref left_nodes), &Value::Nodeset(ref right_nodes)) => {
                let left_strings = str_vals(left_nodes);
                let right_strings = str_vals(right_nodes);
                !left_strings.is_disjoint(&right_strings)
            },
            (&Value::Nodeset(ref nodes), &Number(val)) |
            (&Number(val), &Value::Nodeset(ref nodes)) => {
                let numbers = num_vals(nodes);
                numbers.iter().any(|n| *n == val)
            },
            (&Value::Nodeset(ref nodes), &Value::String(ref val)) |
            (&Value::String(ref val), &Value::Nodeset(ref nodes)) => {
                let strings = str_vals(nodes);
                strings.contains(val)
            },
            (&Boolean(_), _) |
            (_, &Boolean(_)) => left_val.boolean() == right_val.boolean(),
            (&Number(_), _) |
            (_, &Number(_)) => left_val.number() == right_val.number(),
            _ => left_val.string() == right_val.string()
        };

        Ok(v)
    }
}

impl Expression for Equal {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        self.boolean_evaluate(context).map(Boolean)
    }
}

#[derive(Debug)]
pub struct NotEqual {
    equal: Equal,
}

impl NotEqual {
    pub fn new(left: SubExpression, right: SubExpression) -> SubExpression {
        Box::new(NotEqual {
            equal: Equal{left: left, right: right}
        })
    }
}

impl Expression for NotEqual {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        self.equal.boolean_evaluate(context).map(|v| Boolean(!v))
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<SubExpression>,
}

impl Expression for Function {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        context.function_for_name(&self.name)
            .ok_or_else(|| Error::UnknownFunction(self.name.clone()))
            .and_then(|fun| {
                let args = try!(self.arguments.iter().map(|arg| arg.evaluate(context)).collect());
                fun.evaluate(context, args).map_err(Error::FunctionEvaluation)
            })
    }
}

#[derive(Debug)]
pub struct Literal {
    value: LiteralValue,
}

impl From<LiteralValue> for Literal {
    fn from(other: LiteralValue) -> Literal {
        Literal { value: other }
    }
}

impl Expression for Literal {
    fn evaluate<'a, 'd>(&self, _: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        Ok(self.value.clone().into())
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
        Box::new(Math{left: left, right: right, operation: add})
    }

    pub fn subtraction(left: SubExpression, right: SubExpression) -> SubExpression {
        Box::new(Math{left: left, right: right, operation: subtract})
    }

    pub fn multiplication(left: SubExpression, right: SubExpression) -> SubExpression {
        Box::new(Math{left: left, right: right, operation: multiply})
    }

    pub fn division(left: SubExpression, right: SubExpression) -> SubExpression {
        Box::new(Math{left: left, right: right, operation: divide})
    }

    pub fn remainder(left: SubExpression, right: SubExpression) -> SubExpression {
        Box::new(Math{left: left, right: right, operation: modulus})
    }
}

impl Expression for Math {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        let left = try!(self.left.evaluate(context));
        let right = try!(self.right.evaluate(context));
        let op = self.operation;
        Ok(Number(op(left.number(), right.number())))
    }
}

impl fmt::Debug for Math {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Math {{ left: {:?}, right: {:?} }}", self.left, self.right)
    }
}

#[derive(Debug)]
pub struct Negation {
    pub expression: SubExpression,
}

impl Expression for Negation {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        self.expression.evaluate(context).map(|r| Number(-r.number()))
    }
}

#[derive(Debug)]
pub struct Or {
    left:  SubExpression,
    right: SubExpression,
}

binary_constructor!(Or);

impl Expression for Or {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        let left = try!(self.left.evaluate(context)).boolean();
        let v = left || try!(self.right.evaluate(context)).boolean();
        Ok(Boolean(v))
    }
}

#[derive(Debug)]
pub struct Path {
    start_point: SubExpression,
    steps: Vec<Step>,
}

impl Path {
    pub fn new(start_point: SubExpression, steps: Vec<Step>) -> SubExpression {
        Box::new(Path {start_point: start_point, steps: steps})
    }
}

impl Expression for Path {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        let result = try!(self.start_point.evaluate(context));
        let mut result = try!(value_into_nodeset(result));

        for step in &self.steps {
            result = try!(step.evaluate(context, result));
        }

        Ok(Value::Nodeset(result))
    }
}

#[derive(Debug)]
pub struct Filter {
    node_selector: SubExpression,
    predicate: Predicate,
}

impl Filter {
    pub fn new(node_selector: SubExpression, predicate: SubExpression) -> SubExpression {
        let predicate = Predicate { expression: predicate };
        Box::new(Filter { node_selector: node_selector, predicate: predicate })
    }
}

impl Expression for Filter {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        self.node_selector.evaluate(context)
            .and_then(value_into_nodeset)
            .and_then(|nodes| self.predicate.select(context, nodes))
            .and_then(|nodes| Ok(Value::Nodeset(nodes)))
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
        Box::new(Relational{left: left, right: right, operation: less_than})
    }

    pub fn less_than_or_equal(left: SubExpression, right: SubExpression) -> SubExpression
    {
        Box::new(Relational{left: left, right: right, operation: less_than_or_equal})
    }

    pub fn greater_than(left: SubExpression, right: SubExpression) -> SubExpression
    {
        Box::new(Relational{left: left, right: right, operation: greater_than})
    }

    pub fn greater_than_or_equal(left: SubExpression, right: SubExpression) -> SubExpression
    {
        Box::new(Relational{left: left, right: right, operation: greater_than_or_equal})
    }
}

impl Expression for Relational {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        let left_val = try!(self.left.evaluate(context));
        let right_val = try!(self.right.evaluate(context));
        let op = self.operation;
        Ok(Boolean(op(left_val.number(), right_val.number())))
    }
}

impl fmt::Debug for Relational {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Relational {{ left: {:?}, right: {:?} }}", self.left, self.right)
    }
}

#[allow(missing_copy_implementations)]
#[derive(Debug)]
pub struct RootNode;

impl Expression for RootNode {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        Ok(Value::Nodeset(nodeset![context.node.document().root()]))
    }
}

#[derive(Debug)]
struct Predicate {
    pub expression: SubExpression
}

impl Predicate {
    fn select<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>, nodes: Nodeset<'d>)
                      -> Result<Nodeset<'d>, Error>
    {
        context.predicate_iter(nodes).filter_map(|ctx| {
            match self.matches(&ctx) {
                Ok(true) => Some(Ok(ctx)),
                Ok(false) => None,
                Err(e) => Some(Err(e)),
            }
        }).collect()
    }

    fn matches(&self, context: &context::Evaluation) -> Result<bool, Error> {
        let value = try!(self.expression.evaluate(context));

        let v = match value {
            Number(v) => context.position() == v as usize,
            _ => value.boolean()
        };

        Ok(v)
    }
}

pub type Step = ParameterizedStep<Axis>;
pub type StepTest = Box<NodeTest + 'static>;

#[derive(Debug)]
pub struct ParameterizedStep<A> {
    axis: A,
    node_test: StepTest,
    predicates: Vec<Predicate>,
}

impl<A> ParameterizedStep<A>
    where A: AxisLike,
{
    pub fn new(axis: A, node_test: StepTest, predicates: Vec<SubExpression>) -> ParameterizedStep<A> {
        let preds = predicates.into_iter().map(|p| Predicate { expression: p }).collect();
        ParameterizedStep { axis: axis, node_test: node_test, predicates: preds }
    }

    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>, starting_nodes: Nodeset<'d>)
                        -> Result<Nodeset<'d>, Error>
    {
        // For every starting node, we collect new nodes based on the
        // axis and node-test. We evaluate the predicates on the total
        // set of new nodes.

        self.apply_predicates(context, self.apply_axis(context, starting_nodes))
    }

    fn apply_axis<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>, starting_nodes: Nodeset<'d>)
                        -> Nodeset<'d>
    {
        let mut result = Nodeset::new();

        for node in starting_nodes.iter() {
            let child_context = context.new_context_for(node);
            self.axis.select_nodes(&child_context, &*self.node_test, &mut result);
        }

        result
    }

    fn apply_predicates<'a, 'd>(&self,
                                context: &context::Evaluation<'a, 'd>,
                                nodes: Nodeset<'d>)
                                -> Result<Nodeset<'d>, Error>
    {
        let mut nodes = nodes;

        for predicate in &self.predicates {
            nodes = try!(predicate.select(context, nodes));
        }

        Ok(nodes)
    }
}

#[derive(Debug)]
pub struct Union {
    pub left:  SubExpression,
    pub right: SubExpression,
}

binary_constructor!(Union);

impl Expression for Union {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        let as_nodes = |e: &SubExpression| e.evaluate(context).and_then(value_into_nodeset);

        let mut left_nodes = try!(as_nodes(&self.left));
        let right_nodes = try!(as_nodes(&self.right));

        left_nodes.add_nodeset(&right_nodes);
        Ok(Value::Nodeset(left_nodes))
    }
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
}

impl Expression for Variable {
    fn evaluate<'a, 'd>(&self, context: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
        context.value_of(&self.name)
            .cloned()
            .ok_or_else(|| Error::UnknownVariable(self.name.clone()))
    }
}

#[cfg(test)]
mod test {
    use std::borrow::ToOwned;
    use std::cell::RefCell;
    use std::rc::Rc;

    use sxd_document::Package;
    use sxd_document::dom::Document;

    use ::{LiteralValue, Value};
    use ::Value::{Boolean, Number, String};
    use ::axis::AxisLike;
    use ::context;
    use ::function;
    use ::node_test::NodeTest;
    use ::nodeset::Nodeset;

    use super::*;

    #[derive(Debug)]
    struct FailExpression;
    impl Expression for FailExpression {
        fn evaluate<'a, 'd>(&self, _: &context::Evaluation<'a, 'd>) -> Result<Value<'d>, Error> {
            panic!("Should never be called");
        }
    }

    struct Setup<'d> {
        doc: Document<'d>,
        context: context::Core<'d>,
    }

    impl<'d> Setup<'d> {
        fn new(package: &'d Package) -> Setup<'d> {
            Setup {
                doc: package.as_document(),
                context: context::Core::without_core_functions(),
            }
        }

        fn context(&'d self) -> context::Evaluation<'d, 'd> {
            let node = self.doc.create_element("test");
            self.context.borrow_with_context_node(node).evaluation_context()
        }
    }

    #[test]
    fn expression_and_returns_logical_and() {
        let package = Package::new();
        let setup = Setup::new(&package);

        let left  = Box::new(Literal{value: LiteralValue::Boolean(true)});
        let right = Box::new(Literal{value: LiteralValue::Boolean(true)});

        let expr = And{left: left, right: right};

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Boolean(true)));
    }

    #[test]
    fn expression_and_short_circuits_when_left_argument_is_false() {
        let package = Package::new();
        let setup = Setup::new(&package);

        let left  = Box::new(Literal{value: LiteralValue::Boolean(false)});
        let right = Box::new(FailExpression);

        let expr = And{left: left, right: right};

        let context = setup.context();
        expr.evaluate(&context).ok().unwrap();
    }

    #[test]
    fn expression_equal_intersects_string_values_of_two_nodesets() {
        let package = Package::new();
        let mut setup = Setup::new(&package);

        let string_value_1 = setup.doc.create_text("same");
        let string_value_2 = setup.doc.create_text("same");

        setup.context.set_variable("left", Value::Nodeset(nodeset![string_value_1]));
        setup.context.set_variable("right", Value::Nodeset(nodeset![string_value_2]));

        let left  = Box::new(Variable{name: "left".to_owned()});
        let right = Box::new(Variable{name: "right".to_owned()});

        let expr = Equal{left: left, right: right};

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Boolean(true)));
    }

    #[test]
    fn expression_equal_compares_number_value_of_nodeset_to_number() {
        let package = Package::new();
        let mut setup = Setup::new(&package);

        let string_value = setup.doc.create_text("3.14");
        setup.context.set_variable("left", Value::Nodeset(nodeset![string_value]));

        let left  = Box::new(Variable{name: "left".to_owned()});
        let right = Box::new(Literal{value: LiteralValue::Number(6.28)});

        let expr = Equal{left: left, right: right};

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Boolean(false)));
    }

    #[test]
    fn expression_equal_compares_string_value_of_nodeset_to_string() {
        let package = Package::new();
        let mut setup = Setup::new(&package);

        let string_value_1 = setup.doc.create_text("gravy");
        let string_value_2 = setup.doc.create_text("boat");
        setup.context.set_variable("left", Value::Nodeset(nodeset![string_value_1, string_value_2]));

        let left  = Box::new(Variable{name: "left".to_owned()});
        let right = Box::new(Literal{value: LiteralValue::String("boat".to_owned())});

        let expr = Equal{left: left, right: right};

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Boolean(true)));
    }

    #[test]
    fn expression_equal_compares_as_boolean_if_one_argument_is_a_boolean() {
        let package = Package::new();
        let setup = Setup::new(&package);

        let actual_bool = Box::new(Literal{value: LiteralValue::Boolean(false)});
        let truthy_str = Box::new(Literal{value: LiteralValue::String("hello".to_owned())});

        let expr = Equal{left: actual_bool, right: truthy_str};

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Boolean(false)));
    }

    #[test]
    fn expression_equal_compares_as_number_if_one_argument_is_a_number() {
        let package = Package::new();
        let setup = Setup::new(&package);

        let actual_number = Box::new(Literal{value: LiteralValue::Number(-42.0)});
        let number_str = Box::new(Literal{value: LiteralValue::String("-42.0".to_owned())});

        let expr = Equal{left: number_str, right: actual_number};

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Boolean(true)));
    }

    #[test]
    fn expression_equal_compares_as_string_otherwise() {
        let package = Package::new();
        let setup = Setup::new(&package);

        let a_str = Box::new(Literal{value: LiteralValue::String("hello".to_owned())});
        let b_str = Box::new(Literal{value: LiteralValue::String("World".to_owned())});

        let expr = Equal{left: a_str, right: b_str};

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Boolean(false)));
    }

    #[test]
    fn expression_not_equal_negates_equality() {
        let package = Package::new();
        let setup = Setup::new(&package);

        let a_str = Box::new(Literal{value: LiteralValue::Boolean(true)});
        let b_str = Box::new(Literal{value: LiteralValue::Boolean(false)});

        let expr = NotEqual::new(a_str, b_str);

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Boolean(true)));
    }

    struct StubFunction {
        value: &'static str,
    }

    impl function::Function for StubFunction {
        fn evaluate<'a, 'd>(&self,
                            _: &context::Evaluation<'a, 'd>,
                            _: Vec<Value<'d>>) -> Result<Value<'d>, function::Error>
        {
            Ok(String(self.value.to_owned()))
        }
    }

    #[test]
    fn expression_function_evaluates_input_arguments() {
        let package = Package::new();
        let mut setup = Setup::new(&package);

        let arg_expr: Box<Expression> = Box::new(Literal{value: LiteralValue::Boolean(true)});
        setup.context.set_function("test-fn", StubFunction { value: "the function ran" });

        let expr = Function { name: "test-fn".to_owned(), arguments: vec![arg_expr] };

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(String("the function ran".to_owned())));
    }

    #[test]
    fn expression_function_unknown_function_is_reported_as_an_error() {
        let package = Package::new();
        let setup = Setup::new(&package);

        let expr = Function { name: "unknown-fn".to_owned(), arguments: vec![] };

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Err(Error::UnknownFunction("unknown-fn".to_owned())));
    }

    #[test]
    fn expression_math_does_basic_math() {
        let package = Package::new();
        let setup = Setup::new(&package);

        let left  = Box::new(Literal{value: LiteralValue::Number(10.0)});
        let right = Box::new(Literal{value: LiteralValue::Number(5.0)});

        let expr = Math::multiplication(left, right);

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Number(50.0)));
    }

    #[test]
    fn filter_with_numeric_predicate_selects_that_node() {
        let package = Package::new();
        let mut setup = Setup::new(&package);

        let input_node_1 = setup.doc.create_element("one");
        let input_node_2 = setup.doc.create_element("two");
        let input_nodeset = nodeset![input_node_1, input_node_2];

        setup.context.set_variable("nodes", Value::Nodeset(input_nodeset));

        let selected_nodes = Box::new(Variable{name: "nodes".to_owned()});
        let predicate = Box::new(Literal{value: LiteralValue::Number(1.0)});

        let expr = Filter::new(selected_nodes, predicate);

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Value::Nodeset(nodeset![input_node_1])));
    }

    #[test]
    fn filter_with_false_predicate_selects_no_nodes() {
        let package = Package::new();
        let mut setup = Setup::new(&package);

        let input_node_1 = setup.doc.create_element("one");
        let input_node_2 = setup.doc.create_element("two");
        let input_nodeset = nodeset![input_node_1, input_node_2];

        setup.context.set_variable("nodes", Value::Nodeset(input_nodeset));

        let selected_nodes = Box::new(Variable{name: "nodes".to_owned()});
        let predicate = Box::new(Literal{value: LiteralValue::Boolean(false)});

        let expr = Filter::new(selected_nodes, predicate);

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Value::Nodeset(nodeset![])));
    }

    #[test]
    fn expression_relational_does_basic_comparisons() {
        let package = Package::new();
        let setup = Setup::new(&package);

        let left  = Box::new(Literal{value: LiteralValue::Number(10.0)});
        let right = Box::new(Literal{value: LiteralValue::Number(5.0)});

        let expr = Relational::less_than(left, right);

        let context = setup.context();
        let res = expr.evaluate(&context);
        assert_eq!(res, Ok(Boolean(false)));
    }

    #[test]
    fn expression_root_node_finds_the_root() {
        let package = Package::new();
        let setup = Setup::new(&package);

        let expr = RootNode;

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Value::Nodeset(nodeset![setup.doc.root()])));
    }

    #[derive(Debug, Clone)]
    struct MockAxis {
        calls: Rc<RefCell<usize>>,
    }

    impl MockAxis {
        fn new() -> MockAxis {
            MockAxis{ calls: Rc::new(RefCell::new(0)) }
        }

        fn calls(&self) -> usize {
            *self.calls.borrow()
        }
    }

    impl AxisLike for MockAxis {
        fn select_nodes(&self,
                        _context:   &context::Evaluation,
                        _node_test: &NodeTest,
                        _result:    &mut Nodeset)
        {
            *self.calls.borrow_mut() += 1;
        }
    }

    #[derive(Debug)]
    struct DummyNodeTest;
    impl NodeTest for DummyNodeTest {
        fn test(&self, _context: &context::Evaluation, _result: &mut Nodeset) {
        }
    }

    #[test]
    fn step_delegates_to_the_axis() {
        let package = Package::new();
        let setup = Setup::new(&package);

        let axis = MockAxis::new();
        let node_test = DummyNodeTest;

        let expr = ParameterizedStep::new(axis.clone(), Box::new(node_test), vec![]);

        let context = setup.context();
        expr.evaluate(&context, nodeset![context.node]).ok().unwrap();

        assert_eq!(1, axis.calls());
    }

    #[test]
    fn expression_union_combines_nodesets() {
        let package = Package::new();
        let mut setup = Setup::new(&package);

        let left_node = setup.doc.create_element("left");
        let nodes = nodeset![left_node];
        setup.context.set_variable("left", Value::Nodeset(nodes));
        let left = Box::new(Variable{name: "left".to_owned()});

        let right_node = setup.doc.create_element("right");
        let nodes = nodeset![right_node];
        setup.context.set_variable("right", Value::Nodeset(nodes));
        let right = Box::new(Variable{name: "right".to_owned()});

        let expr = Union{left: left, right: right};

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Value::Nodeset(nodeset![left_node, right_node])));
    }

    #[test]
    fn expression_variable_looks_up_the_variable() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        setup.context.set_variable("foo", Boolean(true));

        let expr = Variable{name: "foo".to_owned()};

        let context = setup.context();
        let res = expr.evaluate(&context);

        assert_eq!(res, Ok(Boolean(true)));
    }
}

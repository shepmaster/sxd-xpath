//! Support for visiting XPaths.

use crate::axis::Axis;
use crate::expression::{Expression, Step, StepTest, Predicate, SubExpression};
use crate::{LiteralValue, OwnedPrefixedName};

pub trait Visitable {
    fn visit(&self, visitor: &mut dyn Visitor);
}

impl<T: ?Sized> Visitable for Box<T>
where
    T: Visitable,
{
    fn visit(&self, visitor: &mut dyn Visitor) {
        (**self).visit(visitor)
    }
}

pub trait Visitor {
    fn visit_and(&mut self, left: &SubExpression, right: &SubExpression);

    fn visit_attribute(&mut self, prefix: Option<&str>, local_part: &str);

    fn visit_comment(&mut self);

    fn visit_context_node(&mut self);

    fn visit_element(&mut self, prefix: Option<&str>, local_part: &str);

    fn visit_equal(&mut self, left: &SubExpression, right: &SubExpression);

    fn visit_filter(&mut self, node_selector: &SubExpression, predicate: &Predicate);

    fn visit_function(&mut self, name: &OwnedPrefixedName, arguments: &[SubExpression]);

    fn visit_literal(&mut self, value: &LiteralValue);

    fn visit_math(
        &mut self,
        left: &SubExpression,
        right: &SubExpression,
        operation: &fn(f64, f64) -> f64,
    );

    fn visit_namespace(&mut self, prefix: Option<&str>, local_part: &str);

    fn visit_node(&mut self);

    fn visit_negation(&mut self, expression: &SubExpression);

    fn visit_not_equal(&mut self, left: &SubExpression, right: &SubExpression);

    fn visit_or(&mut self, left: &SubExpression, right: &SubExpression);

    fn visit_path(&mut self, start_point: &SubExpression, steps: &[Step]);

    fn visit_processing_instruction(&mut self, target: Option<&str>);

    fn visit_relational(
        &mut self,
        left: &SubExpression,
        right: &SubExpression,
        operation: &fn(f64, f64) -> bool,
    );

    fn visit_root_node(&mut self);

    fn visit_step(&mut self, axis: &Axis, node_test: &StepTest, predicates: &[Predicate]);
    
    fn visit_text(&mut self);

    fn visit_union(&mut self, left: &SubExpression, right: &SubExpression);

    fn visit_variable(&mut self, name: &OwnedPrefixedName);

    fn visit_xpath(&mut self, xpath: &std::boxed::Box<(dyn Expression + 'static)>);
}

#[cfg(test)]
mod test {
    use crate::{Factory, LiteralValue, OwnedPrefixedName};
    use crate::axis::Axis;
    use crate::expression::{Expression, Predicate, Step, StepTest, SubExpression};
    use crate::node_test::{Element, NameTest};
    use crate::visitor::{Visitable, Visitor};

    #[derive(Debug)]
    enum Node {
	XPath(Box<Node>),
	Path(Box<Node>, Vec<Node>),
	RootNode,
	None,
	Step(Axis, StepTest, Vec<Node>),
    }

    impl Visitor for Node {
	fn visit_and(&mut self, _left: &SubExpression, _right: &SubExpression) { }

	fn visit_attribute(&mut self, _prefix: Option<&str>, _local_part: &str) { }

	fn visit_comment(&mut self) { }

	fn visit_context_node(&mut self) { }

	fn visit_element(&mut self, _prefix: Option<&str>, _local_part: &str) { }

	fn visit_equal(&mut self, _left: &SubExpression, _right: &SubExpression) { }

	fn visit_filter(&mut self, _node_selector: &SubExpression, _predicate: &Predicate) { }

	fn visit_function(&mut self, _name: &OwnedPrefixedName, _arguments: &[SubExpression]) { }

	fn visit_literal(&mut self, _value: &LiteralValue) { }

	fn visit_math(
	    &mut self,
	    _left: &SubExpression,
	    _right: &SubExpression,
	    _operation: &fn(f64, f64) -> f64,
	) { }

	fn visit_namespace(&mut self, _prefix: Option<&str>, _local_part: &str) { }

	fn visit_node(&mut self) { }

	fn visit_negation(&mut self, _expression: &SubExpression) { }

	fn visit_not_equal(&mut self, _left: &SubExpression, _right: &SubExpression) { }

	fn visit_or(&mut self, _left: &SubExpression, _right: &SubExpression) { }

	fn visit_path(&mut self, start_point: &SubExpression, steps: &[Step]) {
	    let mut start_point_node = Node::None;
	    start_point.visit(&mut start_point_node);

	    let step_nodes = steps.iter().map(|step| {
		let mut step_node = Node::None;
		step.visit(&mut step_node);
		step_node
	    }).collect::<Vec<_>>();
	    
	    *self = Node::Path(Box::new(start_point_node), step_nodes);
	}

	fn visit_processing_instruction(&mut self, _target: Option<&str>) { }

	fn visit_relational(
	    &mut self,
	    _left: &SubExpression,
	    _right: &SubExpression,
	    _operation: &fn(f64, f64) -> bool,
	) { }

	fn visit_root_node(&mut self) {
	    *self = Node::RootNode;
        }

	fn visit_step(&mut self, axis: &Axis, node_test: &StepTest, predicates: &[Predicate]) {
	    let predicate_nodes = predicates.iter().map(|predicate| {
		let mut predicate_node = Node::None;
		predicate.expression.visit(&mut predicate_node);
		predicate_node
	    }).collect::<Vec<_>>();

	    *self = Node::Step(axis.clone(), node_test.clone_box(), predicate_nodes);
	}

	fn visit_text(&mut self) { }

	fn visit_union(&mut self, _left: &SubExpression, _right: &SubExpression) { }

	fn visit_variable(&mut self, _name: &OwnedPrefixedName) { }

	fn visit_xpath(&mut self, xpath: &std::boxed::Box<(dyn Expression + 'static)>) {
	    let mut xpath_node = Node::None;
	    xpath.visit(&mut xpath_node);

	    *self = Node::XPath(Box::new(xpath_node));
	}
    }

    #[test]
    fn visit() {
	let xpath = Factory::new().build("/root").expect("Could not compile XPath");

	let mut node = Node::None;
	xpath.visit(&mut node);

	assert_eq!(
	    format!("{:?}", node),
	    format!("{:?}", Node::XPath(
		Box::new(Node::Path(
		    Box::new(Node::RootNode),
		    vec![
			Node::Step(
			    Axis::Child,
			    Box::new(Element::new(NameTest {
				prefix: None,
				local_part: "root".to_string()
			    })),
			    Vec::new()
			)
		    ],
		))
	    )),
	);
    }
}

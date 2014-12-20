extern crate document;

use self::PrincipalNodeType::*;

use super::EvaluationContext;
use super::node_test::XPathNodeTest;
use super::nodeset::Nodeset;
use super::nodeset::Node::ElementNode;

#[allow(missing_copy_implementations)]
pub enum PrincipalNodeType {
    Attribute,
    Element,
}

/// A directed traversal of Nodes.
pub trait Axis {
    /// Applies the given node test to the nodes selected by this axis,
    /// adding matching nodes to the nodeset.
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &XPathNodeTest,
                            result:    &mut Nodeset<'d>);

    /// Describes what node type is naturally selected by this axis.
    fn principal_node_type(&self) -> PrincipalNodeType {
        Element
    }
}

pub type SubAxis = Box<Axis + 'static>;

#[allow(missing_copy_implementations)]
pub struct AxisAttribute;

impl Axis for AxisAttribute {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &XPathNodeTest,
                            result:    &mut Nodeset<'d>)
    {
        if let ElementNode(ref e) = context.node {
            for attr in e.attributes().iter() {
                let mut attr_context = context.new_context_for(1);
                attr_context.next(*attr);

                node_test.test(&attr_context, result);
            }
        }
    }

    fn principal_node_type(&self) -> PrincipalNodeType {
        Attribute
    }
}

#[allow(missing_copy_implementations)]
pub struct AxisChild;

impl Axis for AxisChild {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &XPathNodeTest,
                            result:    &mut Nodeset<'d>)
    {
        let n = context.node;

        for child in n.children().iter() {
            let mut child_context = context.new_context_for(1);
            child_context.next(*child);

            node_test.test(&child_context, result);
        }
    }
}

#[allow(missing_copy_implementations)]
pub struct AxisDescendant;

impl Axis for AxisDescendant {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &XPathNodeTest,
                            result:    &mut Nodeset<'d>)
    {
        let n = context.node;

        for child in n.children().iter() {
            let mut child_context = context.new_context_for(1);
            child_context.next(*child);

            node_test.test(&child_context, result);
            self.select_nodes(&child_context, node_test, result);
        }
    }
}

#[allow(missing_copy_implementations)]
pub struct AxisDescendantOrSelf {
    descendant: AxisDescendant,
}

impl AxisDescendantOrSelf {
    pub fn new() -> SubAxis { box AxisDescendantOrSelf{descendant: AxisDescendant} }
}

impl Axis for AxisDescendantOrSelf {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &XPathNodeTest,
                            result:    &mut Nodeset<'d>)
    {
        node_test.test(context, result);
        self.descendant.select_nodes(context, node_test, result);
    }
}

#[allow(missing_copy_implementations)]
pub struct AxisParent;

impl Axis for AxisParent {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &XPathNodeTest,
                            result:    &mut Nodeset<'d>)
    {
        if let Some(p) = context.node.parent() {
            let mut parent_context = context.new_context_for(1);
            parent_context.next(p);
            node_test.test(&parent_context, result);
        }
    }
}

#[allow(missing_copy_implementations)]
pub struct AxisSelf;

impl Axis for AxisSelf {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &XPathNodeTest,
                            result:    &mut Nodeset<'d>)
    {
        node_test.test(context, result);
    }
}

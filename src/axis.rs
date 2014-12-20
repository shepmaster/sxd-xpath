extern crate document;

use super::EvaluationContext;
use super::node_test::NodeTest;
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
                            node_test: &NodeTest,
                            result:    &mut Nodeset<'d>);

    /// Describes what node type is naturally selected by this axis.
    fn principal_node_type(&self) -> PrincipalNodeType {
        PrincipalNodeType::Element
    }
}

pub type SubAxis = Box<Axis + 'static>;

#[allow(missing_copy_implementations)]
pub struct Attribute;

impl Axis for Attribute {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &NodeTest,
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
        PrincipalNodeType::Attribute
    }
}

#[allow(missing_copy_implementations)]
pub struct Child;

impl Axis for Child {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &NodeTest,
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
pub struct Descendant;

impl Axis for Descendant {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &NodeTest,
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
pub struct DescendantOrSelf {
    descendant: Descendant,
}

impl DescendantOrSelf {
    pub fn new() -> SubAxis { box DescendantOrSelf{descendant: Descendant} }
}

impl Axis for DescendantOrSelf {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &NodeTest,
                            result:    &mut Nodeset<'d>)
    {
        node_test.test(context, result);
        self.descendant.select_nodes(context, node_test, result);
    }
}

#[allow(missing_copy_implementations)]
pub struct Parent;

impl Axis for Parent {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &NodeTest,
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
pub struct Self;

impl Axis for Self {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &NodeTest,
                            result:    &mut Nodeset<'d>)
    {
        node_test.test(context, result);
    }
}

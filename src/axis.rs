extern crate document;

use document::Nodeset;
use document::ElementAny;

use super::XPathEvaluationContext;
use super::node_test::XPathNodeTest;

pub enum PrincipalNodeType {
  Attribute,
  Element,
}

/// A directed traversal of Nodes.
pub trait XPathAxis {
    /// Applies the given node test to the nodes selected by this axis,
    /// adding matching nodes to the nodeset.
    fn select_nodes(&self,
                    context:   &XPathEvaluationContext,
                    node_test: &XPathNodeTest,
                    result:    &mut Nodeset);

    /// Describes what node type is naturally selected by this axis.
    fn principal_node_type(&self) -> PrincipalNodeType {
        Element
    }
}

pub type SubAxis = Box<XPathAxis + 'static>;

pub struct AxisAttribute;

impl XPathAxis for AxisAttribute {
    fn select_nodes(&self,
                    context:   &XPathEvaluationContext,
                    node_test: &XPathNodeTest,
                    result:    &mut Nodeset)
    {
        let n = &context.node;

        match n {
            &ElementAny(ref e) =>
                for attr in e.attributes().iter() {
                    let mut attr_context = context.new_context_for(1);
                    attr_context.next(attr.clone());

                    node_test.test(&attr_context, result);
                },
            _ => {}
        }
    }

    fn principal_node_type(&self) -> PrincipalNodeType {
        Attribute
    }
}

pub struct AxisChild;

impl XPathAxis for AxisChild {
    fn select_nodes(&self,
                    context:   &XPathEvaluationContext,
                    node_test: &XPathNodeTest,
                    result:    &mut Nodeset)
    {
        let n = &context.node;

        for child in n.children().iter() {
            let mut child_context = context.new_context_for(1);
            child_context.next(child.clone());

            node_test.test(&child_context, result);
        }
    }
}

pub struct AxisDescendant;

impl XPathAxis for AxisDescendant {
    fn select_nodes(&self,
                    context:   &XPathEvaluationContext,
                    node_test: &XPathNodeTest,
                    result:    &mut Nodeset)
    {
        let n = &context.node;

        for child in n.children().iter() {
            let mut child_context = context.new_context_for(1);
            child_context.next(child.clone());

            node_test.test(&child_context, result);
            self.select_nodes(&child_context, node_test, result);
        }
    }
}

pub struct AxisDescendantOrSelf {
    descendant: AxisDescendant,
}

impl AxisDescendantOrSelf {
    pub fn new() -> SubAxis { box AxisDescendantOrSelf{descendant: AxisDescendant} }
}

impl XPathAxis for AxisDescendantOrSelf {
    fn select_nodes(&self,
                    context:   &XPathEvaluationContext,
                    node_test: &XPathNodeTest,
                    result:    &mut Nodeset)
    {
        node_test.test(context, result);
        self.descendant.select_nodes(context, node_test, result);
    }
}

pub struct AxisParent;

impl XPathAxis for AxisParent {
    fn select_nodes(&self,
                    context:   &XPathEvaluationContext,
                    node_test: &XPathNodeTest,
                    result:    &mut Nodeset)
    {
        let n = &context.node;

        match n.parent() {
            Some(p) => {
                let mut parent_context = context.new_context_for(1);
                parent_context.next(p);
                node_test.test(&parent_context, result);
            },
            None => {}
        }
    }
}

pub struct AxisSelf;

impl XPathAxis for AxisSelf {
    fn select_nodes(&self,
                    context:   &XPathEvaluationContext,
                    node_test: &XPathNodeTest,
                    result:    &mut Nodeset)
    {
        node_test.test(context, result);
    }
}

use super::XPathEvaluationContext;
use super::nodeset::Nodeset;
use super::nodeset::Node::{AttributeNode,ElementNode,TextNode};

pub trait XPathNodeTest {
    fn test<'a, 'd>(&self, context: &XPathEvaluationContext<'a, 'd>, result: &mut Nodeset<'d>);
}

pub type SubNodeTest = Box<XPathNodeTest + 'static>;

pub struct NodeTestAttribute {
    pub name: String,
}

impl XPathNodeTest for NodeTestAttribute {
    fn test<'a, 'd>(&self, context: &XPathEvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
        match context.node {
            AttributeNode(ref a) =>
                if self.name.as_slice() == "*" || a.name() == self.name.as_slice() {
                    result.add(context.node);
                },
            _ => {}
        }
    }
}

pub struct NodeTestElement {
    pub name: String,
}

impl XPathNodeTest for NodeTestElement {
    fn test<'a, 'd>(&self, context: &XPathEvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
        match context.node {
            ElementNode(ref e) =>
                // TODO: redo namespaces!
                // if (_name.has_prefix() != e->qname().has_namespace()) return;

                // if (_name.has_prefix()) {
                //     let prefix_uri = context.find_namespace_for_prefix(_name.prefix());

                //     if (! prefix_uri) return;
                //     if (*prefix_uri != e->qname().namespace_uri()) return;
                // }

                if self.name.as_slice() == "*" || e.name() == self.name.as_slice() {
                    result.add(context.node);
                },
            _ => {},
        }
    }
}

pub struct NodeTestNode;

impl XPathNodeTest for NodeTestNode {
    fn test<'a, 'd>(&self, context: &XPathEvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
        result.add(context.node);
    }
}

pub struct NodeTestText;

impl XPathNodeTest for NodeTestText {
    fn test<'a, 'd>(&self, context: &XPathEvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
        match context.node {
            TextNode(_) => result.add(context.node),
            _ => {},
        }
    }
}

use document::Nodeset;
use document::{AttributeAny,ElementAny,TextAny};

use super::XPathEvaluationContext;

pub trait XPathNodeTest {
    fn test(&self, context: &XPathEvaluationContext, result: &mut Nodeset);
}

pub type SubNodeTest = Box<XPathNodeTest + 'static>;

pub struct NodeTestAttribute {
    pub name: String,
}

impl XPathNodeTest for NodeTestAttribute {
    fn test(&self, context: &XPathEvaluationContext, result: &mut Nodeset) {
        match context.node {
            AttributeAny(ref a) =>
                if self.name.as_slice() == "*" || a.name() == self.name {
                    result.add(context.node.clone());
                },
            _ => {}
        }
    }
}

pub struct NodeTestElement {
    pub name: String,
}

impl XPathNodeTest for NodeTestElement {
    fn test(&self, context: &XPathEvaluationContext, result: &mut Nodeset) {
        match context.node {
            ElementAny(ref e) =>
                // TODO: redo namespaces!
                // if (_name.has_prefix() != e->qname().has_namespace()) return;

                // if (_name.has_prefix()) {
                //     let prefix_uri = context.find_namespace_for_prefix(_name.prefix());

                //     if (! prefix_uri) return;
                //     if (*prefix_uri != e->qname().namespace_uri()) return;
                // }

                if self.name.as_slice() == "*" || e.name() == self.name {
                    result.add(context.node.clone());
                },
            _ => {},
        }
    }
}

pub struct NodeTestNode;

impl XPathNodeTest for NodeTestNode {
    fn test(&self, context: &XPathEvaluationContext, result: &mut Nodeset) {
        result.add(context.node.clone());
    }
}

pub struct NodeTestText;

impl XPathNodeTest for NodeTestText {
    fn test(&self, context: &XPathEvaluationContext, result: &mut Nodeset) {
        match context.node {
            TextAny(_) => result.add(context.node.clone()),
            _ => {},
        }
    }
}

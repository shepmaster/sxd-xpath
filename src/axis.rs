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
pub struct Ancestor;

impl Axis for Ancestor {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &NodeTest,
                            result:    &mut Nodeset<'d>)
    {
        let mut node = context.node;
        while let Some(parent) = node.parent() {
            let mut child_context = context.new_context_for(1);
            child_context.next(parent);

            node_test.test(&child_context, result);
            node = parent;
        }
    }
}

#[allow(missing_copy_implementations)]
pub struct AncestorOrSelf;

impl Axis for AncestorOrSelf {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &NodeTest,
                            result:    &mut Nodeset<'d>)
    {
        node_test.test(context, result);
        Ancestor.select_nodes(context, node_test, result)
    }
}


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

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use document::Package;

    use super::super::EvaluationContext;
    use super::super::node_test::NodeTest;
    use super::super::nodeset::{Nodeset,ToNode};

    use super::{Axis,Ancestor,AncestorOrSelf};

    struct DummyNodeTest;
    impl NodeTest for DummyNodeTest {
        fn test<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
            result.add(context.node)
        }
    }

    fn execute<'n, A, N>(axis: A, node: N)
        -> Nodeset<'n>
        where A: Axis,
              N: ToNode<'n>,
    {
        let functions = &HashMap::new();
        let variables = &HashMap::new();
        let namespaces = &HashMap::new();

        let context = &EvaluationContext::new(node, functions, variables, namespaces);
        let node_test = &DummyNodeTest;
        let mut result = Nodeset::new();

        axis.select_nodes(context, node_test, &mut result);

        result
    }

    #[test]
    fn ancestor_includes_parents() {
        let package = Package::new();
        let doc = package.as_document();

        let level0 = doc.root();
        let level1 = doc.create_element("b");
        let level2 = doc.create_text("c");

        level0.append_child(level1);
        level1.append_child(level2);

        let result = execute(Ancestor, level2);

        assert_eq!(result, nodeset![level1, level0]);
    }

    #[test]
    fn ancestor_or_self_also_includes_self() {
        let package = Package::new();
        let doc = package.as_document();

        let level0 = doc.root();
        let level1 = doc.create_element("b");
        let level2 = doc.create_text("c");

        level0.append_child(level1);
        level1.append_child(level2);

        let result = execute(AncestorOrSelf, level2);

        assert_eq!(result, nodeset![level2, level1, level0]);
    }
}

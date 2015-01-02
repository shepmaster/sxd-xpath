extern crate document;

use super::EvaluationContext;
use super::node_test::NodeTest;
use super::nodeset::{Nodeset,Node};
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

fn preceding_following_sibling<'a, 'd>(context:   &EvaluationContext<'a, 'd>,
                                       node_test: &NodeTest,
                                       result:    &mut Nodeset<'d>,
                                       f: fn(&Node<'d>) -> Vec<Node<'d>>)
{
    let sibs = f(&context.node);
    for sibling in sibs.iter() {
        let mut child_context = context.new_context_for(1);
        child_context.next(*sibling);

        node_test.test(&child_context, result);
    }
}

#[allow(missing_copy_implementations)]
pub struct PrecedingSibling;

impl Axis for PrecedingSibling {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &NodeTest,
                            result:    &mut Nodeset<'d>)
    {
        preceding_following_sibling(context, node_test, result, Node::preceding_siblings)
    }
}

#[allow(missing_copy_implementations)]
pub struct FollowingSibling;

impl Axis for FollowingSibling {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &NodeTest,
                            result:    &mut Nodeset<'d>)
    {
        preceding_following_sibling(context, node_test, result, Node::following_siblings)
    }
}

fn preceding_following<'a, 'd>(context:   &EvaluationContext<'a, 'd>,
             node_test: &NodeTest,
             result:    &mut Nodeset<'d>,
             f: fn(&Node<'d>) -> Vec<Node<'d>>)
{
    let mut node = context.node;

    loop {
        let sibs = f(&node);
        for sibling in sibs.iter() {
            let mut child_context = context.new_context_for(1);
            child_context.next(*sibling);

            node_test.test(&child_context, result);
        }

        match node.parent() {
            Some(parent) => node = parent,
            None => break
        }
    }
}

#[allow(missing_copy_implementations)]
pub struct Preceding;

impl Axis for Preceding {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &NodeTest,
                            result:    &mut Nodeset<'d>)
    {
        preceding_following(context, node_test, result, Node::preceding_siblings)
    }
}

#[allow(missing_copy_implementations)]
pub struct Following;

impl Axis for Following {
    fn select_nodes<'a, 'd>(&self,
                            context:   &EvaluationContext<'a, 'd>,
                            node_test: &NodeTest,
                            result:    &mut Nodeset<'d>)
    {
        preceding_following(context, node_test, result, Node::following_siblings)
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
    use document::dom4;

    use super::super::EvaluationContext;
    use super::super::node_test::NodeTest;
    use super::super::nodeset::{Nodeset,ToNode};

    use super::{
        Axis,
        Ancestor,
        AncestorOrSelf,
        PrecedingSibling,
        FollowingSibling,
        Preceding,
        Following,
    };

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

    #[test]
    fn preceding_sibling_selects_in_reverse_document_order() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();
        let child1 = doc.create_element("a");
        let child2 = doc.create_comment("b");
        let child3 = doc.create_processing_instruction("c", None);

        root.append_child(child1);
        root.append_child(child2);
        root.append_child(child3);

        let result = execute(PrecedingSibling, child3);

        assert_eq!(result, nodeset![child2, child1]);
    }

    #[test]
    fn following_sibling_selects_in_document_order() {
        let package = Package::new();
        let doc = package.as_document();

        let root = doc.root();
        let child1 = doc.create_element("a");
        let child2 = doc.create_comment("b");
        let child3 = doc.create_processing_instruction("c", None);

        root.append_child(child1);
        root.append_child(child2);
        root.append_child(child3);

        let result = execute(FollowingSibling, child1);

        assert_eq!(result, nodeset![child2, child3]);
    }

    fn setup_preceding_following<'d>(doc: &'d dom4::Document<'d>) -> [dom4::Element<'d>; 5] {
        let parent = doc.create_element("parent");

        let a1 = doc.create_element("a1");
        let a2 = doc.create_element("a2");
        let a3 = doc.create_element("a3");

        let b1 = doc.create_element("b1");
        let b2 = doc.create_element("b2");
        let b3 = doc.create_element("b3");

        parent.append_child(a1);
        parent.append_child(a2);
        parent.append_child(a3);

        a2.append_child(b1);
        a2.append_child(b2);
        a2.append_child(b3);

        [a1, b1, b2, b3, a3]
    }

    #[test]
    fn preceding_selects_in_reverse_document_order() {
        let package = Package::new();
        let doc = package.as_document();
        let [a1, b1, b2, _, _] = setup_preceding_following(&doc);

        let result = execute(Preceding, b2);

        assert_eq!(result, nodeset![b1, a1]);
    }

    #[test]
    fn following_selects_in_document_order() {
        let package = Package::new();
        let doc = package.as_document();
        let [_, _, b2, b3, a3] = setup_preceding_following(&doc);

        let result = execute(Following, b2);

        assert_eq!(result, nodeset![b3, a3]);
    }
}

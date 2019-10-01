extern crate sxd_document;

use std::fmt;

use context;
use node_test::NodeTest;
use nodeset::{self, Node, OrderedNodes};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PrincipalNodeType {
    Attribute,
    Element,
    Namespace,
}

/// A directed traversal of Nodes.
pub trait AxisLike: fmt::Debug {
    /// Applies the given node test to the nodes selected by this axis,
    /// adding matching nodes to the nodeset.
    fn select_nodes<'c, 'd>(
        &self,
        context: &context::Evaluation<'c, 'd>,
        node_test: &NodeTest,
    ) -> OrderedNodes<'d>;

    /// Describes what node type is naturally selected by this axis.
    fn principal_node_type(&self) -> PrincipalNodeType {
        PrincipalNodeType::Element
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Axis {
    Ancestor,
    AncestorOrSelf,
    Attribute,
    Namespace,
    Child,
    Descendant,
    DescendantOrSelf,
    Parent,
    PrecedingSibling,
    FollowingSibling,
    Preceding,
    Following,
    SelfAxis,
}

struct CompleteNodeTest<'c, 'd: 'c> {
    context: &'c context::Evaluation<'c, 'd>,
    node_test: &'c NodeTest,
    result: OrderedNodes<'d>,
}

impl<'c, 'd> CompleteNodeTest<'c, 'd> {
    fn new(context: &'c context::Evaluation<'c, 'd>, node_test: &'c NodeTest) -> Self {
        CompleteNodeTest {
            context: context,
            node_test: node_test,
            result: OrderedNodes::new(),
        }
    }

    fn run(&mut self, node: Node<'d>) {
        let new_context = self.context.new_context_for(node);
        self.node_test.test(&new_context, &mut self.result);
    }
}

impl AxisLike for Axis {
    fn select_nodes<'c, 'd>(
        &self,
        context: &context::Evaluation<'c, 'd>,
        node_test: &NodeTest,
    ) -> OrderedNodes<'d> {
        use self::Axis::*;

        let mut node_test = CompleteNodeTest::new(context, node_test);

        match *self {
            Ancestor => each_parent(context.node, |n| node_test.run(n)),
            AncestorOrSelf => node_and_each_parent(context.node, |n| node_test.run(n)),
            Attribute => {
                if let Node::Element(ref e) = context.node {
                    for attr in e.attributes() {
                        node_test.run(Node::Attribute(attr));
                    }
                }
            }
            Namespace => {
                if let Node::Element(ref e) = context.node {
                    for ns in e.namespaces_in_scope() {
                        let ns = Node::Namespace(nodeset::Namespace {
                            parent: *e,
                            prefix: ns.prefix(),
                            uri: ns.uri(),
                        });

                        node_test.run(ns);
                    }
                }
            }
            Child => {
                for child in context.node.children() {
                    node_test.run(child);
                }
            }
            Descendant => {
                for child in context.node.children() {
                    preorder_left_to_right(child, |n| node_test.run(n));
                }
            }
            DescendantOrSelf => preorder_left_to_right(context.node, |n| node_test.run(n)),
            Parent => {
                if let Some(parent) = context.node.parent() {
                    node_test.run(parent);
                }
            }
            PrecedingSibling => {
                for sibling in context.node.preceding_siblings() {
                    node_test.run(sibling)
                }
            }
            FollowingSibling => {
                for sibling in context.node.following_siblings() {
                    node_test.run(sibling)
                }
            }
            Preceding => node_and_each_parent(context.node, |node| {
                for sibling in node.preceding_siblings() {
                    postorder_right_to_left(sibling, |n| node_test.run(n));
                }
            }),
            Following => node_and_each_parent(context.node, |node| {
                for sibling in node.following_siblings() {
                    preorder_left_to_right(sibling, |n| node_test.run(n));
                }
            }),
            SelfAxis => node_test.run(context.node),
        }

        node_test.result
    }

    fn principal_node_type(&self) -> PrincipalNodeType {
        use self::Axis::*;
        match *self {
            Attribute => PrincipalNodeType::Attribute,
            Namespace => PrincipalNodeType::Namespace,
            _ => PrincipalNodeType::Element,
        }
    }
}

fn preorder_left_to_right<'d, F>(node: Node<'d>, mut f: F)
where
    F: FnMut(Node<'d>),
{
    let mut stack = vec![node];

    while let Some(current) = stack.pop() {
        f(current);

        for child in current.children().into_iter().rev() {
            stack.push(child);
        }
    }
}

// There's other implementations that only require a single stack; are
// those applicable? Are they better?
fn postorder_right_to_left<'d, F>(node: Node<'d>, mut f: F)
where
    F: FnMut(Node<'d>),
{
    let mut stack = vec![node];
    let mut stack2 = vec![];

    while let Some(current) = stack.pop() {
        for child in current.children().into_iter().rev() {
            stack.push(child);
        }
        stack2.push(current);
    }

    for current in stack2.into_iter().rev() {
        f(current);
    }
}

fn node_and_each_parent<'d, F>(node: Node<'d>, mut f: F)
where
    F: FnMut(Node<'d>),
{
    f(node);
    each_parent(node, f);
}

fn each_parent<'d, F>(mut node: Node<'d>, mut f: F)
where
    F: FnMut(Node<'d>),
{
    while let Some(parent) = node.parent() {
        f(parent);
        node = parent;
    }
}

#[cfg(test)]
mod test {
    use sxd_document::dom;
    use sxd_document::Package;

    use context::{self, Context};
    use node_test::NodeTest;
    use nodeset::{Node, OrderedNodes};

    use super::Axis::*;
    use super::*;

    #[derive(Debug)]
    struct DummyNodeTest;
    impl NodeTest for DummyNodeTest {
        fn test<'c, 'd>(
            &self,
            context: &context::Evaluation<'c, 'd>,
            result: &mut OrderedNodes<'d>,
        ) {
            result.add(context.node)
        }
    }

    fn execute<'n, N>(axis: Axis, node: N) -> OrderedNodes<'n>
    where
        N: Into<Node<'n>>,
    {
        let context = Context::without_core_functions();
        let context = context::Evaluation::new(&context, node.into());
        let node_test = &DummyNodeTest;

        axis.select_nodes(&context, node_test)
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

        assert_eq!(result, ordered_nodes![level1, level0]);
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

        assert_eq!(result, ordered_nodes![level2, level1, level0]);
    }

    #[test]
    fn descendant_includes_parents() {
        let package = Package::new();
        let doc = package.as_document();

        let level0 = doc.root();
        let level1 = doc.create_element("b");
        let level2 = doc.create_text("c");

        level0.append_child(level1);
        level1.append_child(level2);

        let result = execute(Descendant, level0);

        assert_eq!(result, ordered_nodes![level1, level2]);
    }

    #[test]
    fn descendant_or_self_also_includes_self() {
        let package = Package::new();
        let doc = package.as_document();

        let level0 = doc.root();
        let level1 = doc.create_element("b");
        let level2 = doc.create_text("c");

        level0.append_child(level1);
        level1.append_child(level2);

        let result = execute(DescendantOrSelf, level0);

        assert_eq!(result, ordered_nodes![level0, level1, level2]);
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

        assert_eq!(result, ordered_nodes![child2, child1]);
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

        assert_eq!(result, ordered_nodes![child2, child3]);
    }

    // <a0>
    //   <b0>
    //     <c0 />
    //     <c1 />
    //   </b0>
    //   <b1>
    //     <c2 />
    //     <c3 />
    //     <c4 />
    //   </b1>
    //   <b2>
    //     <c5 />
    //     <c6 />
    //   </b2>
    // </a0>

    struct PrecedingFollowing<'d> {
        b: [dom::Element<'d>; 3],
        c: [dom::Element<'d>; 7],
        midpoint: dom::Element<'d>,
    }

    impl<'d> PrecedingFollowing<'d> {
        fn new(doc: dom::Document<'d>) -> Self {
            let a = doc.create_element("a");

            let b0 = doc.create_element("b0");
            let b1 = doc.create_element("b1");
            let b2 = doc.create_element("b2");

            let c0 = doc.create_element("c0");
            let c1 = doc.create_element("c1");
            let c2 = doc.create_element("c2");
            let c3 = doc.create_element("c3");
            let c4 = doc.create_element("c4");
            let c5 = doc.create_element("c5");
            let c6 = doc.create_element("c6");

            a.append_child(b0);
            a.append_child(b1);
            a.append_child(b2);

            b0.append_child(c0);
            b0.append_child(c1);

            b1.append_child(c2);
            b1.append_child(c3);
            b1.append_child(c4);

            b2.append_child(c5);
            b2.append_child(c6);

            PrecedingFollowing {
                midpoint: c3,
                b: [b0, b1, b2],
                c: [c0, c1, c2, c3, c4, c5, c6],
            }
        }
    }

    #[test]
    fn preceding_selects_in_reverse_document_order() {
        let package = Package::new();
        let doc = package.as_document();
        let PrecedingFollowing { b, c, midpoint } = PrecedingFollowing::new(doc);

        let result = execute(Preceding, midpoint);

        assert_eq!(result, ordered_nodes![c[2], c[1], c[0], b[0]]);
    }

    #[test]
    fn following_selects_in_document_order() {
        let package = Package::new();
        let doc = package.as_document();
        let PrecedingFollowing { b, c, midpoint } = PrecedingFollowing::new(doc);

        let result = execute(Following, midpoint);

        assert_eq!(result, ordered_nodes![c[4], b[2], c[5], c[6]]);
    }
}

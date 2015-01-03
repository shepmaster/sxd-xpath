use document::dom4;

use self::Node::*;
use std::slice::Iter;

macro_rules! unpack(
    ($enum_name:ident, $name:ident, $wrapper:ident, dom4::$inner:ident) => (
        impl<'d> $enum_name<'d> {
            pub fn $name(self) -> Option<dom4::$inner<'d>> {
                match self {
                    $wrapper(n) => Some(n),
                    _ => None,
                }
            }
        }
    )
);

macro_rules! conversion_trait(
    ($tr_name:ident, $method:ident, $res_type:ident,
        { $(dom4::$leaf_type:ident => $variant:ident),* }
    ) => (
        pub trait $tr_name<'d> {
            fn $method(self) -> $res_type<'d>;
        }

        impl<'d> $tr_name<'d> for $res_type<'d> {
            fn $method(self) -> $res_type<'d> {
                self
            }
        }

        $(impl<'d> $tr_name<'d> for dom4::$leaf_type<'d> {
            fn $method(self) -> $res_type<'d> {
                $variant(self)
            }
        })*
    )
);

#[derive(Clone,PartialEq,Show,Copy)]
pub enum Node<'d> {
    RootNode(dom4::Root<'d>),
    ElementNode(dom4::Element<'d>),
    AttributeNode(dom4::Attribute<'d>),
    TextNode(dom4::Text<'d>),
    CommentNode(dom4::Comment<'d>),
    ProcessingInstructionNode(dom4::ProcessingInstruction<'d>),
}

unpack!(Node, root, RootNode, dom4::Root);
unpack!(Node, element, ElementNode, dom4::Element);
unpack!(Node, attribute, AttributeNode, dom4::Attribute);
unpack!(Node, text, TextNode, dom4::Text);
unpack!(Node, comment, CommentNode, dom4::Comment);
unpack!(Node, processing_instruction, ProcessingInstructionNode, dom4::ProcessingInstruction);

impl<'d> Node<'d> {
    pub fn document(&self) -> &'d dom4::Document<'d> {
        match self {
            &RootNode(n)                  => n.document(),
            &ElementNode(n)               => n.document(),
            &AttributeNode(n)             => n.document(),
            &TextNode(n)                  => n.document(),
            &CommentNode(n)               => n.document(),
            &ProcessingInstructionNode(n) => n.document(),
        }
    }

    pub fn parent(&self) -> Option<Node<'d>> {
        match self {
            &RootNode(_)                  => None,
            &ElementNode(n)               => n.parent().map(|n| n.to_node()),
            &AttributeNode(n)             => n.parent().map(|n| n.to_node()),
            &TextNode(n)                  => n.parent().map(|n| n.to_node()),
            &CommentNode(n)               => n.parent().map(|n| n.to_node()),
            &ProcessingInstructionNode(n) => n.parent().map(|n| n.to_node()),
        }
    }

    pub fn children(&self) -> Vec<Node<'d>> {
        match self {
            &RootNode(n)                  => n.children().iter().map(|n| n.to_node()).collect(),
            &ElementNode(n)               => n.children().iter().map(|n| n.to_node()).collect(),
            &AttributeNode(_)             => Vec::new(),
            &TextNode(_)                  => Vec::new(),
            &CommentNode(_)               => Vec::new(),
            &ProcessingInstructionNode(_) => Vec::new(),
        }
    }

    pub fn preceding_siblings(&self) -> Vec<Node<'d>> {
        match self {
            &RootNode(_)                  => Vec::new(),
            &ElementNode(n)               => n.preceding_siblings().iter().rev().map(|n| n.to_node()).collect(),
            &AttributeNode(_)             => Vec::new(),
            &TextNode(n)                  => n.preceding_siblings().iter().rev().map(|n| n.to_node()).collect(),
            &CommentNode(n)               => n.preceding_siblings().iter().rev().map(|n| n.to_node()).collect(),
            &ProcessingInstructionNode(n) => n.preceding_siblings().iter().rev().map(|n| n.to_node()).collect(),
        }
    }

    pub fn following_siblings(&self) -> Vec<Node<'d>> {
        match self {
            &RootNode(_)                  => Vec::new(),
            &ElementNode(n)               => n.following_siblings().iter().map(|n| n.to_node()).collect(),
            &AttributeNode(_)             => Vec::new(),
            &TextNode(n)                  => n.following_siblings().iter().map(|n| n.to_node()).collect(),
            &CommentNode(n)               => n.following_siblings().iter().map(|n| n.to_node()).collect(),
            &ProcessingInstructionNode(n) => n.following_siblings().iter().map(|n| n.to_node()).collect(),
        }
    }
}

conversion_trait!(ToNode, to_node, Node, {
    dom4::Root => RootNode,
    dom4::Element => ElementNode,
    dom4::Attribute => AttributeNode,
    dom4::Text => TextNode,
    dom4::Comment => CommentNode,
    dom4::ProcessingInstruction => ProcessingInstructionNode
});

impl<'d> ToNode<'d> for dom4::ChildOfRoot<'d> {
    fn to_node(self) -> Node<'d> {
        match self {
            dom4::ChildOfRoot::Element(n)               => ElementNode(n),
            dom4::ChildOfRoot::Comment(n)               => CommentNode(n),
            dom4::ChildOfRoot::ProcessingInstruction(n) => ProcessingInstructionNode(n),
        }
    }
}

impl<'d> ToNode<'d> for dom4::ChildOfElement<'d> {
    fn to_node(self) -> Node<'d> {
        match self {
            dom4::ChildOfElement::Element(n)               => ElementNode(n),
            dom4::ChildOfElement::Text(n)                  => TextNode(n),
            dom4::ChildOfElement::Comment(n)               => CommentNode(n),
            dom4::ChildOfElement::ProcessingInstruction(n) => ProcessingInstructionNode(n),
        }
    }
}

impl<'d> ToNode<'d> for dom4::ParentOfChild<'d> {
    fn to_node(self) -> Node<'d> {
        match self {
            dom4::ParentOfChild::Root(n)    => RootNode(n),
            dom4::ParentOfChild::Element(n) => ElementNode(n),
        }
    }
}

/// A collection of nodes
#[derive(PartialEq,Show,Clone)]
pub struct Nodeset<'d> {
    nodes: Vec<Node<'d>>,
}

impl<'d> Nodeset<'d> {
    pub fn new() -> Nodeset<'d> {
        Nodeset { nodes: Vec::new() }
    }

    pub fn add<N : ToNode<'d>>(&mut self, node: N) {
        self.nodes.push(node.to_node());
    }

    pub fn iter(&self) -> Iter<Node<'d>> {
        self.nodes.iter()
    }

    pub fn add_nodeset(& mut self, other: &Nodeset<'d>) {
        self.nodes.push_all(other.nodes.as_slice());
    }

    pub fn size(&self) -> uint {
        self.nodes.len()
    }
}

#[cfg(test)]
mod test {
    use document::Package;
    use super::Node::{
        AttributeNode,
        CommentNode,
        ElementNode,
        ProcessingInstructionNode,
        RootNode,
        TextNode,
    };
    use super::Nodeset;

    #[test]
    fn nodeset_can_include_all_node_types() {
        let package = Package::new();
        let doc = package.as_document();
        let mut nodes = Nodeset::new();

        let r = doc.root();
        let e = doc.create_element("element");
        let a = e.set_attribute_value("name", "value");
        let t = doc.create_text("text");
        let c = doc.create_comment("comment");
        let p = doc.create_processing_instruction("pi", None);

        nodes.add(r);
        nodes.add(e);
        nodes.add(a);
        nodes.add(t);
        nodes.add(c);
        nodes.add(p);

        let node_vec: Vec<_> = nodes.iter().collect();

        assert_eq!(6, node_vec.len());
        assert_eq!(node_vec[0], &RootNode(r));
        assert_eq!(node_vec[1], &ElementNode(e));
        assert_eq!(node_vec[2], &AttributeNode(a));
        assert_eq!(node_vec[3], &TextNode(t));
        assert_eq!(node_vec[4], &CommentNode(c));
        assert_eq!(node_vec[5], &ProcessingInstructionNode(p));
    }

    #[test]
    fn nodesets_can_be_combined() {
        let package = Package::new();
        let doc = package.as_document();

        let mut all_nodes = Nodeset::new();
        let mut nodes1 = Nodeset::new();
        let mut nodes2 = Nodeset::new();

        let e1 = doc.create_element("element1");
        let e2 = doc.create_element("element2");

        all_nodes.add(e1);
        all_nodes.add(e2);

        nodes1.add(e1);
        nodes2.add(e2);

        nodes1.add_nodeset(&nodes2);

        assert_eq!(all_nodes, nodes1);
    }
}

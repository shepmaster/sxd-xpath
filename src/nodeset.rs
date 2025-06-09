//! Support for collections of nodes.

use std::borrow::ToOwned;
use std::collections::hash_set;
use std::collections::{HashMap, HashSet};
use std::iter::{FromIterator, IntoIterator};

use sxd_document::dom;
use sxd_document::QName;

macro_rules! unpack(
    ($enum_name:ident, {
        $($name:ident, $wrapper:ident, dom::$inner:ident),*
    }) => (
        $(
            pub fn $name(self) -> Option<dom::$inner<'d>> {
                match self {
                    $enum_name::$wrapper(n) => Some(n),
                    _ => None,
                }
            }
        )*
    )
);

macro_rules! conversion_trait(
    ($res_type:ident, {
        $(dom::$leaf_type:ident => Node::$variant:ident),*
    }) => (
        $(impl<'d> From<dom::$leaf_type<'d>> for $res_type<'d>  {
            fn from(v: dom::$leaf_type<'d>) -> $res_type<'d> {
                Node::$variant(v)
            }
        })*
    )
);

/// Represents a namespace.
///
/// This differs from the DOM, which does not treat namespaces as a
/// separate item.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Namespace<'d> {
    pub parent: dom::Element<'d>,
    pub prefix: &'d str,
    pub uri: &'d str,
}

impl<'d> Namespace<'d> {
    pub fn document(&self) -> dom::Document<'d> {
        self.parent.document()
    }
    pub fn parent(&self) -> dom::Element<'d> {
        self.parent
    }
    pub fn prefix(&self) -> &'d str {
        self.prefix
    }
    pub fn uri(&self) -> &'d str {
        self.uri
    }
    pub fn expanded_name(&self) -> QName<'d> {
        QName::new(self.prefix)
    }
}

/// Any of the various types of nodes found in an XML document.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Node<'d> {
    Root(dom::Root<'d>),
    Element(dom::Element<'d>),
    Attribute(dom::Attribute<'d>),
    Text(dom::Text<'d>),
    Comment(dom::Comment<'d>),
    Namespace(Namespace<'d>),
    ProcessingInstruction(dom::ProcessingInstruction<'d>),
}

impl<'d> Node<'d> {
    /// The document to which this node belongs.
    pub fn document(&self) -> dom::Document<'d> {
        use self::Node::*;
        match *self {
            Root(n) => n.document(),
            Element(n) => n.document(),
            Attribute(n) => n.document(),
            Text(n) => n.document(),
            Comment(n) => n.document(),
            ProcessingInstruction(n) => n.document(),
            Namespace(n) => n.document(),
        }
    }

    /// The name of the node, including a prefix that corresponds to the namespace, if any.
    pub fn prefixed_name(&self) -> Option<String> {
        use self::Node::*;

        fn qname_prefixed_name(
            element: dom::Element<'_>,
            name: QName<'_>,
            preferred_prefix: Option<&str>,
        ) -> String {
            if let Some(ns_uri) = name.namespace_uri() {
                if let Some(prefix) = element.prefix_for_namespace_uri(ns_uri, preferred_prefix) {
                    format!("{}:{}", prefix, name.local_part())
                } else {
                    name.local_part().to_owned()
                }
            } else {
                name.local_part().to_owned()
            }
        }

        match *self {
            Root(_) => None,
            Element(n) => Some(qname_prefixed_name(n, n.name(), n.preferred_prefix())),
            Attribute(n) => {
                let parent = n.parent().expect("Cannot process attribute without parent");
                Some(qname_prefixed_name(parent, n.name(), n.preferred_prefix()))
            }
            Text(_) => None,
            Comment(_) => None,
            ProcessingInstruction(n) => Some(n.target().to_owned()),
            Namespace(n) => Some(n.prefix().to_owned()),
        }
    }

    /// Returns the [expanded name][] of the node, if any.
    ///
    /// [expanded name]: https://www.w3.org/TR/xpath/#dt-expanded-name
    pub fn expanded_name(&self) -> Option<QName<'d>> {
        use self::Node::*;
        match *self {
            Root(_) => None,
            Element(n) => Some(n.name()),
            Attribute(n) => Some(n.name()),
            Text(_) => None,
            Comment(_) => None,
            ProcessingInstruction(n) => Some(QName::new(n.target())),
            Namespace(n) => Some(n.expanded_name()),
        }
    }

    /// Returns the parent of the node, if any.
    pub fn parent(&self) -> Option<Node<'d>> {
        use self::Node::*;
        match *self {
            Root(_) => None,
            Element(n) => n.parent().map(Into::into),
            Attribute(n) => n.parent().map(Into::into),
            Text(n) => n.parent().map(Into::into),
            Comment(n) => n.parent().map(Into::into),
            ProcessingInstruction(n) => n.parent().map(Into::into),
            Namespace(n) => Some(n.parent().into()),
        }
    }

    /// Returns the children of the node, if any.
    pub fn children(&self) -> Vec<Node<'d>> {
        use self::Node::*;
        match *self {
            Root(n) => n.children().into_iter().map(Into::into).collect(),
            Element(n) => n.children().into_iter().map(Into::into).collect(),
            Attribute(_) => Vec::new(),
            Text(_) => Vec::new(),
            Comment(_) => Vec::new(),
            ProcessingInstruction(_) => Vec::new(),
            Namespace(_) => Vec::new(),
        }
    }

    /// Returns the nodes with the same parent that occur before this node.
    pub fn preceding_siblings(&self) -> Vec<Node<'d>> {
        use self::Node::*;
        match *self {
            Root(_) => Vec::new(),
            Element(n) => n
                .preceding_siblings()
                .into_iter()
                .rev()
                .map(Into::into)
                .collect(),
            Attribute(_) => Vec::new(),
            Text(n) => n
                .preceding_siblings()
                .into_iter()
                .rev()
                .map(Into::into)
                .collect(),
            Comment(n) => n
                .preceding_siblings()
                .into_iter()
                .rev()
                .map(Into::into)
                .collect(),
            ProcessingInstruction(n) => n
                .preceding_siblings()
                .into_iter()
                .rev()
                .map(Into::into)
                .collect(),
            Namespace(_) => Vec::new(),
        }
    }

    /// Returns the nodes with the same parent that occur after this node.
    pub fn following_siblings(&self) -> Vec<Node<'d>> {
        use self::Node::*;
        match *self {
            Root(_) => Vec::new(),
            Element(n) => n.following_siblings().into_iter().map(Into::into).collect(),
            Attribute(_) => Vec::new(),
            Text(n) => n.following_siblings().into_iter().map(Into::into).collect(),
            Comment(n) => n.following_siblings().into_iter().map(Into::into).collect(),
            ProcessingInstruction(n) => {
                n.following_siblings().into_iter().map(Into::into).collect()
            }
            Namespace(_) => Vec::new(),
        }
    }

    /// Returns the [string value] of this node.
    ///
    /// [string value]: https://www.w3.org/TR/xpath/#dt-string-value
    pub fn string_value(&self) -> String {
        use self::Node::*;

        fn document_order_text_nodes(node: Node<'_>, result: &mut String) {
            for child in node.children() {
                match child {
                    Node::Element(_) => document_order_text_nodes(child, result),
                    Node::Text(n) => result.push_str(n.text()),
                    _ => {}
                }
            }
        }

        fn text_descendants_string_value(node: Node<'_>) -> String {
            let mut result = String::new();
            document_order_text_nodes(node, &mut result);
            result
        }

        match *self {
            Root(_) => text_descendants_string_value(*self),
            Element(_) => text_descendants_string_value(*self),
            Attribute(n) => n.value().to_owned(),
            ProcessingInstruction(n) => n.value().unwrap_or("").to_owned(),
            Comment(n) => n.text().to_owned(),
            Text(n) => n.text().to_owned(),
            Namespace(n) => n.uri().to_owned(),
        }
    }

    unpack!(Node, {
        root, Root, dom::Root,
        element, Element, dom::Element,
        attribute, Attribute, dom::Attribute,
        text, Text, dom::Text,
        comment, Comment, dom::Comment,
        processing_instruction, ProcessingInstruction, dom::ProcessingInstruction
    });

    pub fn namespace(self) -> Option<Namespace<'d>> {
        match self {
            Node::Namespace(n) => Some(n),
            _ => None,
        }
    }
}

conversion_trait!(Node, {
    dom::Root                  => Node::Root,
    dom::Element               => Node::Element,
    dom::Attribute             => Node::Attribute,
    dom::Text                  => Node::Text,
    dom::Comment               => Node::Comment,
    dom::ProcessingInstruction => Node::ProcessingInstruction
});

impl<'d> From<dom::ChildOfRoot<'d>> for Node<'d> {
    fn from(other: dom::ChildOfRoot<'d>) -> Node<'d> {
        use self::Node::*;
        match other {
            dom::ChildOfRoot::Element(n) => Element(n),
            dom::ChildOfRoot::Comment(n) => Comment(n),
            dom::ChildOfRoot::ProcessingInstruction(n) => ProcessingInstruction(n),
        }
    }
}

impl<'d> From<dom::ChildOfElement<'d>> for Node<'d> {
    fn from(other: dom::ChildOfElement<'d>) -> Node<'d> {
        use self::Node::*;
        match other {
            dom::ChildOfElement::Element(n) => Element(n),
            dom::ChildOfElement::Text(n) => Text(n),
            dom::ChildOfElement::Comment(n) => Comment(n),
            dom::ChildOfElement::ProcessingInstruction(n) => ProcessingInstruction(n),
        }
    }
}

impl<'d> From<dom::ParentOfChild<'d>> for Node<'d> {
    fn from(other: dom::ParentOfChild<'d>) -> Self {
        use self::Node::*;
        match other {
            dom::ParentOfChild::Root(n) => Root(n),
            dom::ParentOfChild::Element(n) => Element(n),
        }
    }
}

/// An unordered collection of unique nodes
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Nodeset<'d> {
    nodes: HashSet<Node<'d>>,
}

impl<'d> Nodeset<'d> {
    pub fn new() -> Nodeset<'d> {
        Default::default()
    }

    /// Checks if the node is present in the set
    pub fn contains<N>(&self, node: N) -> bool
    where
        N: Into<Node<'d>>,
    {
        self.nodes.contains(&node.into())
    }

    /// Add the given node to the set
    pub fn add<N>(&mut self, node: N)
    where
        N: Into<Node<'d>>,
    {
        self.nodes.insert(node.into());
    }

    pub fn iter<'a>(&'a self) -> Iter<'a, 'd> {
        IntoIterator::into_iter(self)
    }

    pub fn size(&self) -> usize {
        self.nodes.len()
    }

    /// Returns the node that occurs first in [document order]
    ///
    /// [document order]: https://www.w3.org/TR/xpath/#dt-document-order
    pub fn document_order_first(&self) -> Option<Node<'d>> {
        let node = self.nodes.iter().next()?;

        if self.nodes.len() == 1 {
            return Some(*node);
        }

        let order = DocOrder::new(node.document());

        self.nodes
            .iter()
            .min_by_key(|&&n| order.order_of(n))
            .cloned()
    }

    pub fn document_order(&self) -> Vec<Node<'d>> {
        let mut nodes: Vec<_> = self.iter().collect();
        if nodes.len() == 1 {
            return nodes;
        }

        let doc = match nodes.first().map(Node::document) {
            Some(doc) => doc,
            None => return nodes,
        };

        let order = DocOrder::new(doc);
        nodes.sort_by_key(|&n| order.order_of(n));
        nodes
    }
}

impl<'d> Extend<Node<'d>> for Nodeset<'d> {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Node<'d>>,
    {
        self.nodes.extend(iter)
    }
}

// Rebuilding this multiple times cannot possibly be performant,
// but I want to see how widely used this is first before
// picking an appropriate caching point.
struct DocOrder<'d>(HashMap<Node<'d>, usize>);

impl<'d> DocOrder<'d> {
    fn new(doc: dom::Document<'d>) -> Self {
        let mut idx = 0;
        let mut stack: Vec<Node<'_>> = vec![doc.root().into()];
        #[allow(clippy::mutable_key_type)]
        let mut order = HashMap::new();

        while let Some(n) = stack.pop() {
            order.insert(n, idx);
            idx += 1;

            stack.extend(n.children().into_iter().rev());

            if let Node::Element(e) = n {
                // TODO: namespaces
                stack.extend(e.attributes().into_iter().map(Node::Attribute));
            }
        }

        DocOrder(order)
    }

    fn order_of(&self, node: Node<'d>) -> usize {
        // See the library-level docs for rationale on this MAX
        self.0.get(&node).cloned().unwrap_or(usize::MAX)
    }
}

impl<'a, 'd: 'a> IntoIterator for &'a Nodeset<'d> {
    type Item = Node<'d>;
    type IntoIter = Iter<'a, 'd>;

    fn into_iter(self) -> Iter<'a, 'd> {
        Iter {
            iter: self.nodes.iter(),
        }
    }
}

impl<'d> IntoIterator for Nodeset<'d> {
    type Item = Node<'d>;
    type IntoIter = IntoIter<'d>;

    fn into_iter(self) -> IntoIter<'d> {
        IntoIter {
            iter: self.nodes.into_iter(),
        }
    }
}

impl<'d> From<OrderedNodes<'d>> for Nodeset<'d> {
    fn from(other: OrderedNodes<'d>) -> Self {
        other.0.into_iter().collect()
    }
}

impl<'d> FromIterator<Node<'d>> for Nodeset<'d> {
    fn from_iter<I>(iterator: I) -> Nodeset<'d>
    where
        I: IntoIterator<Item = Node<'d>>,
    {
        Nodeset {
            nodes: iterator.into_iter().collect(),
        }
    }
}

pub struct Iter<'a, 'd> {
    iter: hash_set::Iter<'a, Node<'d>>,
}

impl<'a, 'd: 'a> Iterator for Iter<'a, 'd> {
    type Item = Node<'d>;
    fn next(&mut self) -> Option<Node<'d>> {
        self.iter.next().cloned()
    }
}

pub struct IntoIter<'d> {
    iter: hash_set::IntoIter<Node<'d>>,
}

impl<'d> Iterator for IntoIter<'d> {
    type Item = Node<'d>;
    fn next(&mut self) -> Option<Node<'d>> {
        self.iter.next()
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct OrderedNodes<'d>(Vec<Node<'d>>);

impl<'d> OrderedNodes<'d> {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn size(&self) -> usize {
        self.0.len()
    }

    pub fn add(&mut self, node: Node<'d>) {
        self.0.push(node)
    }
}

impl<'d> From<Vec<Node<'d>>> for OrderedNodes<'d> {
    fn from(other: Vec<Node<'d>>) -> Self {
        OrderedNodes(other)
    }
}

impl<'d> From<OrderedNodes<'d>> for Vec<Node<'d>> {
    fn from(other: OrderedNodes<'d>) -> Self {
        other.0
    }
}

impl<'d> FromIterator<Node<'d>> for OrderedNodes<'d> {
    fn from_iter<I>(iterator: I) -> OrderedNodes<'d>
    where
        I: IntoIterator<Item = Node<'d>>,
    {
        OrderedNodes(iterator.into_iter().collect())
    }
}

#[cfg(test)]
mod test {
    use std::borrow::ToOwned;

    use sxd_document::Package;

    use super::Node::*;
    use super::{Node, Nodeset};

    fn into_node<'d, T: Into<Node<'d>>>(n: T) -> Node<'d> {
        n.into()
    }

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

        assert_eq!(6, nodes.size());
        assert!(nodes.contains(Root(r)));
        assert!(nodes.contains(Element(e)));
        assert!(nodes.contains(Attribute(a)));
        assert!(nodes.contains(Text(t)));
        assert!(nodes.contains(Comment(c)));
        assert!(nodes.contains(ProcessingInstruction(p)));
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

        nodes1.extend(nodes2);

        assert_eq!(all_nodes, nodes1);
    }

    #[test]
    fn nodeset_knows_first_node_in_document_order() {
        let package = Package::new();
        let doc = package.as_document();

        let c1 = doc.create_comment("1");
        let c2 = doc.create_comment("2");
        doc.root().append_child(c1);
        doc.root().append_child(c2);

        let nodes = nodeset![c2, c1];

        assert_eq!(Some(into_node(c1)), nodes.document_order_first());
    }

    #[test]
    fn attributes_come_before_children_in_document_order() {
        let package = Package::new();
        let doc = package.as_document();

        let parent = doc.create_element("parent");
        let attr = parent.set_attribute_value("a", "v");
        let child = doc.create_element("child");

        doc.root().append_child(parent);
        parent.append_child(child);

        let nodes = nodeset![child, attr];

        assert_eq!(Some(attr.into()), nodes.document_order_first());
    }

    #[test]
    fn prefixed_name_of_element_with_preferred_prefix() {
        let package = Package::new();
        let doc = package.as_document();

        let e = doc.create_element(("uri", "wow"));
        e.set_preferred_prefix(Some("prefix"));
        e.register_prefix("prefix", "uri");
        let node: Node<'_> = e.into();

        assert_eq!(Some("prefix:wow".to_owned()), node.prefixed_name());
    }

    #[test]
    fn prefixed_name_of_element_with_prefix() {
        let package = Package::new();
        let doc = package.as_document();

        let e = doc.create_element(("uri", "wow"));
        e.register_prefix("prefix", "uri");
        let node: Node<'_> = e.into();

        assert_eq!(Some("prefix:wow".to_owned()), node.prefixed_name());
    }

    #[test]
    fn prefixed_name_of_element_without_prefix() {
        // See library-level doc about missing prefixes
        let package = Package::new();
        let doc = package.as_document();

        let e = doc.create_element(("uri", "wow"));
        let node: Node<'_> = e.into();

        assert_eq!(Some("wow".to_owned()), node.prefixed_name());
    }

    #[test]
    fn prefixed_name_of_attribute_with_preferred_prefix() {
        let package = Package::new();
        let doc = package.as_document();

        let e = doc.create_element("element");
        let a = e.set_attribute_value(("uri", "attr"), "value");
        a.set_preferred_prefix(Some("prefix"));
        e.register_prefix("prefix", "uri");
        let node: Node<'_> = a.into();

        assert_eq!(Some("prefix:attr".to_owned()), node.prefixed_name());
    }

    #[test]
    fn prefixed_name_of_attribute_with_prefix() {
        let package = Package::new();
        let doc = package.as_document();

        let e = doc.create_element("element");
        let a = e.set_attribute_value(("uri", "attr"), "value");
        e.register_prefix("prefix", "uri");
        let node: Node<'_> = a.into();

        assert_eq!(Some("prefix:attr".to_owned()), node.prefixed_name());
    }

    #[test]
    fn prefixed_name_of_processing_instruction() {
        let package = Package::new();
        let doc = package.as_document();

        let pi = doc.create_processing_instruction("target", Some("value"));
        let node: Node<'_> = pi.into();

        assert_eq!(Some("target".to_owned()), node.prefixed_name());
    }

    #[test]
    fn string_value_of_element_node_is_concatenation_of_descendant_text_nodes() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("hello");
        let child = doc.create_element("world");
        let text1 = doc.create_text("Presenting: ");
        let text2 = doc.create_text("Earth");
        let text3 = doc.create_text("!");

        element.append_child(text1);
        element.append_child(child);
        child.append_child(text2);
        element.append_child(text3);

        assert_eq!("Presenting: Earth!", into_node(element).string_value());
    }

    #[test]
    fn string_value_of_attribute_node_is_value() {
        let package = Package::new();
        let doc = package.as_document();
        let element = doc.create_element("hello");
        let attribute: Node<'_> = element.set_attribute_value("world", "Earth").into();
        assert_eq!("Earth", attribute.string_value());
    }

    #[test]
    fn string_value_of_pi_node_is_empty_when_no_value() {
        let package = Package::new();
        let doc = package.as_document();
        let pi: Node<'_> = doc.create_processing_instruction("hello", None).into();
        assert_eq!("", pi.string_value());
    }

    #[test]
    fn string_value_of_pi_node_is_the_value_when_value() {
        let package = Package::new();
        let doc = package.as_document();
        let pi: Node<'_> = doc
            .create_processing_instruction("hello", Some("world"))
            .into();
        assert_eq!("world", pi.string_value());
    }

    #[test]
    fn string_value_of_comment_node_is_the_text() {
        let package = Package::new();
        let doc = package.as_document();
        let comment: Node<'_> = doc.create_comment("hello world").into();
        assert_eq!("hello world", comment.string_value());
    }

    #[test]
    fn string_value_of_text_node_is_the_text() {
        let package = Package::new();
        let doc = package.as_document();
        let text: Node<'_> = doc.create_text("hello world").into();
        assert_eq!("hello world", text.string_value());
    }
}

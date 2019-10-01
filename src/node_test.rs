use std::fmt;

use sxd_document::QName;

use crate::context;
use crate::nodeset::{self, OrderedNodes};

pub trait NodeTest: fmt::Debug {
    fn test<'c, 'd>(&self, context: &context::Evaluation<'c, 'd>, result: &mut OrderedNodes<'d>);
}

impl<T: ?Sized> NodeTest for Box<T>
where
    T: NodeTest,
{
    fn test<'c, 'd>(&self, context: &context::Evaluation<'c, 'd>, result: &mut OrderedNodes<'d>) {
        (**self).test(context, result)
    }
}

pub type SubNodeTest = Box<dyn NodeTest + 'static>;

#[derive(Debug, Clone, PartialEq)]
pub struct NameTest {
    pub prefix: Option<String>,
    pub local_part: String,
}

impl NameTest {
    fn matches(&self, context: &context::Evaluation, node_name: QName) -> bool {
        let is_wildcard = self.local_part == "*";

        let test_uri = self.prefix.as_ref().map(|p| {
            // TODO: Error for undefined prefix
            context.namespace_for(p).expect("No namespace for prefix")
        });

        match (is_wildcard, test_uri) {
            (true, None) => true,
            (true, Some(..)) => test_uri == node_name.namespace_uri(),
            _ => test_uri == node_name.namespace_uri() && self.local_part == node_name.local_part(),
        }
    }
}

#[derive(Debug)]
pub struct Attribute {
    name_test: NameTest,
}

impl Attribute {
    pub fn new(name: NameTest) -> Attribute {
        Attribute { name_test: name }
    }
}

impl NodeTest for Attribute {
    fn test<'c, 'd>(&self, context: &context::Evaluation<'c, 'd>, result: &mut OrderedNodes<'d>) {
        if let nodeset::Node::Attribute(ref a) = context.node {
            if self.name_test.matches(context, a.name()) {
                result.add(context.node);
            }
        }
    }
}

#[derive(Debug)]
pub struct Namespace {
    name_test: NameTest,
}

impl Namespace {
    pub fn new(name: NameTest) -> Namespace {
        Namespace { name_test: name }
    }
}

impl NodeTest for Namespace {
    fn test<'c, 'd>(&self, context: &context::Evaluation<'c, 'd>, result: &mut OrderedNodes<'d>) {
        if let nodeset::Node::Namespace(ref ns) = context.node {
            if self.name_test.matches(context, QName::new(ns.prefix())) {
                result.add(context.node);
            }
        }
    }
}

#[derive(Debug)]
pub struct Element {
    name_test: NameTest,
}

impl Element {
    pub fn new(name: NameTest) -> Element {
        Element { name_test: name }
    }
}

impl NodeTest for Element {
    fn test<'c, 'd>(&self, context: &context::Evaluation<'c, 'd>, result: &mut OrderedNodes<'d>) {
        if let nodeset::Node::Element(ref e) = context.node {
            if self.name_test.matches(context, e.name()) {
                result.add(context.node);
            }
        }
    }
}

#[allow(missing_copy_implementations)]
#[derive(Debug)]
pub struct Node;

impl NodeTest for Node {
    fn test<'c, 'd>(&self, context: &context::Evaluation<'c, 'd>, result: &mut OrderedNodes<'d>) {
        result.add(context.node);
    }
}

#[allow(missing_copy_implementations)]
#[derive(Debug)]
pub struct Text;

impl NodeTest for Text {
    fn test<'c, 'd>(&self, context: &context::Evaluation<'c, 'd>, result: &mut OrderedNodes<'d>) {
        if let nodeset::Node::Text(_) = context.node {
            result.add(context.node);
        }
    }
}

#[allow(missing_copy_implementations)]
#[derive(Debug)]
pub struct Comment;

impl NodeTest for Comment {
    fn test<'c, 'd>(&self, context: &context::Evaluation<'c, 'd>, result: &mut OrderedNodes<'d>) {
        if let nodeset::Node::Comment(_) = context.node {
            result.add(context.node);
        }
    }
}

#[derive(Debug)]
pub struct ProcessingInstruction {
    target: Option<String>,
}

impl ProcessingInstruction {
    pub fn new(target: Option<String>) -> ProcessingInstruction {
        ProcessingInstruction { target: target }
    }
}

impl NodeTest for ProcessingInstruction {
    fn test<'c, 'd>(&self, context: &context::Evaluation<'c, 'd>, result: &mut OrderedNodes<'d>) {
        if let nodeset::Node::ProcessingInstruction(pi) = context.node {
            match self.target {
                Some(ref name) if name == &pi.target() => result.add(context.node),
                Some(_) => {}
                None => result.add(context.node),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::borrow::ToOwned;

    use sxd_document::dom::{self, Document};
    use sxd_document::{Package, QName};

    use crate::context::{self, Context};
    use crate::nodeset::OrderedNodes;

    use super::*;

    struct Setup<'d> {
        doc: Document<'d>,
        context: Context<'d>,
    }

    impl<'d> Setup<'d> {
        fn new(package: &'d Package) -> Setup {
            Setup {
                doc: package.as_document(),
                context: Context::without_core_functions(),
            }
        }

        fn register_prefix(&mut self, prefix: &str, namespace_uri: &str) {
            self.context.set_namespace(prefix, namespace_uri);
        }

        fn context_for_attribute<'n, N>(
            &'d self,
            name: N,
            val: &str,
        ) -> (dom::Attribute<'d>, context::Evaluation<'d, 'd>)
        where
            N: Into<QName<'n>>,
        {
            let e = self.doc.create_element("element");
            let a = e.set_attribute_value(name, val);
            let c = context::Evaluation::new(&self.context, a.into());
            (a, c)
        }

        fn context_for_ns_attribute(
            &'d mut self,
            prefix: &str,
            nsuri: &str,
            local: &str,
            value: &str,
        ) -> (dom::Attribute<'d>, context::Evaluation<'d, 'd>) {
            self.register_prefix(prefix, nsuri);
            self.context_for_attribute((nsuri, local), value)
        }

        fn context_for_element<'n, N>(
            &'d self,
            name: N,
        ) -> (dom::Element<'d>, context::Evaluation<'d, 'd>)
        where
            N: Into<QName<'n>>,
        {
            let e = self.doc.create_element(name);
            let c = context::Evaluation::new(&self.context, e.into());
            (e, c)
        }

        fn context_for_ns_element(
            &'d mut self,
            prefix: &str,
            nsuri: &str,
            local: &str,
        ) -> (dom::Element<'d>, context::Evaluation<'d, 'd>) {
            self.register_prefix(prefix, nsuri);
            self.context_for_element((nsuri, local))
        }
    }

    fn run_attribute<'d>(
        context: &context::Evaluation<'d, 'd>,
        prefix: Option<&str>,
        local: &str,
    ) -> OrderedNodes<'d> {
        let mut result = OrderedNodes::new();
        let name = NameTest {
            prefix: prefix.map(|p| p.to_owned()),
            local_part: local.to_owned(),
        };
        let test = Attribute::new(name);
        test.test(context, &mut result);
        result
    }

    #[test]
    fn attribute_test_selects_attributes_with_matching_names() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (attribute, context) = setup.context_for_attribute("hello", "world");

        let result = run_attribute(&context, None, "hello");
        assert_eq!(ordered_nodes![attribute], result);
    }

    #[test]
    fn attribute_test_does_not_select_other_names() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (_, context) = setup.context_for_attribute("goodbye", "world");

        let result = run_attribute(&context, None, "hello");
        assert_eq!(ordered_nodes![], result);
    }

    #[test]
    fn attribute_test_supports_a_wildcard_match() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (attribute, context) = setup.context_for_attribute("whatever", "value");

        let result = run_attribute(&context, None, "*");
        assert_eq!(ordered_nodes![attribute], result);
    }

    #[test]
    fn attribute_test_matches_on_namespace_and_name() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        setup.register_prefix("another", "different-uri");
        let (attribute, context) =
            setup.context_for_ns_attribute("prefix", "namespace", "name", "value");

        let result = run_attribute(&context, Some("prefix"), "name");
        assert_eq!(ordered_nodes![attribute], result);

        let result = run_attribute(&context, Some("prefix"), "wrong-name");
        assert_eq!(ordered_nodes![], result);

        let result = run_attribute(&context, Some("another"), "name");
        assert_eq!(ordered_nodes![], result);
    }

    #[test]
    fn attribute_test_matches_on_namespace_when_wildcard_with_prefix() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        setup.register_prefix("another", "different-uri");
        let (attribute, context) =
            setup.context_for_ns_attribute("prefix", "namespace", "name", "value");

        let result = run_attribute(&context, Some("prefix"), "*");
        assert_eq!(ordered_nodes![attribute], result);

        let result = run_attribute(&context, Some("another"), "*");
        assert_eq!(ordered_nodes![], result);
    }

    #[test]
    fn attribute_test_ignores_namespace_when_wildcard_without_prefix() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        let (attribute, context) =
            setup.context_for_ns_attribute("prefix", "namespace", "name", "value");

        let result = run_attribute(&context, None, "*");
        assert_eq!(ordered_nodes![attribute], result);
    }

    #[test]
    fn attribute_test_does_not_match_when_attribute_has_namespace_but_without_prefix() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        let (_, context) = setup.context_for_ns_attribute("prefix", "namespace", "name", "value");

        let result = run_attribute(&context, None, "name");
        assert_eq!(ordered_nodes![], result);
    }

    fn run_element<'d>(
        context: &context::Evaluation<'d, 'd>,
        prefix: Option<&str>,
        local: &str,
    ) -> OrderedNodes<'d> {
        let mut result = OrderedNodes::new();
        let name = NameTest {
            prefix: prefix.map(|p| p.to_owned()),
            local_part: local.to_owned(),
        };
        let test = Element::new(name);
        test.test(context, &mut result);
        result
    }

    #[test]
    fn element_test_selects_nodes_with_matching_names() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (element, context) = setup.context_for_element("hello");

        let result = run_element(&context, None, "hello");
        assert_eq!(ordered_nodes![element], result);
    }

    #[test]
    fn element_test_does_not_select_other_names() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (_, context) = setup.context_for_element("goodbye");

        let result = run_element(&context, None, "hello");
        assert_eq!(ordered_nodes![], result);
    }

    #[test]
    fn element_test_supports_a_wildcard_match() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (element, context) = setup.context_for_element("hello");

        let result = run_element(&context, None, "*");
        assert_eq!(ordered_nodes![element], result);
    }

    #[test]
    fn element_test_matches_on_namespace_and_name() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        setup.register_prefix("another", "different-uri");
        let (element, context) = setup.context_for_ns_element("prefix", "uri", "name");

        let result = run_element(&context, Some("prefix"), "name");
        assert_eq!(ordered_nodes![element], result);

        let result = run_element(&context, Some("prefix"), "wrong-name");
        assert_eq!(ordered_nodes![], result);

        let result = run_element(&context, Some("another"), "name");
        assert_eq!(ordered_nodes![], result);
    }

    #[test]
    fn element_test_matches_on_namespace_when_wildcard_with_prefix() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        setup.register_prefix("another", "different-uri");
        let (element, context) = setup.context_for_ns_element("prefix", "uri", "name");

        let result = run_element(&context, Some("prefix"), "*");
        assert_eq!(ordered_nodes![element], result);

        let result = run_element(&context, Some("another"), "*");
        assert_eq!(ordered_nodes![], result);
    }

    #[test]
    fn element_test_ignores_namespace_when_wildcard_without_prefix() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        let (element, context) = setup.context_for_ns_element("prefix", "uri", "name");

        let result = run_element(&context, None, "*");
        assert_eq!(ordered_nodes![element], result);
    }

    #[test]
    fn element_test_does_not_match_when_element_has_namespace_but_without_prefix() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        let (_, context) = setup.context_for_ns_element("prefix", "uri", "name");

        let result = run_element(&context, None, "name");
        assert_eq!(ordered_nodes![], result);
    }
}

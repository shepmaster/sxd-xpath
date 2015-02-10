use std::fmt;

use document::QName;

use super::EvaluationContext;
use super::nodeset::{self,Nodeset};

pub trait NodeTest: fmt::Debug {
    fn test<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>, result: &mut Nodeset<'d>);
}

pub type SubNodeTest = Box<NodeTest + 'static>;

#[derive(Clone,PartialEq,Debug)]
pub struct NameTest {
    pub prefix: Option<String>,
    pub local_part: String,
}

impl NameTest {
    fn matches(&self, context: &EvaluationContext, node_name: QName) -> bool {
        let is_wildcard = self.local_part == "*";

        let test_uri = self.prefix.as_ref().map(|p| {
            // TODO: Error for undefined prefix
            context.namespace_for(p)
                .expect("No namespace for prefix")
        });

        match (is_wildcard, test_uri) {
            (true, None) => true,
            (true, Some(..)) => test_uri == node_name.namespace_uri(),
            _ => {
                test_uri == node_name.namespace_uri() &&
                    self.local_part == node_name.local_part()
            },
        }
    }
}

#[derive(Debug)]
pub struct Attribute {
    name_test: NameTest,
}

impl Attribute {
    pub fn new(name: NameTest) -> Attribute {
        Attribute {
            name_test: name,
        }
    }
}

impl NodeTest for Attribute {
    fn test<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
        if let nodeset::Node::Attribute(ref a) = context.node {
            if self.name_test.matches(context, a.name()) {
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
        Element {
            name_test: name,
        }
    }
}

impl NodeTest for Element {
    fn test<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
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
    fn test<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
        result.add(context.node);
    }
}

#[allow(missing_copy_implementations)]
#[derive(Debug)]
pub struct Text;

impl NodeTest for Text {
    fn test<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
        if let nodeset::Node::Text(_) = context.node {
            result.add(context.node);
        }
    }
}

#[allow(missing_copy_implementations)]
#[derive(Debug)]
pub struct Comment;

impl NodeTest for Comment {
    fn test<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
        if let nodeset::Node::Comment(_) = context.node {
            result.add(context.node);
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use document::{Package,ToQName};
    use document::dom4::{Document,Element,Attribute};

    use super::super::{EvaluationContext,Functions,Variables,Namespaces};
    use super::super::nodeset::Nodeset;

    use super::super::node_test;
    use super::NodeTest;

    struct Setup<'d> {
        doc: Document<'d>,
        functions: Functions,
        variables: Variables<'d>,
        namespaces: Namespaces,
    }

    impl<'d> Setup<'d> {
        fn new(package: &'d Package) -> Setup {
            Setup {
                doc: package.as_document(),
                functions: HashMap::new(),
                variables: HashMap::new(),
                namespaces: HashMap::new(),
            }
        }

        fn register_prefix(&mut self, prefix: &str, namespace_uri: &str) {
            self.namespaces.insert(prefix.to_string(), namespace_uri.to_string());
        }

        fn context_for_attribute<'n, N>(&'d self, name: N, val: &str)
                                        -> (Attribute<'d>, EvaluationContext<'d, 'd>)
            where N: ToQName<'n>
        {
            let e = self.doc.create_element("element");
            let a = e.set_attribute_value(name, val);
            let c = EvaluationContext::new(a, &self.functions, &self.variables, &self.namespaces);
            (a, c)
        }

        fn context_for_ns_attribute(&'d mut self, prefix: &str, nsuri: &str, local: &str, value: &str)
                                    -> (Attribute<'d>, EvaluationContext<'d, 'd>)
        {
            self.register_prefix(prefix, nsuri);
            self.context_for_attribute((nsuri, local), value)
        }

        fn context_for_element<'n, N>(&'d self, name: N)
                                  -> (Element<'d>, EvaluationContext<'d, 'd>)
            where N: ToQName<'n>
        {
            let e = self.doc.create_element(name);
            let c = EvaluationContext::new(e, &self.functions, &self.variables, &self.namespaces);
            (e, c)
        }

        fn context_for_ns_element(&'d mut self, prefix: &str, nsuri: &str, local: &str)
                                  -> (Element<'d>, EvaluationContext<'d, 'd>)
        {
            self.register_prefix(prefix, nsuri);
            self.context_for_element((nsuri, local))
        }
    }

    fn run_attribute<'d>(context: &EvaluationContext<'d, 'd>, prefix: Option<&str>, local: &str)
                       -> Nodeset<'d>
    {
        let mut result = Nodeset::new();
        let name = super::NameTest {
            prefix: prefix.map(|p| p.to_string()),
            local_part: local.to_string()
        };
        let test = node_test::Attribute::new(name);
        test.test(context, &mut result);
        result
    }

    #[test]
    fn attribute_test_selects_attributes_with_matching_names() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (attribute, context) = setup.context_for_attribute("hello", "world");

        let result = run_attribute(&context, None, "hello");
        assert_eq!(nodeset![attribute], result);
    }

    #[test]
    fn attribute_test_does_not_select_other_names() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (_, context) = setup.context_for_attribute("goodbye", "world");

        let result = run_attribute(&context, None, "hello");
        assert_eq!(nodeset![], result);
    }

    #[test]
    fn attribute_test_supports_a_wildcard_match() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (attribute, context) = setup.context_for_attribute("whatever", "value");

        let result = run_attribute(&context, None, "*");
        assert_eq!(nodeset![attribute], result);
    }

    #[test]
    fn attribute_test_matches_on_namespace_and_name() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        setup.register_prefix("another", "different-uri");
        let (attribute, context) = setup.context_for_ns_attribute("prefix", "namespace", "name", "value");

        let result = run_attribute(&context, Some("prefix"), "name");
        assert_eq!(nodeset![attribute], result);

        let result = run_attribute(&context, Some("prefix"), "wrong-name");
        assert_eq!(nodeset![], result);

        let result = run_attribute(&context, Some("another"), "name");
        assert_eq!(nodeset![], result);
    }

    #[test]
    fn attribute_test_matches_on_namespace_when_wildcard_with_prefix() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        setup.register_prefix("another", "different-uri");
        let (attribute, context) = setup.context_for_ns_attribute("prefix", "namespace", "name", "value");

        let result = run_attribute(&context, Some("prefix"), "*");
        assert_eq!(nodeset![attribute], result);

        let result = run_attribute(&context, Some("another"), "*");
        assert_eq!(nodeset![], result);
    }

    #[test]
    fn attribute_test_ignores_namespace_when_wildcard_without_prefix() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        let (attribute, context) = setup.context_for_ns_attribute("prefix", "namespace", "name", "value");

        let result = run_attribute(&context, None, "*");
        assert_eq!(nodeset![attribute], result);
    }

    #[test]
    fn attribute_test_does_not_match_when_attribute_has_namespace_but_without_prefix() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        let (_, context) = setup.context_for_ns_attribute("prefix", "namespace", "name", "value");

        let result = run_attribute(&context, None, "name");
        assert_eq!(nodeset![], result);
    }

    fn run_element<'d>(context: &EvaluationContext<'d, 'd>, prefix: Option<&str>, local: &str)
                       -> Nodeset<'d>
    {
        let mut result = Nodeset::new();
        let name = super::NameTest {
            prefix: prefix.map(|p| p.to_string()),
            local_part: local.to_string()
        };
        let test = node_test::Element::new(name);
        test.test(context, &mut result);
        result
    }

    #[test]
    fn element_test_selects_nodes_with_matching_names() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (element, context) = setup.context_for_element("hello");

        let result = run_element(&context, None, "hello");
        assert_eq!(nodeset![element], result);
    }

    #[test]
    fn element_test_does_not_select_other_names() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (_, context) = setup.context_for_element("goodbye");

        let result = run_element(&context, None, "hello");
        assert_eq!(nodeset![], result);
    }

    #[test]
    fn element_test_supports_a_wildcard_match() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (element, context) = setup.context_for_element("hello");

        let result = run_element(&context, None, "*");
        assert_eq!(nodeset![element], result);
    }

    #[test]
    fn element_test_matches_on_namespace_and_name() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        setup.register_prefix("another", "different-uri");
        let (element, context) = setup.context_for_ns_element("prefix", "uri", "name");

        let result = run_element(&context, Some("prefix"), "name");
        assert_eq!(nodeset![element], result);

        let result = run_element(&context, Some("prefix"), "wrong-name");
        assert_eq!(nodeset![], result);

        let result = run_element(&context, Some("another"), "name");
        assert_eq!(nodeset![], result);
    }

    #[test]
    fn element_test_matches_on_namespace_when_wildcard_with_prefix() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        setup.register_prefix("another", "different-uri");
        let (element, context) = setup.context_for_ns_element("prefix", "uri", "name");

        let result = run_element(&context, Some("prefix"), "*");
        assert_eq!(nodeset![element], result);

        let result = run_element(&context, Some("another"), "*");
        assert_eq!(nodeset![], result);
    }

    #[test]
    fn element_test_ignores_namespace_when_wildcard_without_prefix() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        let (element, context) = setup.context_for_ns_element("prefix", "uri", "name");

        let result = run_element(&context, None, "*");
        assert_eq!(nodeset![element], result);
    }

    #[test]
    fn element_test_does_not_match_when_element_has_namespace_but_without_prefix() {
        let package = Package::new();
        let mut setup = Setup::new(&package);
        let (_, context) = setup.context_for_ns_element("prefix", "uri", "name");

        let result = run_element(&context, None, "name");
        assert_eq!(nodeset![], result);
    }
}

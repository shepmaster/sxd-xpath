use super::EvaluationContext;
use super::nodeset::Nodeset;
use super::nodeset::Node::{AttributeNode,ElementNode,TextNode};

pub trait NodeTest {
    fn test<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>, result: &mut Nodeset<'d>);
}

pub type SubNodeTest = Box<NodeTest + 'static>;

pub struct Attribute {
    pub name: String,
}

impl NodeTest for Attribute {
    fn test<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
        if let AttributeNode(ref a) = context.node {
            if self.name == "*" || a.name().local_part() == self.name {
                result.add(context.node);
            }
        }
    }
}

pub struct Element {
    pub name: String,
}

impl NodeTest for Element {
    fn test<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
        if let ElementNode(ref e) = context.node {
            // TODO: redo namespaces!
            // if (_name.has_prefix() != e->qname().has_namespace()) return;

            // if (_name.has_prefix()) {
            //     let prefix_uri = context.find_namespace_for_prefix(_name.prefix());

            //     if (! prefix_uri) return;
            //     if (*prefix_uri != e->qname().namespace_uri()) return;
            // }

            if self.name == "*" || e.name().local_part() == self.name {
                result.add(context.node);
            }
        }
    }
}

#[allow(missing_copy_implementations)]
pub struct Node;

impl NodeTest for Node {
    fn test<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
        result.add(context.node);
    }
}

#[allow(missing_copy_implementations)]
pub struct Text;

impl NodeTest for Text {
    fn test<'a, 'd>(&self, context: &EvaluationContext<'a, 'd>, result: &mut Nodeset<'d>) {
        if let TextNode(_) = context.node {
            result.add(context.node);
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use document::Package;
    use document::dom4::{Document,Element,Attribute};

    use super::super::EvaluationContext;
    use super::super::{Functions,Variables};
    use super::super::nodeset::Nodeset;

    use super::super::node_test;
    use super::NodeTest;

    struct Setup<'d> {
        doc: Document<'d>,
        functions: Functions,
        variables: Variables<'d>,
    }

    impl<'d> Setup<'d> {
        fn new(package: &'d Package) -> Setup {
            Setup {
                doc: package.as_document(),
                functions: HashMap::new(),
                variables: HashMap::new(),
            }
        }

        fn context_for_attribute(&'d self, name: &str, val: &str)
                                 -> (Attribute<'d>, EvaluationContext<'d, 'd>)
        {
            let e = self.doc.create_element("element");
            let a = e.set_attribute_value(name, val);
            let c = EvaluationContext::new(a, &self.functions, &self.variables);
            (a, c)
        }

        fn context_for_element(&'d self, name: &str)
                               -> (Element<'d>, EvaluationContext<'d, 'd>)
        {
            let e = self.doc.create_element(name);
            let c = EvaluationContext::new(e, &self.functions, &self.variables);
            (e, c)
        }
    }

    #[test]
    fn attribute_test_selects_attributes_with_matching_names() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (attribute, context) = setup.context_for_attribute("hello", "world");
        let mut result = Nodeset::new();

        let test = node_test::Attribute{name: "hello".to_string()};
        test.test(&context, &mut result);

        assert_eq!(nodeset![attribute], result);
    }

    #[test]
    fn attribute_test_does_not_select_other_names() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (_, context) = setup.context_for_attribute("goodbye", "world");
        let mut result = Nodeset::new();

        let test = node_test::Attribute{name: "hello".to_string()};
        test.test(&context, &mut result);

        assert_eq!(nodeset![], result);
    }

    #[test]
    fn attribute_test_supports_a_wildcard_match() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (attribute, context) = setup.context_for_attribute("whatever", "value");
        let mut result = Nodeset::new();

        let test = node_test::Attribute{name: "*".to_string()};
        test.test(&context, &mut result);

        assert_eq!(nodeset![attribute], result);
    }

    #[test]
    fn element_test_selects_nodes_with_matching_names() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (element, context) = setup.context_for_element("hello");
        let mut result = Nodeset::new();

        let test = node_test::Element{name: "hello".to_string()};
        test.test(&context, &mut result);

        assert_eq!(nodeset![element], result);
    }

    #[test]
    fn element_test_does_not_select_other_names() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (_, context) = setup.context_for_element("goodbye");
        let mut result = Nodeset::new();

        let test = node_test::Element{name: "hello".to_string()};
        test.test(&context, &mut result);

        assert_eq!(nodeset![], result);
    }

    #[test]
    fn element_test_supports_a_wildcard_match() {
        let package = Package::new();
        let setup = Setup::new(&package);
        let (element, context) = setup.context_for_element("hello");
        let mut result = Nodeset::new();

        let test = node_test::Element{name: "*".to_string()};
        test.test(&context, &mut result);

        assert_eq!(nodeset![element], result);
    }
}

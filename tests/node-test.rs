#![feature(phase)]

extern crate document;
#[phase(plugin, link)]
extern crate xpath;

use std::collections::HashMap;

use document::Package;
use document::dom4::{Document,Element,Attribute};

use xpath::EvaluationContext;
use xpath::{Functions,Variables};
use xpath::nodeset::Nodeset;

use xpath::node_test::XPathNodeTest;
use xpath::node_test::{NodeTestAttribute, NodeTestElement};

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

    let test = NodeTestAttribute{name: "hello".to_string()};
    test.test(&context, &mut result);

    assert_eq!(nodeset![attribute], result);
}

#[test]
fn attribute_test_does_not_select_other_names() {
    let package = Package::new();
    let setup = Setup::new(&package);
    let (_, context) = setup.context_for_attribute("goodbye", "world");
    let mut result = Nodeset::new();

    let test = NodeTestAttribute{name: "hello".to_string()};
    test.test(&context, &mut result);

    assert_eq!(nodeset![], result);
}

#[test]
fn attribute_test_supports_a_wildcard_match() {
    let package = Package::new();
    let setup = Setup::new(&package);
    let (attribute, context) = setup.context_for_attribute("whatever", "value");
    let mut result = Nodeset::new();

    let test = NodeTestAttribute{name: "*".to_string()};
    test.test(&context, &mut result);

    assert_eq!(nodeset![attribute], result);
}

#[test]
fn element_test_selects_nodes_with_matching_names() {
    let package = Package::new();
    let setup = Setup::new(&package);
    let (element, context) = setup.context_for_element("hello");
    let mut result = Nodeset::new();

    let test = NodeTestElement{name: "hello".to_string()};
    test.test(&context, &mut result);

    assert_eq!(nodeset![element], result);
}

#[test]
fn element_test_does_not_select_other_names() {
    let package = Package::new();
    let setup = Setup::new(&package);
    let (_, context) = setup.context_for_element("goodbye");
    let mut result = Nodeset::new();

    let test = NodeTestElement{name: "hello".to_string()};
    test.test(&context, &mut result);

    assert_eq!(nodeset![], result);
}

#[test]
fn element_test_supports_a_wildcard_match() {
    let package = Package::new();
    let setup = Setup::new(&package);
    let (element, context) = setup.context_for_element("hello");
    let mut result = Nodeset::new();

    let test = NodeTestElement{name: "*".to_string()};
    test.test(&context, &mut result);

    assert_eq!(nodeset![element], result);
}

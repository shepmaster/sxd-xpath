#![feature(phase)]

#[phase(plugin, link)]
extern crate document;
extern crate xpath;

use std::collections::hashmap::HashMap;

use document::{Document,Element,Attribute,Nodeset};
use document::{ToAny};

use xpath::XPathEvaluationContext;
use xpath::{Functions,Variables};

use xpath::node_test::XPathNodeTest;
use xpath::node_test::{NodeTestAttribute, NodeTestElement};

struct Setup {
    doc: Document,
    element: Element,
    functions: Functions,
    variables: Variables,
}

impl Setup {
    fn new() -> Setup {
        let d = Document::new();
        let e = d.new_element("element".to_string());
        Setup {
            doc: d,
            element: e,
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    fn context_for_attribute<'a>(&'a self, name: &str, val: &str)
                                 -> (Attribute, XPathEvaluationContext<'a>)
    {
        let a = self.element.set_attribute(name.to_string(), val.to_string());
        let c = XPathEvaluationContext::new(a.to_any(), &self.functions, &self.variables);
        (a, c)
    }

    fn context_for_element<'a>(&'a self, name: &str)
                               -> (Element, XPathEvaluationContext<'a>)
    {
        let e = self.doc.new_element(name.to_string());
        let c = XPathEvaluationContext::new(e.to_any(), &self.functions, &self.variables);
        (e, c)
    }
}

#[test]
fn attribute_test_selects_attributes_with_matching_names() {
    let setup = Setup::new();
    let (attribute, context) = setup.context_for_attribute("hello", "world");
    let mut result = Nodeset::new();

    let test = NodeTestAttribute{name: "hello".to_string()};
    test.test(&context, &mut result);

    assert_eq!(nodeset![attribute], result);
}

#[test]
fn attribute_test_does_not_select_other_names() {
    let setup = Setup::new();
    let (_, context) = setup.context_for_attribute("goodbye", "world");
    let mut result = Nodeset::new();

    let test = NodeTestAttribute{name: "hello".to_string()};
    test.test(&context, &mut result);

    assert_eq!(nodeset![], result);
}

#[test]
fn attribute_test_supports_a_wildcard_match() {
    let setup = Setup::new();
    let (attribute, context) = setup.context_for_attribute("whatever", "value");
    let mut result = Nodeset::new();

    let test = NodeTestAttribute{name: "*".to_string()};
    test.test(&context, &mut result);

    assert_eq!(nodeset![attribute], result);
}

#[test]
fn element_test_selects_nodes_with_matching_names() {
    let setup = Setup::new();
    let (element, context) = setup.context_for_element("hello");
    let mut result = Nodeset::new();

    let test = NodeTestElement{name: "hello".to_string()};
    test.test(&context, &mut result);

    assert_eq!(nodeset![element], result);
}

#[test]
fn element_test_does_not_select_other_names() {
    let setup = Setup::new();
    let (_, context) = setup.context_for_element("goodbye");
    let mut result = Nodeset::new();

    let test = NodeTestElement{name: "hello".to_string()};
    test.test(&context, &mut result);

    assert_eq!(nodeset![], result);
}

#[test]
fn element_test_supports_a_wildcard_match() {
    let setup = Setup::new();
    let (element, context) = setup.context_for_element("hello");
    let mut result = Nodeset::new();

    let test = NodeTestElement{name: "*".to_string()};
    test.test(&context, &mut result);

    assert_eq!(nodeset![element], result);
}

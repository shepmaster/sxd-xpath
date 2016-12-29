extern crate sxd_document;
extern crate sxd_xpath;

use std::borrow::ToOwned;
use sxd_document::dom::Document;
use sxd_document::parser::parse;
use sxd_xpath::{Value, Factory, ContextCore};

#[test]
fn functions_accept_arguments() {
    let package = parse("<a/>").unwrap();
    let doc = package.as_document();

    let result = evaluate(&doc, "concat('hello', ' ', 'world')");

    assert_eq!(Value::String("hello world".to_owned()), result);
}

#[test]
fn axis_is_fully_applied_before_predicates_filter() {
    let package = parse("<a><b/></a>").unwrap();
    let doc = package.as_document();

    let result = evaluate(&doc, "count(//*[1])");

    assert_eq!(Value::Number(1.0), result);
}

#[test]
fn position_function_in_predicate() {
    let package = parse("<a><b/><b/></a>").unwrap();
    let doc = package.as_document();

    let result = evaluate(&doc, "count(//a/*[position() = 2])");

    assert_eq!(Value::Number(1.0), result);
}

fn evaluate<'d>(package: &'d Document<'d>, xpath: &str) -> Value<'d> {
    let setup = Setup::new();
    setup.evaluate(package, xpath)
}

#[derive(Default)]
struct Setup<'d> {
    context: ContextCore<'d>,
    factory: Factory,
}

impl<'d> Setup<'d> {
    fn new() -> Setup<'d> {
        Default::default()
    }

    fn evaluate(&self, doc: &'d Document<'d>, xpath: &str) -> Value<'d> {
        let xpath = self.factory.build(xpath).unwrap().unwrap();
        xpath.evaluate(&self.context.borrow_with_context_node(doc.root()).evaluation_context()).ok().unwrap()
    }
}

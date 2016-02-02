extern crate sxd_document;
extern crate sxd_xpath;

use std::borrow::ToOwned;
use std::collections::HashMap;
use sxd_document::dom::Document;
use sxd_document::parser::parse;
use sxd_xpath::{Value,Functions,Variables,Namespaces,Factory,EvaluationContext,Expression};

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

struct Setup<'d> {
    functions: Functions,
    variables: Variables<'d>,
    namespaces: Namespaces,
    factory: Factory,
}

impl<'d> Setup<'d> {
    fn new() -> Setup<'d> {
        let mut fns = HashMap::new();
        sxd_xpath::function::register_core_functions(&mut fns);
        Setup {
            functions: fns,
            variables: HashMap::new(),
            namespaces: HashMap::new(),
            factory: Factory::new(),
        }
    }

    fn evaluate(&self, doc: &'d Document<'d>, xpath: &str) -> Value<'d> {
        let root = doc.root();
        let context = EvaluationContext::new(
            root,
            &self.functions,
            &self.variables,
            &self.namespaces,
        );

        let xpath = self.factory.build(xpath).unwrap().unwrap();
        xpath.evaluate(&context).ok().unwrap()
    }
}

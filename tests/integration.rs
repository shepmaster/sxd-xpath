extern crate document;
extern crate xpath;

use std::collections::HashMap;
use document::Package;
use document::dom4::Document;
use document::parser::Parser;
use xpath::{Value,Functions,Variables,Namespaces,Factory,EvaluationContext};
use xpath::expression::Expression;

#[test]
fn axis_is_fully_applied_before_predicates_filter() {
    let package = parse("<a><b/></a>");
    let doc = package.as_document();

    let result = evaluate(&doc, "count(//*[1])");

    assert_eq!(Value::Number(1.0), result);
}

fn parse(xml: &str) -> Package {
    Parser::new().parse(xml).unwrap()
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
        xpath::function::register_core_functions(&mut fns);
        Setup {
            functions: fns,
            variables: HashMap::new(),
            namespaces: HashMap::new(),
            factory: Factory::new(),
        }
    }

    fn evaluate(&self, doc: &'d Document<'d>, xpath: &str) -> Value<'d> {
        let root = doc.root();
        let mut context = EvaluationContext::new(root,
                                                 &self.functions,
                                                 &self.variables,
                                                 &self.namespaces);
        context.next(root);

        let xpath = self.factory.build(xpath).unwrap().unwrap();
        xpath.evaluate(&context)
    }
}

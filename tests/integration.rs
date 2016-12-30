extern crate sxd_document;
#[macro_use]
extern crate sxd_xpath;

use std::borrow::ToOwned;
use sxd_document::{dom, parser};
use sxd_xpath::{Value, Factory, Context, evaluate_xpath};
use sxd_xpath::{context, function};

#[test]
fn functions_accept_arguments() {
    with_document("<a/>", |doc| {
        let result = evaluate_xpath(&doc, "concat('hello', ' ', 'world')");

        assert_eq!(Ok(Value::String("hello world".to_owned())), result);
    });
}

#[test]
fn axis_is_fully_applied_before_predicates_filter() {
    with_document("<a><b/></a>", |doc| {
        let result = evaluate_xpath(&doc, "count(//*[1])");

        assert_eq!(Ok(Value::Number(1.0)), result);
    });
}

#[test]
fn position_function_in_predicate() {
    with_document("<a><b/><b/></a>", |doc| {
        let result = evaluate_xpath(&doc, "count(//a/*[position() = 2])");

        assert_eq!(Ok(Value::Number(1.0)), result);
    });
}

#[test]
fn variables_with_qualified_names() {
    with_document("<a/>", |doc| {
        let mut setup = Setup::new();
        setup.context.set_variable(("uri:namespace", "name"), 42.0);
        setup.context.set_namespace("prefix", "uri:namespace");

        let result = setup.evaluate(&doc, "$prefix:name");

        assert_eq!(42.0, result);
    });
}

#[test]
fn functions_with_qualified_names() {
    with_document("<a/>", |doc| {
        let mut setup = Setup::new();
        setup.context.set_function(("uri:namespace", "constant"), ConstantValueFunction(42.0));
        setup.context.set_namespace("prefix", "uri:namespace");

        let result = setup.evaluate(&doc, "prefix:constant()");

        assert_eq!(42.0, result);
    });
}

#[test]
fn nodesets_are_unique() {
    with_document("<a/>", |doc| {
        let result = evaluate_xpath(&doc, "/ | /");

        assert_eq!(Ok(Value::Nodeset(nodeset![doc.root()])), result);
    });
}

fn with_document<F>(xml: &str, f: F)
    where F: FnOnce(dom::Document),
{
    let package = parser::parse(xml).expect("Unable to parse test XML");
    f(package.as_document());
}

#[derive(Default)]
struct Setup<'d> {
    context: Context<'d>,
    factory: Factory,
}

impl<'d> Setup<'d> {
    fn new() -> Setup<'d> {
        Default::default()
    }

    fn evaluate(&self, doc: &'d dom::Document<'d>, xpath: &str) -> Value<'d> {
        let xpath = self.factory.build(xpath).unwrap().unwrap();
        xpath.evaluate(&self.context, doc.root()).unwrap()
    }
}

struct ConstantValueFunction(f64);

impl function::Function for ConstantValueFunction {
    fn evaluate<'c, 'd>(&self,
                        _context: &context::Evaluation<'c, 'd>,
                        _args: Vec<Value<'d>>) -> Result<Value<'d>, function::Error>
    {
        Ok(Value::Number(self.0))
    }
}

extern crate document;
extern crate xpath;

use std::cmp::min;
use std::collections::HashMap;
use std::io::File;

use document::dom4::ToNode;
use document::parser::Parser;

use xpath::{XPathEvaluationContext,XPathFactory};
use xpath::expression::XPathExpression;

fn pretty_error(xml: &str, position: uint) -> &str {
    let s = xml.slice_from(position);
    let l = s.char_len();
    s.slice_chars(0, min(l, 15))
}

fn main() {
    let mut args = std::os::args();

    let filename = args.remove(1).expect("File required");
    let xpath_str = args.remove(1).expect("XPath required");

    let factory = XPathFactory::new();

    let expr = match factory.build(xpath_str.as_slice()) {
        Err(x) => panic!("Unable to compile XPath: {}", x),
        Ok(None) => panic!("Unable to compile XPath"),
        Ok(Some(x)) => x,
    };

    let p = Parser::new();

    let path = Path::new(filename);
    let mut file = File::open(&path);

    let data = match file.read_to_string() {
        Ok(x) => x,
        Err(x) => panic!("Can't read: {}", x),
    };

    let package = match p.parse(data.as_slice()) {
        Ok(d) => d,
        Err(point) => panic!("Unable to parse: {}", pretty_error(data.as_slice(), point)),
    };

    let d = package.as_document();

    let mut functions = HashMap::new();
    xpath::function::register_core_functions(& mut functions);
    let variables = HashMap::new();
    let mut context = XPathEvaluationContext::new(d.root().to_node(),
                                                  &functions,
                                                  &variables);
    context.next(d.root().to_node());

    let res = expr.evaluate(&context);

    println!("{}", res);
}

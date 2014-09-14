extern crate document;
extern crate xpath;

use std::cmp::min;
use std::collections::hashmap::HashMap;
use std::io::File;

use document::ToAny;
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
        Err(x) => fail!("Unable to compile XPath: {}", x),
        Ok(None) => fail!("Unable to compile XPath"),
        Ok(Some(x)) => x,
    };

    let p = Parser::new();

    let path = Path::new(filename);
    let mut file = File::open(&path);

    let data = match file.read_to_end() {
        Ok(x) => x,
        Err(x) => fail!("Can't read: {}", x),
    };

    let data = match String::from_utf8(data) {
        Ok(x) => x,
        Err(x) => fail!("Unable to convert to UTF-8: {}", x),
    };

    let d = match p.parse(data.as_slice()) {
        Ok(d) => d,
        Err(point) => fail!("Unable to parse: {}", pretty_error(data.as_slice(), point)),
    };

    let mut functions = HashMap::new();
    xpath::function::register_core_functions(& mut functions);
    let variables = HashMap::new();
    let mut context = XPathEvaluationContext::new(d.root().to_any(),
                                                  &functions,
                                                  &variables);
    context.next(d.root().to_any());

    let res = expr.evaluate(&context);

    println!("{}", res);
}

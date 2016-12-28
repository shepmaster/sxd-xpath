extern crate sxd_document;
extern crate sxd_xpath;
extern crate getopts;

use std::borrow::ToOwned;
use std::env;
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, Read};

use sxd_document::parser::parse;

use sxd_xpath::{EvaluationContext, Factory, Expression, Value};

use getopts::Options;

fn print_usage(program: &str, opts: &Options) {
    let brief = format!("Usage: {} [options] FILE...", program);
    print!("{}", opts.usage(&brief));
}

fn pretty_error(xml: &str, position: usize) -> String {
    let s = &xml[position..];
    s.chars().take(15).collect()
}

fn build_xpath(xpath_str: &str) -> Box<Expression> {
    let factory = Factory::new();

    match factory.build(xpath_str) {
        Err(x) => panic!("Unable to compile XPath: {:?}", x),
        Ok(None) => panic!("Unable to compile XPath"),
        Ok(Some(x)) => x,
    }
}

fn load_xml<R>(input: R) -> sxd_document::Package
    where R: Read
{
    let mut input = input;
    let mut data = String::new();

    if let Err(x) = input.read_to_string(&mut data) {
        panic!("Can't read: {}", x);
    }

    match parse(&data) {
        Ok(d) => d,
        Err((point, _)) => panic!("Unable to parse: {}", pretty_error(&data, point)),
    }
}

fn argument_name_value(s: &str, delim: char) -> Option<(&str, &str)> {
    let parts: Vec<_> = s.splitn(2, delim).collect();
    if parts.len() < 2 {
        None
    } else {
        Some((parts[0], parts[1]))
    }
}

fn build_functions() -> sxd_xpath::Functions {
    let mut functions = HashMap::new();
    sxd_xpath::function::register_core_functions(&mut functions);
    functions
}

fn build_variables<'a>(arguments: &getopts::Matches) -> sxd_xpath::Variables<'a> {
    let mut vars = HashMap::new();

    for boolean in arguments.opt_strs("boolean") {
        match argument_name_value(&boolean, '=') {
            Some((name, "true")) => { vars.insert(name.to_owned(), Value::Boolean(true)); },
            Some((name, "false")) => { vars.insert(name.to_owned(), Value::Boolean(false)); },
            Some((_, val)) => panic!("Unknown boolean value '{}'", val),
            None => panic!("boolean argument '{}' is malformed", boolean),
        }
    }

    for number in arguments.opt_strs("number") {
        match argument_name_value(&number, '=') {
            Some((name, val)) => {
                match val.parse() {
                    Ok(val) => { vars.insert(name.to_owned(), Value::Number(val)); },
                    Err(e) => panic!("Unknown numeric value '{}': {}", val, e),
                }
            },
            None => panic!("number argument '{}' is malformed", number),
        }
    }

    for string in arguments.opt_strs("string") {
        match argument_name_value(&string, '=') {
            Some((name, val)) => { vars.insert(name.to_owned(), Value::String(val.to_owned())); },
            None => panic!("string argument '{}' is malformed", string),
        }
    }

    vars
}

fn build_namespaces(arguments: &getopts::Matches) -> sxd_xpath::Namespaces {
    let mut namespaces = HashMap::new();

    for ns_defn in arguments.opt_strs("namespace") {
        match argument_name_value(&ns_defn, ':') {
            Some((prefix, uri)) => { namespaces.insert(prefix.to_owned(), uri.to_owned()); },
            None => panic!("Namespace definition '{}' is malformed", ns_defn),
        }
    }

    namespaces
}

fn main() {
    let args: Vec<_> = env::args().collect();

    let program_name = &args[0];

    let mut opts = Options::new();

    opts.reqopt("", "xpath", "The XPath to execute", "XPATH");
    opts.optmulti("", "namespace", "set namespace prefix", "PREFIX:URI");
    opts.optmulti("", "string", "set string variable", "NAME=VALUE");
    opts.optmulti("", "number", "set number variable", "NAME=VALUE");
    opts.optmulti("", "boolean", "set boolean variable", "NAME=VALUE");

    let arguments = match opts.parse(&args[1..]) {
        Ok(x) => x,
        Err(x) => {
            println!("{}", x);
            print_usage(program_name, &opts);
            return;
        },
    };

    let xpath_str = arguments.opt_str("xpath").unwrap();
    let xpath = build_xpath(&xpath_str);

    for filename in &arguments.free {
        let package = if *filename == "-" {
            load_xml(io::stdin())
        } else {
            let file = File::open(filename).unwrap();
            load_xml(file)
        };

        let doc = package.as_document();

        let functions = build_functions();
        let variables = build_variables(&arguments);
        let namespaces = build_namespaces(&arguments);

        let context = EvaluationContext::new(
            doc.root(),
            &functions,
            &variables,
            &namespaces,
        );

        let res = xpath.evaluate(&context);

        println!("{:?}", res);
    }
}

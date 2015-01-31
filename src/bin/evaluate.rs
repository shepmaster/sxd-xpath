#![feature(collections)]
#![feature(core)]
#![feature(io)]
#![feature(os)]
#![feature(path)]
#![feature(rustc_private)]

extern crate document;
extern crate xpath;
extern crate getopts;

use std::cmp::min;
use std::collections::HashMap;
use std::old_io::File;

use document::parser::Parser;

use xpath::nodeset::ToNode;
use xpath::{EvaluationContext,Factory,Expression};

use getopts::{reqopt,optmulti,getopts,OptGroup,usage};

fn print_usage(program: &str, opts: &[OptGroup]) {
    let brief = format!("Usage: {} [options] FILE...", program);
    print!("{}", usage(brief.as_slice(), opts));
}

fn pretty_error(xml: &str, position: usize) -> &str {
    let s = &xml[position..];
    let l = s.chars().count();
    s.slice_chars(0, min(l, 15))
}

fn build_xpath(xpath_str: &str) -> Box<Expression> {
    let factory = Factory::new();

    match factory.build(xpath_str) {
        Err(x) => panic!("Unable to compile XPath: {:?}", x),
        Ok(None) => panic!("Unable to compile XPath"),
        Ok(Some(x)) => x,
    }
}

fn load_xml(filename: &str) -> document::Package {
    let p = Parser::new();
    let path = Path::new(filename);
    let mut file = File::open(&path);

    let data = match file.read_to_string() {
        Ok(x) => x,
        Err(x) => panic!("Can't read: {}", x),
    };

    match p.parse(data.as_slice()) {
        Ok(d) => d,
        Err(point) => panic!("Unable to parse: {}", pretty_error(data.as_slice(), point)),
    }
}



fn build_functions() -> xpath::Functions {
    let mut functions = HashMap::new();
    xpath::function::register_core_functions(&mut functions);
    functions
}

fn build_variables<'a>() -> xpath::Variables<'a> {
    HashMap::new()
}

fn build_namespaces(arguments: &getopts::Matches) -> xpath::Namespaces {
    let mut namespaces = HashMap::new();
    for ns_defn in arguments.opt_strs("namespace").iter() {
        let parts: Vec<_> = ns_defn.splitn(2, ':').collect();
        if parts.len() < 2 {
            panic!("Namespace definition '{}' is malformed", ns_defn);
        }

        namespaces.insert(parts[0].to_string(), parts[1].to_string());
    }
    namespaces
}

#[allow(dead_code)]
fn main() {
    let args = std::os::args();
    let program_name = &args[0];

    let opts = &[
        reqopt("", "xpath", "The XPath to execute", "XPATH"),
        optmulti("", "namespace", "set namespace prefix", "PREFIX:URI"),
    ];

    let arguments = match getopts(args.tail(), opts) {
        Ok(x) => x,
        Err(x) => {
            println!("{}", x);
            print_usage(program_name.as_slice(), opts);
            return;
        },
    };

    let xpath_str = arguments.opt_str("xpath").unwrap();
    let xpath = build_xpath(xpath_str.as_slice());

    for filename in arguments.free.iter() {
        let package = load_xml(filename.as_slice());
        let doc = package.as_document();
        let root = doc.root().to_node();

        let functions = build_functions();
        let variables = build_variables();
        let namespaces = build_namespaces(&arguments);

        let context = EvaluationContext::new(
            root,
            &functions,
            &variables,
            &namespaces,
        );

        let res = xpath.evaluate(&context);

        println!("{:?}", res);
    }
}

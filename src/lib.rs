#![crate_name = "xpath"]
#![feature(macro_rules)]

extern crate document;

use std::collections::HashMap;

use document::{Any,ToAny};
use document::{Nodeset};

use tokenizer::{XPathTokenizer,XPathTokenDeabbreviator,XPathTokenDisambiguator};
use parser::XPathParser;

pub mod axis;
pub mod expression;
pub mod function;
pub mod node_test;
pub mod parser;
pub mod token;
pub mod tokenizer;

#[deriving(PartialEq,Show,Clone)]
pub enum XPathValue {
    Boolean(bool),
    Number(f64),
    String(String),
    Nodes(Nodeset), // rename as Nodeset
}

impl XPathValue {
    pub fn boolean(&self) -> bool {
        match *self {
            Boolean(val) => val,
            Number(n) => n != 0.0 && ! n.is_nan(),
            String(ref s) => ! s.is_empty(),
            Nodes(ref nodeset) => nodeset.size() > 0,
        }
    }

    pub fn number(&self) -> f64 {
        match *self {
            Number(val) => val,
            _ => -42.0
        }
    }

    pub fn string(&self) -> String {
        match *self {
            String(ref val) => val.clone(),
            _ => "Unimplemented".to_string(),
        }
    }

    pub fn nodeset(&self) -> Nodeset {
        match *self {
            Nodes(ref ns) => ns.clone(),
            _ => fail!("Did not evaluate to a nodeset!"),
        }
    }
}

pub trait XPathFunction {
    fn evaluate(&self,
                context: &XPathEvaluationContext,
                args: Vec<XPathValue>) -> XPathValue;
}

type BoxFunc = Box<XPathFunction + 'static>;
pub type Functions = HashMap<String, BoxFunc>;
pub type Variables = HashMap<String, XPathValue>;

pub struct XPathEvaluationContext<'a> {
    node: Any,
    functions: & 'a Functions,
    variables: & 'a Variables,
    position: uint,
}

impl<'a> XPathEvaluationContext<'a> {
    pub fn new<A: ToAny>(node: A,
                         functions: &'a Functions,
                         variables: &'a Variables) -> XPathEvaluationContext<'a>
    {
        XPathEvaluationContext {
            node: node.to_any(),
            functions: functions,
            variables: variables,
            position: 0,
        }
    }

    fn node(&self) -> &Any {
        &self.node
    }

    fn new_context_for(& self, _size: uint) -> XPathEvaluationContext<'a> {
        XPathEvaluationContext {
            node: self.node.clone(),
            functions: self.functions,
            variables: self.variables,
            position: 0,
        }
    }

    pub fn next<A: ToAny>(& mut self, node: A) {
        self.node = node.to_any();
        self.position += 1;
    }

    fn position(&self) -> uint {
        self.position
    }

    fn function_for_name(&self, name: &str) -> Option<& 'a BoxFunc> {
        self.functions.find(&name.to_string())
    }

    fn value_of(&self, name: &str) -> Option<&XPathValue> {
        self.variables.find(&name.to_string())
    }
}

pub struct XPathFactory {
    parser: XPathParser,
}

impl XPathFactory {
    pub fn new() -> XPathFactory {
        XPathFactory { parser: XPathParser::new() }
    }

    pub fn build(&self, xpath: &str) -> parser::ParseResult {
        let tokenizer = XPathTokenizer::new(xpath);
        let deabbreviator = XPathTokenDeabbreviator::new(tokenizer);
        let disambiguator = XPathTokenDisambiguator::new(deabbreviator);

        self.parser.parse(disambiguator)
    }
}

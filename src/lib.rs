#![crate_name = "xpath"]
#![feature(macro_rules)]

extern crate document;

use std::collections::HashMap;
use std::string;
use std::num::Float;

use document::dom4::{Node,ToNode};
use document::nodeset::Nodeset;

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
pub enum XPathValue<'d> {
    Boolean(bool),
    Number(f64),
    String(string::String),
    Nodes(Nodeset<'d>), // rename as Nodeset
}

impl<'d> XPathValue<'d> {
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

    pub fn string(&self) -> string::String {
        match *self {
            String(ref val) => val.clone(),
            _ => "Unimplemented".to_string(),
        }
    }

    pub fn nodeset(&self) -> Nodeset<'d> {
        match *self {
            Nodes(ref ns) => ns.clone(),
            _ => panic!("Did not evaluate to a nodeset!"),
        }
    }
}

pub trait XPathFunction {
    fn evaluate<'a, 'd>(&self,
                        context: &XPathEvaluationContext<'a, 'd>,
                        args: Vec<XPathValue<'d>>) -> XPathValue<'d>;
}

type BoxFunc = Box<XPathFunction + 'static>;
pub type Functions = HashMap<string::String, BoxFunc>;
pub type Variables<'d> = HashMap<string::String, XPathValue<'d>>;

pub struct XPathEvaluationContext<'a, 'd : 'a> {
    node: Node<'d>,
    functions: &'a Functions,
    variables: &'a Variables<'d>,
    position: uint,
}

impl<'a, 'd> XPathEvaluationContext<'a, 'd> {
    pub fn new<N: ToNode<'d>>(node: N,
                              functions: &'a Functions,
                              variables: &'a Variables<'d>) -> XPathEvaluationContext<'a, 'd>
    {
        XPathEvaluationContext {
            node: node.to_node(),
            functions: functions,
            variables: variables,
            position: 0,
        }
    }

    fn node(&self) -> &Node<'d> {
        &self.node
    }

    fn new_context_for(&self, _size: uint) -> XPathEvaluationContext<'a, 'd> {
        XPathEvaluationContext {
            node: self.node.clone(),
            functions: self.functions,
            variables: self.variables,
            position: 0,
        }
    }

    pub fn next<N: ToNode<'d>>(&mut self, node: N) {
        self.node = node.to_node();
        self.position += 1;
    }

    fn position(&self) -> uint {
        self.position
    }

    fn function_for_name(&self, name: &str) -> Option<&'a BoxFunc> {
        self.functions.get(&name.to_string())
    }

    fn value_of(&self, name: &str) -> Option<&XPathValue<'d>> {
        self.variables.get(&name.to_string())
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

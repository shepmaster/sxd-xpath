#![crate_name = "xpath"]
#![feature(macro_rules)]
#![feature(phase)]

#[phase(plugin, link)]
extern crate document;

use std::collections::HashMap;
use std::string;
use std::num::Float;

use document::dom4;
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
            String(ref s) => from_str(s.as_slice()).unwrap_or(Float::nan()),
            _ => unimplemented!(),
        }
    }

    pub fn string(&self) -> string::String {
        match *self {
            String(ref val) => val.clone(),
            _ => unimplemented!(),
        }
    }

    pub fn nodeset(&self) -> Nodeset<'d> {
        match *self {
            Nodes(ref ns) => ns.clone(),
            _ => panic!("Did not evaluate to a nodeset!"),
        }
    }
}

trait StringValue {
    fn string_value(&self) -> string::String;
}

fn text_descendants_string_value(node: &Node) -> string::String {
    fn document_order_text_nodes(node: &Node, result: &mut string::String) {
        for child in node.children().iter() {
            match child {
                &dom4::ElementNode(_) => document_order_text_nodes(child, result),
                &dom4::TextNode(n) => result.push_str(n.text()),
                _ => {},
            }
        }
    };

    let mut result = string::String::new();
    document_order_text_nodes(node, &mut result);
    result
}

impl<'d> StringValue for Node<'d> {
    fn string_value(&self) -> string::String {
        match self {
            &dom4::RootNode(_) => text_descendants_string_value(self),
            &dom4::ElementNode(_) => text_descendants_string_value(self),
            &dom4::AttributeNode(n) => string::String::from_str(n.value()),
            &dom4::ProcessingInstructionNode(n) => string::String::from_str(n.value().unwrap_or("")),
            &dom4::CommentNode(n) => string::String::from_str(n.text()),
            &dom4::TextNode(n) => string::String::from_str(n.text()),
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

    fn new_context_for(&self, _size: uint) -> XPathEvaluationContext<'a, 'd> {
        XPathEvaluationContext {
            node: self.node,
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

#[cfg(test)]
mod test {
    use document::Package;
    use document::dom4::ToNode;
    use super::StringValue;

    #[test]
    fn string_value_of_element_node_is_concatenation_of_descendant_text_nodes() {
        let package = Package::new();
        let doc = package.as_document();

        let element = doc.create_element("hello");
        let child = doc.create_element("world");
        let text1 = doc.create_text("Presenting: ");
        let text2 = doc.create_text("Earth");
        let text3 = doc.create_text("!");

        element.append_child(text1);
        element.append_child(child);
        child.append_child(text2);
        element.append_child(text3);

        assert_eq!("Presenting: Earth!", element.to_node().string_value().as_slice());
    }

    #[test]
    fn string_value_of_attribute_node_is_value() {
        let package = Package::new();
        let doc = package.as_document();
        let element = doc.create_element("hello");
        let attribute = element.set_attribute_value("world", "Earth").to_node();
        assert_eq!("Earth", attribute.string_value().as_slice());
    }

    #[test]
    fn string_value_of_pi_node_is_empty_when_no_value() {
        let package = Package::new();
        let doc = package.as_document();
        let pi = doc.create_processing_instruction("hello", None).to_node();
        assert_eq!("", pi.string_value().as_slice());
    }

    #[test]
    fn string_value_of_pi_node_is_the_value_when_value() {
        let package = Package::new();
        let doc = package.as_document();
        let pi = doc.create_processing_instruction("hello", Some("world")).to_node();
        assert_eq!("world", pi.string_value().as_slice());
    }

    #[test]
    fn string_value_of_comment_node_is_the_text() {
        let package = Package::new();
        let doc = package.as_document();
        let comment = doc.create_comment("hello world").to_node();
        assert_eq!("hello world", comment.string_value().as_slice());
    }

    #[test]
    fn string_value_of_text_node_is_the_text() {
        let package = Package::new();
        let doc = package.as_document();
        let text = doc.create_text("hello world").to_node();
        assert_eq!("hello world", text.string_value().as_slice());
    }
}

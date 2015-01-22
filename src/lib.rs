#![crate_name = "xpath"]
#![feature(box_syntax)]
#![allow(unstable)]

#[macro_use]
extern crate document;

use self::Value::*;
use self::nodeset::Nodeset;
use self::nodeset::{Node,ToNode};

use std::collections::HashMap;
use std::iter;
use std::string;
use std::num::Float;
use std::vec;

use tokenizer::{Tokenizer,TokenDeabbreviator};
use parser::Parser;
pub use function::Function;
pub use expression::Expression;

#[macro_use]
pub mod macros;
pub mod nodeset;
pub mod axis;
mod expression;
pub mod function;
mod node_test;
pub mod parser;
mod token;
pub mod tokenizer;

#[derive(PartialEq,Debug,Clone)]
enum LiteralValue {
    Boolean(bool),
    Number(f64),
    String(string::String),
}

impl LiteralValue {
    fn into_value<'d>(self) -> Value<'d> {
        match self {
            LiteralValue::Boolean(v) => Boolean(v),
            LiteralValue::Number(v)  => Number(v),
            LiteralValue::String(v)  => String(v),
        }
    }
}

#[derive(PartialEq,Debug,Clone)]
pub enum Value<'d> {
    Boolean(bool),
    Number(f64),
    String(string::String),
    Nodes(Nodeset<'d>), // rename as Nodeset
}

impl<'d> Value<'d> {
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
            String(ref s) => s.trim().parse().unwrap_or(Float::nan()),
            Boolean(val) => if val { 1.0 } else { 0.0 },
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

    #[allow(dead_code)]
    fn into_literal_value(self) -> LiteralValue {
        match self {
            Boolean(v) => LiteralValue::Boolean(v),
            Number(v)  => LiteralValue::Number(v),
            String(v)  => LiteralValue::String(v),
            Nodes(_)   => panic!("Cannot convert a nodeset to a literal"),
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
                &Node::ElementNode(_) => document_order_text_nodes(child, result),
                &Node::TextNode(n) => result.push_str(n.text()),
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
            &Node::RootNode(_) => text_descendants_string_value(self),
            &Node::ElementNode(_) => text_descendants_string_value(self),
            &Node::AttributeNode(n) => string::String::from_str(n.value()),
            &Node::ProcessingInstructionNode(n) => string::String::from_str(n.value().unwrap_or("")),
            &Node::CommentNode(n) => string::String::from_str(n.text()),
            &Node::TextNode(n) => string::String::from_str(n.text()),
        }
    }
}

type BoxFunc = Box<Function + 'static>;
pub type Functions = HashMap<string::String, BoxFunc>;
pub type Variables<'d> = HashMap<string::String, Value<'d>>;
pub type Namespaces = HashMap<string::String, string::String>;

#[derive(Copy,Clone)]
pub struct EvaluationContext<'a, 'd : 'a> {
    node: Node<'d>,
    functions: &'a Functions,
    variables: &'a Variables<'d>,
    namespaces: &'a Namespaces,
    position: usize,
    size: usize,
}

impl<'a, 'd> EvaluationContext<'a, 'd> {
    pub fn new<N>(node: N,
                  functions: &'a Functions,
                  variables: &'a Variables<'d>,
                  namespaces: &'a Namespaces)
                  -> EvaluationContext<'a, 'd>
        where N: ToNode<'d>
    {
        EvaluationContext {
            node: node.to_node(),
            functions: functions,
            variables: variables,
            namespaces: namespaces,
            position: 1,
            size: 1,
        }
    }

    fn new_context_for<N>(&self, node: N) -> EvaluationContext<'a, 'd>
        where N: ToNode<'d>
    {
        EvaluationContext {
            node: node.to_node(),
            .. *self
        }
    }

    fn position(&self) -> usize {
        self.position
    }

    fn size(&self) -> usize {
        self.size
    }

    fn function_for_name(&self, name: &str) -> Option<&'a BoxFunc> {
        self.functions.get(&name.to_string())
    }

    fn value_of(&self, name: &str) -> Option<&Value<'d>> {
        self.variables.get(&name.to_string())
    }

    fn namespace_for(&self, prefix: &str) -> Option<&str> {
        self.namespaces.get(prefix).map(|ns| ns.as_slice())
    }

    fn predicate_iter<'p>(&'p self, nodes: Nodeset<'d>)
                          -> EvaluationContextPredicateIter<'a, 'd, 'p>
    {
        let sz = nodes.size();
        EvaluationContextPredicateIter {
            parent: self,
            nodes: nodes.into_iter().enumerate(),
            size: sz,
        }
    }
}

struct EvaluationContextPredicateIter<'a : 'p, 'd : 'a + 'p, 'p> {
    parent: &'p EvaluationContext<'a, 'd>,
    nodes: iter::Enumerate<vec::IntoIter<Node<'d>>>,
    size: usize,
}

impl<'a, 'd, 'p> Iterator for EvaluationContextPredicateIter<'a, 'd, 'p> {
    type Item = EvaluationContext<'a, 'd>;

    fn next(&mut self) -> Option<EvaluationContext<'a, 'd>> {
        self.nodes.next().map(|(idx, node)| {
            EvaluationContext {
                node: node,
                position: idx + 1,
                size: self.size,
                .. *self.parent
            }
        })
    }
}

pub struct Factory {
    parser: Parser,
}

impl Factory {
    pub fn new() -> Factory {
        Factory { parser: Parser::new() }
    }

    pub fn build(&self, xpath: &str) -> parser::ParseResult {
        let tokenizer = Tokenizer::new(xpath);
        let deabbreviator = TokenDeabbreviator::new(tokenizer);

        self.parser.parse(deabbreviator)
    }
}

#[doc(hidden)]
mod xpath {
    pub use nodeset;
}

#[cfg(test)]
mod test {
    use document::Package;
    use super::nodeset::ToNode;
    use super::StringValue;
    use super::Value;
    use std::num::Float;

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

    #[test]
    fn number_of_string_is_ieee_754_number() {
        let v = Value::String("1.5".to_string());
        assert_eq!(1.5, v.number());
    }

    #[test]
    fn number_of_string_with_negative_is_negative_number() {
        let v = Value::String("-1.5".to_string());
        assert_eq!(-1.5, v.number());
    }

    #[test]
    fn number_of_string_with_surrounding_whitespace_is_number_without_whitespace() {
        let v = Value::String("\r\n1.5 \t".to_string());
        assert_eq!(1.5, v.number());
    }

    #[test]
    fn number_of_garbage_string_is_nan() {
        let v = Value::String("I am not an IEEE 754 number".to_string());
        assert!(v.number().is_nan());
    }

    #[test]
    fn number_of_boolean_true_is_1() {
        let v = Value::Boolean(true);
        assert_eq!(1.0, v.number());
    }

    #[test]
    fn number_of_boolean_false_is_0() {
        let v = Value::Boolean(false);
        assert_eq!(0.0, v.number());
    }
}

//!
//! ### Namespaces
//!
//! The XPath specification assumes that the XML being processed has a
//! prefix defined for every namespace present in the document. If you
//! are processing XML that was parsed from text, this will be true by
//! construction.
//!
//! If you have programmatically created XML with namespaces but not
//! defined prefixes, some XPath behavior may be confusing:
//!
//! 1. The `name` method will not include a prefix, even if the
//! element or attribute has a namespace.
//! 2. The `namespace` axis will not include namespaces without
//! prefixes.
//!

#[macro_use]
extern crate peresil;
extern crate sxd_document;
#[macro_use]
extern crate quick_error;

use std::borrow::ToOwned;
use std::collections::HashMap;
use std::{iter,string};

use sxd_document::dom::Document;

use self::Value::{Boolean,Number,String};

use nodeset::{Nodeset,Node};
use parser::Parser;
use tokenizer::{Tokenizer,TokenDeabbreviator};

pub use function::Function;
pub use expression::Expression;
pub use expression::Error as ExpressionError;

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

// TODO - this doesn't need to be public, figure out how to make it so
#[derive(PartialEq,Debug,Clone)]
pub enum LiteralValue {
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

/// The primary types of values that an XPath expression accepts
/// as an argument or returns as a result.
#[derive(Debug, Clone, PartialEq)]
pub enum Value<'d> {
    /// A true or false value
    Boolean(bool),
    /// A IEEE-754 double-precision floating point number
    Number(f64),
    /// A string
    String(string::String),
    /// A collection of unique nodes
    Nodeset(Nodeset<'d>),
}

fn str_to_num(s: &str) -> f64 {
    s.trim().parse().unwrap_or(::std::f64::NAN)
}

impl<'d> Value<'d> {
    pub fn boolean(&self) -> bool {
        use Value::*;
        match *self {
            Boolean(val) => val,
            Number(n) => n != 0.0 && ! n.is_nan(),
            String(ref s) => ! s.is_empty(),
            Nodeset(ref nodeset) => nodeset.size() > 0,
        }
    }

    pub fn number(&self) -> f64 {
        use Value::*;
        match *self {
            Boolean(val) => if val { 1.0 } else { 0.0 },
            Number(val) => val,
            String(ref s) => str_to_num(s),
            Nodeset(..) => str_to_num(&self.string()),
        }
    }

    pub fn string(&self) -> string::String {
        use Value::*;
        match *self {
            Boolean(v) => v.to_string(),
            Number(n) => {
                if n.is_infinite() {
                    if n.signum() < 0.0 {
                        "-Infinity".to_owned()
                    } else {
                        "Infinity".to_owned()
                    }
                } else {
                    n.to_string()
                }
            },
            String(ref val) => val.clone(),
            Nodeset(ref ns) => match ns.document_order_first() {
                Some(n) => n.string_value(),
                None => "".to_owned(),
            },
        }
    }

    #[allow(dead_code)]
    fn into_literal_value(self) -> LiteralValue {
        use Value::*;
        match self {
            Boolean(v) => LiteralValue::Boolean(v),
            Number(v)  => LiteralValue::Number(v),
            String(v)  => LiteralValue::String(v),
            Nodeset(_) => panic!("Cannot convert a nodeset to a literal"),
        }
    }
}

pub type BoxFunc = Box<Function + 'static>;
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
        where N: Into<Node<'d>>
    {
        EvaluationContext {
            node: node.into(),
            functions: functions,
            variables: variables,
            namespaces: namespaces,
            position: 1,
            size: 1,
        }
    }

    fn new_context_for<N>(&self, node: N) -> EvaluationContext<'a, 'd>
        where N: Into<Node<'d>>
    {
        EvaluationContext {
            node: node.into(),
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
        self.functions.get(&name.to_owned())
    }

    fn value_of(&self, name: &str) -> Option<&Value<'d>> {
        self.variables.get(&name.to_owned())
    }

    fn namespace_for(&self, prefix: &str) -> Option<&str> {
        self.namespaces.get(prefix).map(|ns| &ns[..])
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
    nodes: iter::Enumerate<nodeset::IntoIter<'d>>,
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

/// The primary entrypoint to convert an XPath represented as a string
/// to a structure that can be evaluated.
pub struct Factory {
    parser: Parser,
}

impl Factory {
    pub fn new() -> Factory {
        Factory { parser: Parser::new() }
    }

    /// Compiles the given string into an XPath structure.
    pub fn build(&self, xpath: &str) -> parser::ParseResult {
        let tokenizer = Tokenizer::new(xpath);
        let deabbreviator = TokenDeabbreviator::new(tokenizer);

        self.parser.parse(deabbreviator)
    }
}

quick_error! {
    /// The failure modes of executing an XPath.
    #[derive(Debug, Clone, PartialEq)]
    pub enum Error {
        /// The XPath was syntactically invalid
        Parsing(err: parser::Error) {
            from()
            cause(err)
            description("Unable to parse XPath")
            display("Unable to parse XPath: {}", err)
        }
        /// The XPath did not construct an expression
        NoXPath {
            description("XPath was empty")
        }
        /// The XPath could not be executed
        Executing(err: expression::Error) {
            from()
            cause(err)
            description("Unable to execute XPath")
            display("Unable to execute XPath: {}", err)
        }
    }
}

/// Easily evaluate an XPath expression
///
/// The core XPath 1.0 functions will be available, and no variables
/// or namespaces will be defined. The root of the document is the
/// context node.
///
/// If you will be evaluating multiple XPaths or the same XPath
/// multiple times, this may not be the most performant solution.
///
/// # Examples
///
/// ```
/// extern crate sxd_document;
/// extern crate sxd_xpath;
///
/// use sxd_document::parser;
/// use sxd_xpath::{evaluate_xpath, Value};
///
/// fn main() {
///     let package = parser::parse("<root><a>1</a><b>2</b></root>").expect("failed to parse the XML");
///     let document = package.as_document();
///
///     assert_eq!(Ok(Value::Number(3.0)), evaluate_xpath(&document, "/*/a + /*/b"));
/// }
/// ```
pub fn evaluate_xpath<'d>(document: &'d Document<'d>, xpath: &str) -> Result<Value<'d>, Error> {
    let factory = Factory::new();
    let expression = factory.build(xpath)?;
    let expression = expression.ok_or(Error::NoXPath)?;

    let mut functions = HashMap::new();
    function::register_core_functions(&mut functions);
    let variables = HashMap::new();
    let namespaces = HashMap::new();

    let context = EvaluationContext::new(
        document.root(),
        &functions,
        &variables,
        &namespaces,
    );

    expression.evaluate(&context).map_err(Into::into)
}

#[cfg(test)]
mod test {
    use std::borrow::ToOwned;

    use sxd_document::{self, dom, Package};

    use super::*;

    #[test]
    fn number_of_string_is_ieee_754_number() {
        let v = Value::String("1.5".to_owned());
        assert_eq!(1.5, v.number());
    }

    #[test]
    fn number_of_string_with_negative_is_negative_number() {
        let v = Value::String("-1.5".to_owned());
        assert_eq!(-1.5, v.number());
    }

    #[test]
    fn number_of_string_with_surrounding_whitespace_is_number_without_whitespace() {
        let v = Value::String("\r\n1.5 \t".to_owned());
        assert_eq!(1.5, v.number());
    }

    #[test]
    fn number_of_garbage_string_is_nan() {
        let v = Value::String("I am not an IEEE 754 number".to_owned());
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

    #[test]
    fn number_of_nodeset_is_number_value_of_first_node_in_document_order() {
        let package = Package::new();
        let doc = package.as_document();

        let c1 = doc.create_comment("42.42");
        let c2 = doc.create_comment("1234");
        doc.root().append_child(c1);
        doc.root().append_child(c2);

        let v = Value::Nodeset(nodeset![c2, c1]);
        assert_eq!(42.42, v.number());
    }

    #[test]
    fn string_of_true_is_true() {
        let v = Value::Boolean(true);
        assert_eq!("true", v.string());
    }

    #[test]
    fn string_of_false_is_false() {
        let v = Value::Boolean(false);
        assert_eq!("false", v.string());
    }

    #[test]
    fn string_of_nan_is_nan() {
        let v = Value::Number(::std::f64::NAN);
        assert_eq!("NaN", v.string());
    }

    #[test]
    fn string_of_positive_zero_is_zero() {
        let v = Value::Number(0.0);
        assert_eq!("0", v.string());
    }

    #[test]
    fn string_of_negative_zero_is_zero() {
        let v = Value::Number(-0.0);
        assert_eq!("0", v.string());
    }

    #[test]
    fn string_of_positive_infinity_is_infinity() {
        let v = Value::Number(::std::f64::INFINITY);
        assert_eq!("Infinity", v.string());
    }

    #[test]
    fn string_of_negative_infinity_is_minus_infinity() {
        let v = Value::Number(::std::f64::NEG_INFINITY);
        assert_eq!("-Infinity", v.string());
    }

    #[test]
    fn string_of_integer_has_no_decimal() {
        let v = Value::Number(-42.0);
        assert_eq!("-42", v.string());
    }

    #[test]
    fn string_of_decimal_has_fractional_part() {
        let v = Value::Number(1.2);
        assert_eq!("1.2", v.string());
    }

    #[test]
    fn string_of_nodeset_is_string_value_of_first_node_in_document_order() {
        let package = Package::new();
        let doc = package.as_document();

        let c1 = doc.create_comment("comment 1");
        let c2 = doc.create_comment("comment 2");
        doc.root().append_child(c1);
        doc.root().append_child(c2);

        let v = Value::Nodeset(nodeset![c2, c1]);
        assert_eq!("comment 1", v.string());
    }

    fn with_document<F>(xml: &str, f: F)
        where F: FnOnce(dom::Document),
    {
        let package = sxd_document::parser::parse(xml).expect("Unable to parse test XML");
        f(package.as_document());
    }

    #[test]
    fn xpath_evaluation_success() {
        with_document("<root><child>content</child></root>", |doc| {
            let result = evaluate_xpath(&doc, "/root/child");

            assert_eq!(Ok("content".to_owned()), result.map(|v| v.string()));
        });
    }

    #[test]
    fn xpath_evaluation_parsing_error() {
        with_document("<root><child>content</child></root>", |doc| {
            use Error::*;
            use parser::Error::*;

            let result = evaluate_xpath(&doc, "/root/child/");

            assert_eq!(Err(Parsing(TrailingSlash)), result);
        });
    }

    #[test]
    fn xpath_evaluation_execution_error() {
        with_document("<root><child>content</child></root>", |doc| {
            use Error::*;
            use expression::Error::*;

            let result = evaluate_xpath(&doc, "$foo");

            assert_eq!(Err(Executing(UnknownVariable("foo".to_owned()))), result);
        });
    }

    #[test]
    fn xpath_evaluation_no_xpath_error() {
        with_document("<root><child>content</child></root>", |doc| {
            let result = evaluate_xpath(&doc, "");

            assert_eq!(Err(Error::NoXPath), result);
        });
    }
}

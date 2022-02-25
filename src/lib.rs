//! # SXD-XPath
//!
//! This is a pure-Rust implementation of XPath, a language for
//! addressing parts of an XML document. It aims to implement [version
//! 1.0 of the XPath specification][spec].
//!
//! XPath is wonderful for quickly navigating the complicated
//! hierarchy that is present in many XML documents while having a
//! concise syntax.
//!
//! [spec]: https://www.w3.org/TR/xpath/
//!
//! ### Examples
//!
//! The quickest way to evaluate an XPath against an XML document is
//! to use [`evaluate_xpath`][evaluate_xpath].
//!
//! ```
//! use sxd_document::parser;
//! use sxd_xpath::{evaluate_xpath, Value};
//!
//! fn main() {
//!     let package = parser::parse("<root>hello</root>").expect("failed to parse XML");
//!     let document = package.as_document();
//!
//!     let value = evaluate_xpath(&document, "/root").expect("XPath evaluation failed");
//!
//!     assert_eq!("hello", value.string());
//! }
//! ```
//!
//! Evaluating an XPath returns a [`Value`][], representing the
//! primary XPath types.
//!
//! For more complex needs, XPath parsing and evaluation can be split
//! apart. This allows the user to specify namespaces, variables,
//! extra functions, and which node evaluation should begin with. You
//! may also compile an XPath once and reuse it multiple times.
//!
//! Parsing is handled with the [`Factory`][] and evaluation relies on
//! the [`Context`][]. Similar functionality to above can be
//! accomplished:
//!
//! ```
//! use sxd_document::parser;
//! use sxd_xpath::{Factory, Context, Value};
//!
//! fn main() {
//!     let package = parser::parse("<root>hello</root>")
//!         .expect("failed to parse XML");
//!     let document = package.as_document();
//!
//!     let factory = Factory::new();
//!     let xpath = factory.build("/root").expect("Could not compile XPath");
//!
//!     let context = Context::new();
//!
//!     let value = xpath.evaluate(&context, document.root())
//!         .expect("XPath evaluation failed");
//!
//!     assert_eq!("hello", value.string());
//! }
//! ```
//!
//! See [`Context`][] for details on how to customize the
//! evaluation of the XPath.
//!
//! [evaluate_xpath]: fn.evaluate_xpath.html
//! [`Value`]: enum.Value.html
//! [`Factory`]: struct.Factory.html
//! [`Context`]: context/struct.Context.html
//!
//! ### Programmatically-created XML
//!
//! The XPath specification assumes certain properties about the XML
//! being processed. If you are processing XML that was parsed from
//! text, this will be true by construction. If you have
//! programmatically created XML, please note the following cases.
//!
//! #### Namespaces
//!
//! If you have programmatically created XML with namespaces but not
//! defined prefixes, some XPath behavior may be confusing:
//!
//! 1. The `name` method will not include a prefix, even if the
//! element or attribute has a namespace.
//! 2. The `namespace` axis will not include namespaces without
//! prefixes.
//!
//! #### Document order
//!
//! If you have programmatically created XML but not attached the
//! nodes to the document, some XPath behavior may be confusing:
//!
//! 1. These nodes have no [*document order*]. If you create a
//! variable containing these nodes and apply a predicate to them,
//! these nodes will appear after any nodes that are present in the
//! document, but the relative order of the nodes is undefined.
//!
//! [*document order*]: https://www.w3.org/TR/xpath/#dt-document-order

use snafu::{ResultExt, Snafu};
use std::borrow::ToOwned;
use std::string;
use sxd_document::dom::Document;
use sxd_document::{PrefixedName, QName};

use crate::parser::Parser;
use crate::tokenizer::{TokenDeabbreviator, Tokenizer};

pub use crate::context::Context;

#[macro_use]
pub mod macros;
mod axis;
pub mod context;
mod expression;
pub mod function;
mod node_test;
pub mod nodeset;
mod parser;
mod token;
mod tokenizer;

// These belong in the the document

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OwnedPrefixedName {
    prefix: Option<String>,
    local_part: String,
}

impl<'a> From<&'a str> for OwnedPrefixedName {
    fn from(local_part: &'a str) -> Self {
        OwnedPrefixedName {
            prefix: None,
            local_part: local_part.into(),
        }
    }
}

impl<'a> From<(&'a str, &'a str)> for OwnedPrefixedName {
    fn from((prefix, local_part): (&'a str, &'a str)) -> Self {
        OwnedPrefixedName {
            prefix: Some(prefix.into()),
            local_part: local_part.into(),
        }
    }
}

impl<'a> From<PrefixedName<'a>> for OwnedPrefixedName {
    fn from(name: PrefixedName<'a>) -> Self {
        OwnedPrefixedName {
            prefix: name.prefix().map(Into::into),
            local_part: name.local_part().into(),
        }
    }
}

impl<'a> From<&'a OwnedPrefixedName> for OwnedPrefixedName {
    fn from(name: &'a OwnedPrefixedName) -> Self {
        OwnedPrefixedName {
            prefix: name.prefix.to_owned(),
            local_part: name.local_part.to_owned(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OwnedQName {
    namespace_uri: Option<String>,
    local_part: String,
}

impl<'a> From<&'a str> for OwnedQName {
    fn from(local_part: &'a str) -> Self {
        OwnedQName {
            namespace_uri: None,
            local_part: local_part.into(),
        }
    }
}

impl<'a> From<(&'a str, &'a str)> for OwnedQName {
    fn from((namespace_uri, local_part): (&'a str, &'a str)) -> Self {
        OwnedQName {
            namespace_uri: Some(namespace_uri.into()),
            local_part: local_part.into(),
        }
    }
}

impl<'a> From<QName<'a>> for OwnedQName {
    fn from(name: QName<'a>) -> Self {
        OwnedQName {
            namespace_uri: name.namespace_uri().map(Into::into),
            local_part: name.local_part().into(),
        }
    }
}

type LiteralValue = Value<'static>;

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
    Nodeset(nodeset::Nodeset<'d>),
}

fn str_to_num(s: &str) -> f64 {
    s.trim().parse().unwrap_or(::std::f64::NAN)
}

impl<'d> Value<'d> {
    pub fn boolean(&self) -> bool {
        use crate::Value::*;
        match *self {
            Boolean(val) => val,
            Number(n) => n != 0.0 && !n.is_nan(),
            String(ref s) => !s.is_empty(),
            Nodeset(ref nodeset) => nodeset.size() > 0,
        }
    }

    pub fn into_boolean(self) -> bool {
        self.boolean()
    }

    pub fn number(&self) -> f64 {
        use crate::Value::*;
        match *self {
            Boolean(val) => {
                if val {
                    1.0
                } else {
                    0.0
                }
            }
            Number(val) => val,
            String(ref s) => str_to_num(s),
            Nodeset(..) => str_to_num(&self.string()),
        }
    }

    pub fn into_number(self) -> f64 {
        self.number()
    }

    pub fn string(&self) -> string::String {
        use crate::Value::*;
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
                    // fix the behavior of negative zero
                    // see https://en.wikipedia.org/wiki/Signed_zero
                    if n == 0.0 {
                        0.0.to_string()
                    } else {
                        n.to_string()
                    }
                }
            }
            String(ref val) => val.clone(),
            Nodeset(ref ns) => match ns.document_order_first() {
                Some(n) => n.string_value(),
                None => "".to_owned(),
            },
        }
    }

    pub fn into_string(self) -> string::String {
        use crate::Value::*;
        match self {
            String(val) => val,
            other => other.string(),
        }
    }
}

macro_rules! from_impl {
    ($raw:ty, $variant:expr) => {
        impl<'d> From<$raw> for Value<'d> {
            fn from(other: $raw) -> Value<'d> {
                $variant(other)
            }
        }
    };
}

from_impl!(bool, Value::Boolean);
from_impl!(f64, Value::Number);
from_impl!(String, Value::String);
impl<'a, 'd> From<&'a str> for Value<'d> {
    fn from(other: &'a str) -> Value<'d> {
        Value::String(other.into())
    }
}
from_impl!(nodeset::Nodeset<'d>, Value::Nodeset);

macro_rules! partial_eq_impl {
    ($raw:ty, $variant:pat => $b:expr) => {
        impl<'d> PartialEq<$raw> for Value<'d> {
            fn eq(&self, other: &$raw) -> bool {
                match *self {
                    $variant => $b == other,
                    _ => false,
                }
            }
        }

        impl<'d> PartialEq<Value<'d>> for $raw {
            fn eq(&self, other: &Value<'d>) -> bool {
                match *other {
                    $variant => $b == self,
                    _ => false,
                }
            }
        }
    };
}

partial_eq_impl!(bool, Value::Boolean(ref v) => v);
partial_eq_impl!(f64, Value::Number(ref v) => v);
partial_eq_impl!(String, Value::String(ref v) => v);
partial_eq_impl!(&'d str, Value::String(ref v) => v);
partial_eq_impl!(nodeset::Nodeset<'d>, Value::Nodeset(ref v) => v);

/// A compiled XPath. Construct via [`Factory`][].
///
/// [`Factory`]: struct.Factory.html
#[derive(Debug)]
pub struct XPath(Box<dyn expression::Expression + 'static>);

impl XPath {
    /// Evaluate this expression in the given context.
    ///
    /// # Examples
    ///
    /// The most common case is to pass in a reference to a [`Context`][]:
    ///
    /// ```rust,no-run
    /// use sxd_document::dom::Document;
    /// use sxd_xpath::{XPath, Context};
    ///
    /// fn my_evaluate(doc: Document, xpath: XPath) {
    ///     let mut context = Context::new();
    ///     let value = xpath.evaluate(&context, doc.root());
    ///     println!("The result was: {:?}", value);
    /// }
    ///
    /// # fn main() {}
    /// ```
    ///
    /// [`Context`]: context/struct.Context.html
    pub fn evaluate<'d, N>(
        &self,
        context: &Context<'d>,
        node: N,
    ) -> Result<Value<'d>, ExecutionError>
    where
        N: Into<nodeset::Node<'d>>,
    {
        let context = context::Evaluation::new(context, node.into());
        self.0.evaluate(&context).map_err(ExecutionError)
    }
}

/// The primary entrypoint to convert an XPath represented as a string
/// to a structure that can be evaluated.
pub struct Factory {
    parser: Parser,
}

impl Factory {
    pub fn new() -> Factory {
        Factory {
            parser: Parser::new(),
        }
    }

    /// Compiles the given string into an XPath structure.
    pub fn build(&self, xpath: &str) -> Result<XPath, ParserError> {
        let tokenizer = Tokenizer::new(xpath);
        let deabbreviator = TokenDeabbreviator::new(tokenizer);

        self.parser
            .parse(deabbreviator)
            .map(XPath)
            .map_err(Into::into)
    }
}

impl Default for Factory {
    fn default() -> Self {
        Factory::new()
    }
}

/// Errors that may occur when parsing an XPath
#[derive(Debug, Snafu, Clone, PartialEq)]
pub struct ParserError(parser::Error);

/// Errors that may occur when executing an XPath
#[derive(Debug, Snafu, Clone, PartialEq)]
pub struct ExecutionError(expression::Error);

/// The failure modes of executing an XPath.
#[derive(Debug, Snafu, Clone, PartialEq)]
pub enum Error {
    /// The XPath was syntactically invalid
    #[snafu(display("Unable to parse XPath: {}", source))]
    Parsing { source: ParserError },
    /// The XPath could not be executed
    #[snafu(display("Unable to execute XPath: {}", source))]
    Executing { source: ExecutionError },
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
    let expression = factory.build(xpath).context(Parsing)?;

    let context = Context::new();

    expression
        .evaluate(&context, document.root())
        .context(Executing)
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
    where
        F: FnOnce(dom::Document<'_>),
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
            let result = evaluate_xpath(&doc, "/root/child/");

            let expected_error = crate::parser::TrailingSlash
                .fail()
                .map_err(ParserError::from)
                .context(Parsing);
            assert_eq!(expected_error, result);
        });
    }

    #[test]
    fn xpath_evaluation_execution_error() {
        with_document("<root><child>content</child></root>", |doc| {
            let result = evaluate_xpath(&doc, "$foo");

            let expected_error = crate::expression::UnknownVariable { name: "foo" }
                .fail()
                .map_err(ExecutionError::from)
                .context(Executing);
            assert_eq!(expected_error, result);
        });
    }
}

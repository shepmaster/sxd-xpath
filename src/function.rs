//! Support for registering and creating XPath functions.

use std::borrow::ToOwned;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter;
use std::ops::Index;

use sxd_document::XmlChar;

use crate::context;
use crate::nodeset::Nodeset;
use crate::{str_to_num, Value};

/// Types that can be used as XPath functions.
pub trait Function {
    /// Evaluate this function in a specific context with a specific
    /// set of arguments.
    fn evaluate<'c, 'd>(
        &self,
        context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error>;
}

/// Represents the kind of an XPath value without carrying a value.
#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum ArgumentType {
    Boolean,
    Number,
    String,
    Nodeset,
}

impl<'a> From<&'a Value<'a>> for ArgumentType {
    fn from(other: &'a Value<'a>) -> ArgumentType {
        match *other {
            Value::Boolean(..) => ArgumentType::Boolean,
            Value::Number(..) => ArgumentType::Number,
            Value::String(..) => ArgumentType::String,
            Value::Nodeset(..) => ArgumentType::Nodeset,
        }
    }
}

quick_error! {
    /// The errors that may occur while evaluating a function
    #[derive(Debug, Clone, PartialEq, Hash)]
    pub enum Error {
        TooManyArguments { expected: usize, actual: usize } {
            description("too many arguments")
            display("too many arguments, expected {} but had {}", expected, actual)
        }
        NotEnoughArguments { expected: usize, actual: usize } {
            description("not enough arguments")
            display("not enough arguments, expected {} but had {}", expected, actual)
        }
        ArgumentMissing {
            description("attempted to use an argument that was not present")
        }
        ArgumentNotANodeset { actual: ArgumentType } {
            description("argument was not a nodeset")
            display("argument was expected to be a nodeset but was a {:?}", actual)
        }
        Other(what: String) {
            description("an error occurred while evaluating a function")
            display("could not evaluate function: {}", what)
        }
    }
}

impl Error {
    fn not_a_nodeset(actual: &Value<'_>) -> Error {
        Error::ArgumentNotANodeset {
            actual: actual.into(),
        }
    }
}

/// Provides common utility functions for dealing with function
/// argument lists.
pub struct Args<'d>(pub Vec<Value<'d>>);

impl<'d> Args<'d> {
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Ensures that there are at least the requested number of arguments.
    pub fn at_least(&self, minimum: usize) -> Result<(), Error> {
        let actual = self.0.len();
        if actual < minimum {
            Err(Error::NotEnoughArguments {
                expected: minimum,
                actual: actual,
            })
        } else {
            Ok(())
        }
    }

    /// Ensures that there are no more than the requested number of arguments.
    pub fn at_most(&self, maximum: usize) -> Result<(), Error> {
        let actual = self.0.len();
        if actual > maximum {
            Err(Error::TooManyArguments {
                expected: maximum,
                actual: actual,
            })
        } else {
            Ok(())
        }
    }

    /// Ensures that there are exactly the requested number of arguments.
    pub fn exactly(&self, expected: usize) -> Result<(), Error> {
        let actual = self.0.len();
        if actual < expected {
            Err(Error::NotEnoughArguments {
                expected: expected,
                actual: actual,
            })
        } else if actual > expected {
            Err(Error::TooManyArguments {
                expected: expected,
                actual: actual,
            })
        } else {
            Ok(())
        }
    }

    /// Converts all the arguments into strings.
    fn into_strings(self) -> Vec<String> {
        self.0.into_iter().map(Value::into_string).collect()
    }

    /// Removes the **last** argument and ensures it is a boolean. If
    /// the argument is not a boolean, it is converted to one.
    pub fn pop_boolean(&mut self) -> Result<bool, Error> {
        let v = self.0.pop().ok_or(Error::ArgumentMissing)?;
        Ok(v.into_boolean())
    }

    /// Removes the **last** argument and ensures it is a number. If
    /// the argument is not a number, it is converted to one.
    pub fn pop_number(&mut self) -> Result<f64, Error> {
        let v = self.0.pop().ok_or(Error::ArgumentMissing)?;
        Ok(v.into_number())
    }

    /// Removes the **last** argument and ensures it is a string. If
    /// the argument is not a string, it is converted to one.
    pub fn pop_string(&mut self) -> Result<String, Error> {
        let v = self.0.pop().ok_or(Error::ArgumentMissing)?;
        Ok(v.into_string())
    }

    /// Removes the **last** argument and ensures it is a nodeset. If
    /// the argument is not a nodeset, a type mismatch error is
    /// returned.
    pub fn pop_nodeset(&mut self) -> Result<Nodeset<'d>, Error> {
        let v = self.0.pop().ok_or(Error::ArgumentMissing)?;
        match v {
            Value::Nodeset(v) => Ok(v),
            a => Err(Error::not_a_nodeset(&a)),
        }
    }

    /// Removes the **last** argument. If no argument is present, the
    /// context node is returned as a nodeset.
    fn pop_value_or_context_node<'c>(
        &mut self,
        context: &context::Evaluation<'c, 'd>,
    ) -> Value<'d> {
        self.0
            .pop()
            .unwrap_or_else(|| Value::Nodeset(nodeset![context.node]))
    }

    /// Removes the **last** argument if it is a string. If no
    /// argument is present, the context node is converted to a string
    /// and returned. If there is an argument but it is not a string,
    /// it is converted to one.
    fn pop_string_value_or_context_node(
        &mut self,
        context: &context::Evaluation<'_, '_>,
    ) -> String {
        self.0
            .pop()
            .map(Value::into_string)
            .unwrap_or_else(|| context.node.string_value())
    }

    /// Removes the **last** argument if it is a nodeset. If no
    /// argument is present, the context node is added to a nodeset
    /// and returned. If there is an argument but it is not a nodeset,
    /// a type mismatch error is returned.
    fn pop_nodeset_or_context_node<'c>(
        &mut self,
        context: &context::Evaluation<'c, 'd>,
    ) -> Result<Nodeset<'d>, Error> {
        match self.0.pop() {
            Some(Value::Nodeset(ns)) => Ok(ns),
            Some(arg) => Err(Error::not_a_nodeset(&arg)),
            None => Ok(nodeset![context.node]),
        }
    }
}

impl<'d> Index<usize> for Args<'d> {
    type Output = Value<'d>;

    fn index(&self, index: usize) -> &Value<'d> {
        self.0.index(index)
    }
}

struct Last;

impl Function for Last {
    fn evaluate<'c, 'd>(
        &self,
        context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let args = Args(args);
        args.exactly(0)?;
        Ok(Value::Number(context.size as f64))
    }
}

struct Position;

impl Function for Position {
    fn evaluate<'c, 'd>(
        &self,
        context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let args = Args(args);
        args.exactly(0)?;
        Ok(Value::Number(context.position as f64))
    }
}

struct Count;

impl Function for Count {
    fn evaluate<'c, 'd>(
        &self,
        _context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.exactly(1)?;
        let arg = args.pop_nodeset()?;
        Ok(Value::Number(arg.size() as f64))
    }
}

struct LocalName;

impl Function for LocalName {
    fn evaluate<'c, 'd>(
        &self,
        context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_nodeset_or_context_node(context)?;
        let name = arg
            .document_order_first()
            .and_then(|n| n.expanded_name())
            .map(|q| q.local_part())
            .unwrap_or("");
        Ok(Value::String(name.to_owned()))
    }
}

struct NamespaceUri;

impl Function for NamespaceUri {
    fn evaluate<'c, 'd>(
        &self,
        context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_nodeset_or_context_node(context)?;
        let name = arg
            .document_order_first()
            .and_then(|n| n.expanded_name())
            .and_then(|q| q.namespace_uri())
            .unwrap_or("");
        Ok(Value::String(name.to_owned()))
    }
}

struct Name;

impl Function for Name {
    fn evaluate<'c, 'd>(
        &self,
        context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_nodeset_or_context_node(context)?;
        let name = arg
            .document_order_first()
            .and_then(|n| n.prefixed_name())
            .unwrap_or_else(String::new);
        Ok(Value::String(name))
    }
}

struct StringFn;

impl Function for StringFn {
    fn evaluate<'c, 'd>(
        &self,
        context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_value_or_context_node(context);
        Ok(Value::String(arg.string()))
    }
}

struct Concat;

impl Function for Concat {
    fn evaluate<'c, 'd>(
        &self,
        _context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let args = Args(args);
        args.at_least(2)?;
        let args = args.into_strings();
        Ok(Value::String(args.concat()))
    }
}

struct TwoStringPredicate(fn(&str, &str) -> bool);

impl Function for TwoStringPredicate {
    fn evaluate<'c, 'd>(
        &self,
        _context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let args = Args(args);
        args.exactly(2)?;
        let args = args.into_strings();
        let v = self.0(&args[0], &args[1]);
        Ok(Value::Boolean(v))
    }
}

fn starts_with() -> TwoStringPredicate {
    fn imp(a: &str, b: &str) -> bool {
        str::starts_with(a, b)
    };
    TwoStringPredicate(imp)
}
fn contains() -> TwoStringPredicate {
    fn imp(a: &str, b: &str) -> bool {
        str::contains(a, b)
    };
    TwoStringPredicate(imp)
}

struct SubstringCommon(for<'s> fn(&'s str, &'s str) -> &'s str);

impl Function for SubstringCommon {
    fn evaluate<'c, 'd>(
        &self,
        _context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let args = Args(args);
        args.exactly(2)?;
        let args = args.into_strings();
        let s = self.0(&args[0], &args[1]);
        Ok(Value::String(s.to_owned()))
    }
}

fn substring_before() -> SubstringCommon {
    fn inner<'a>(haystack: &'a str, needle: &'a str) -> &'a str {
        match haystack.find(needle) {
            Some(pos) => &haystack[..pos],
            None => "",
        }
    }
    SubstringCommon(inner)
}

fn substring_after() -> SubstringCommon {
    fn inner<'a>(haystack: &'a str, needle: &'a str) -> &'a str {
        match haystack.find(needle) {
            Some(pos) => &haystack[pos + needle.len()..],
            None => "",
        }
    }
    SubstringCommon(inner)
}

struct Substring;

impl Function for Substring {
    fn evaluate<'c, 'd>(
        &self,
        _context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.at_least(2)?;
        args.at_most(3)?;

        let len = if args.len() == 3 {
            let len = args.pop_number()?;
            round_ties_to_positive_infinity(len)
        } else {
            ::std::f64::INFINITY
        };

        let start = args.pop_number()?;
        let start = round_ties_to_positive_infinity(start);
        let s = args.pop_string()?;

        let chars = s.chars().enumerate();
        let selected_chars = chars
            .filter_map(|(p, s)| {
                let p = (p + 1) as f64; // 1-based indexing
                if p >= start && p < start + len {
                    Some(s)
                } else {
                    None
                }
            })
            .collect();

        Ok(Value::String(selected_chars))
    }
}

struct StringLength;

impl Function for StringLength {
    fn evaluate<'c, 'd>(
        &self,
        context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_string_value_or_context_node(context);
        Ok(Value::Number(arg.chars().count() as f64))
    }
}

struct NormalizeSpace;

impl Function for NormalizeSpace {
    fn evaluate<'c, 'd>(
        &self,
        context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_string_value_or_context_node(context);
        // TODO: research itertools or another pure-iterator solution
        let s: Vec<_> = arg
            .split(XmlChar::is_space_char)
            .filter(|s| !s.is_empty())
            .collect();
        let s = s.join(" ");
        Ok(Value::String(s))
    }
}

struct Translate;

impl Function for Translate {
    fn evaluate<'c, 'd>(
        &self,
        _context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.exactly(3)?;

        let to = args.pop_string()?;
        let from = args.pop_string()?;
        let s = args.pop_string()?;

        let mut replacements = HashMap::new();
        let pairs = from
            .chars()
            .zip(to.chars().map(Some).chain(iter::repeat(None)));
        for (from, to) in pairs {
            if let Entry::Vacant(entry) = replacements.entry(from) {
                entry.insert(to);
            }
        }

        let s = s
            .chars()
            .filter_map(|c| replacements.get(&c).cloned().unwrap_or_else(|| Some(c)))
            .collect();

        Ok(Value::String(s))
    }
}

struct BooleanFn;

impl Function for BooleanFn {
    fn evaluate<'c, 'd>(
        &self,
        _context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let args = Args(args);
        args.exactly(1)?;
        Ok(Value::Boolean(args[0].boolean()))
    }
}

struct Not;

impl Function for Not {
    fn evaluate<'c, 'd>(
        &self,
        _context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.exactly(1)?;
        let arg = args.pop_boolean()?;
        Ok(Value::Boolean(!arg))
    }
}

struct BooleanLiteral(bool);

impl Function for BooleanLiteral {
    fn evaluate<'c, 'd>(
        &self,
        _context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let args = Args(args);
        args.exactly(0)?;
        Ok(Value::Boolean(self.0))
    }
}

fn true_fn() -> BooleanLiteral {
    BooleanLiteral(true)
}
fn false_fn() -> BooleanLiteral {
    BooleanLiteral(false)
}

struct NumberFn;

impl Function for NumberFn {
    fn evaluate<'c, 'd>(
        &self,
        context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_value_or_context_node(context);
        Ok(Value::Number(arg.number()))
    }
}

struct Sum;

impl Function for Sum {
    fn evaluate<'c, 'd>(
        &self,
        _context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.exactly(1)?;
        let arg = args.pop_nodeset()?;
        let r = arg
            .iter()
            .map(|n| str_to_num(&n.string_value()))
            .fold(0.0, |acc, i| acc + i);
        Ok(Value::Number(r))
    }
}

struct NumberConvert(fn(f64) -> f64);

impl Function for NumberConvert {
    fn evaluate<'c, 'd>(
        &self,
        _context: &context::Evaluation<'c, 'd>,
        args: Vec<Value<'d>>,
    ) -> Result<Value<'d>, Error> {
        let mut args = Args(args);
        args.exactly(1)?;
        let arg = args.pop_number()?;
        Ok(Value::Number(self.0(arg)))
    }
}

fn floor() -> NumberConvert {
    NumberConvert(f64::floor)
}
fn ceiling() -> NumberConvert {
    NumberConvert(f64::ceil)
}

// http://stackoverflow.com/a/28124775/155423
fn round_ties_to_positive_infinity(x: f64) -> f64 {
    let y = x.floor();
    if x == y {
        x
    } else {
        let z = (2.0 * x - y).floor();
        // Should use copysign
        if x.is_sign_positive() ^ z.is_sign_positive() {
            -z
        } else {
            z
        }
    }
}

fn round() -> NumberConvert {
    NumberConvert(round_ties_to_positive_infinity)
}

/// Adds the [XPath 1.0 core function library][corelib].
///
/// [corelib]: https://www.w3.org/TR/xpath/#corelib
pub fn register_core_functions(context: &mut context::Context<'_>) {
    context.set_function("last", Last);
    context.set_function("position", Position);
    context.set_function("count", Count);
    context.set_function("local-name", LocalName);
    context.set_function("namespace-uri", NamespaceUri);
    context.set_function("name", Name);
    context.set_function("string", StringFn);
    context.set_function("concat", Concat);
    context.set_function("starts-with", starts_with());
    context.set_function("contains", contains());
    context.set_function("substring-before", substring_before());
    context.set_function("substring-after", substring_after());
    context.set_function("substring", Substring);
    context.set_function("string-length", StringLength);
    context.set_function("normalize-space", NormalizeSpace);
    context.set_function("translate", Translate);
    context.set_function("boolean", BooleanFn);
    context.set_function("not", Not);
    context.set_function("true", true_fn());
    context.set_function("false", false_fn());
    context.set_function("number", NumberFn);
    context.set_function("sum", Sum);
    context.set_function("floor", floor());
    context.set_function("ceiling", ceiling());
    context.set_function("round", round());
}

#[cfg(test)]
mod test {
    use std::borrow::ToOwned;
    use std::{f64, fmt};

    use sxd_document::Package;

    use crate::context;
    use crate::nodeset::Node;
    use crate::{LiteralValue, Value};

    use super::{
        ceiling, contains, floor, round, starts_with, substring_after, substring_before, BooleanFn,
        Concat, Count, Error, Function, Last, LocalName, Name, NamespaceUri, NormalizeSpace,
        NumberFn, Position, StringFn, StringLength, Substring, Sum, Translate,
    };

    /// Converts each argument into a `Value` and packs them into a
    /// vector.
    macro_rules! args {
        ( $($val:expr,)* ) => {
            vec![
                $( Value::from($val), )*
            ]
        };
        ( $($val:expr),* ) => {
            args![$($val, )*]
        };
    }

    struct Setup<'d> {
        context: context::Context<'d>,
    }

    impl<'d> Setup<'d> {
        fn new() -> Setup<'d> {
            Setup {
                context: context::Context::without_core_functions(),
            }
        }

        fn evaluate<N, F>(&self, node: N, f: F, args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
        where
            N: Into<Node<'d>>,
            F: Function,
        {
            let context = context::Evaluation::new(&self.context, node.into());
            f.evaluate(&context, args)
        }
    }

    fn evaluate_literal<F, F2, T>(f: F, args: Vec<LiteralValue>, rf: F2) -> T
    where
        F: Function,
        F2: FnOnce(Result<Value<'_>, Error>) -> T,
    {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        rf(setup.evaluate(doc.root(), f, args))
    }

    #[test]
    fn last_returns_context_size() {
        evaluate_literal(Last, args![], |r| {
            assert_eq!(Ok(Value::Number(1.0)), r);
        });
    }

    #[test]
    fn position_returns_context_position() {
        evaluate_literal(Position, args![], |r| {
            assert_eq!(Ok(Value::Number(1.0)), r);
        });
    }

    #[test]
    fn count_counts_nodes_in_nodeset() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let r = setup.evaluate(doc.root(), Count, args![nodeset![doc.root()]]);

        assert_eq!(Ok(Value::Number(1.0)), r);
    }

    #[test]
    fn local_name_gets_name_of_element() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let e = doc.create_element(("uri", "wow"));
        doc.root().append_child(e);

        let r = setup.evaluate(doc.root(), LocalName, args![nodeset![e]]);

        assert_eq!(Ok(Value::String("wow".to_owned())), r);
    }

    #[test]
    fn local_name_is_empty_for_empty_nodeset() {
        evaluate_literal(LocalName, args![nodeset![]], |r| {
            assert_eq!(Ok(Value::String("".to_owned())), r);
        });
    }

    #[test]
    fn namespace_uri_gets_uri_of_element() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let e = doc.create_element(("uri", "wow"));
        doc.root().append_child(e);

        let r = setup.evaluate(doc.root(), NamespaceUri, args![nodeset![e]]);

        assert_eq!(Ok(Value::String("uri".to_owned())), r);
    }

    #[test]
    fn name_uses_declared_prefix() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let e = doc.create_element(("uri", "wow"));
        e.register_prefix("prefix", "uri");
        doc.root().append_child(e);

        let r = setup.evaluate(doc.root(), Name, args![nodeset![e]]);

        assert_eq!(Ok(Value::String("prefix:wow".to_owned())), r);
    }

    #[test]
    fn string_converts_to_string() {
        evaluate_literal(StringFn, args![true], |r| {
            assert_eq!(Ok(Value::String("true".to_owned())), r);
        });
    }

    #[test]
    fn concat_combines_strings() {
        evaluate_literal(Concat, args!["hello", " ", "world"], |r| {
            assert_eq!(Ok(Value::String("hello world".to_owned())), r);
        });
    }

    #[test]
    fn starts_with_checks_prefixes() {
        evaluate_literal(starts_with(), args!["hello", "he"], |r| {
            assert_eq!(Ok(Value::Boolean(true)), r);
        });
    }

    #[test]
    fn contains_looks_for_a_needle() {
        evaluate_literal(contains(), args!["astronomer", "ono"], |r| {
            assert_eq!(Ok(Value::Boolean(true)), r);
        });
    }

    #[test]
    fn substring_before_slices_before() {
        evaluate_literal(substring_before(), args!["1999/04/01", "/"], |r| {
            assert_eq!(Ok(Value::String("1999".to_owned())), r);
        });
    }

    #[test]
    fn substring_after_slices_after() {
        evaluate_literal(substring_after(), args!["1999/04/01", "/"], |r| {
            assert_eq!(Ok(Value::String("04/01".to_owned())), r);
        });
    }

    #[test]
    fn substring_is_one_indexed() {
        evaluate_literal(Substring, args!["あいうえお", 2.0], |r| {
            assert_eq!(Ok(Value::String("いうえお".to_owned())), r);
        });
    }

    #[test]
    fn substring_has_optional_length() {
        evaluate_literal(Substring, args!["あいうえお", 2.0, 3.0], |r| {
            assert_eq!(Ok(Value::String("いうえ".to_owned())), r);
        });
    }

    fn substring_test(s: &str, start: f64, len: f64) -> String {
        evaluate_literal(Substring, args![s, start, len], |r| match r {
            Ok(Value::String(s)) => s,
            r => panic!("substring failed: {:?}", r),
        })
    }

    #[test]
    fn substring_rounds_values() {
        assert_eq!("いうえ", substring_test("あいうえお", 1.5, 2.6));
    }

    #[test]
    fn substring_is_a_window_of_the_characters() {
        assert_eq!("あい", substring_test("あいうえお", 0.0, 3.0));
    }

    #[test]
    fn substring_with_nan_start_is_empty() {
        assert_eq!("", substring_test("あいうえお", f64::NAN, 3.0));
    }

    #[test]
    fn substring_with_nan_len_is_empty() {
        assert_eq!("", substring_test("あいうえお", 1.0, f64::NAN));
    }

    #[test]
    fn substring_with_infinite_len_goes_to_end_of_string() {
        assert_eq!(
            "あいうえお",
            substring_test("あいうえお", -42.0, f64::INFINITY)
        );
    }

    #[test]
    fn substring_with_negative_infinity_start_is_empty() {
        assert_eq!(
            "",
            substring_test("あいうえお", f64::NEG_INFINITY, f64::INFINITY)
        );
    }

    #[test]
    fn string_length_counts_characters() {
        evaluate_literal(StringLength, args!["日本語"], |r| {
            assert_eq!(Ok(Value::Number(3.0)), r);
        });
    }

    #[test]
    fn normalize_space_removes_leading_space() {
        evaluate_literal(NormalizeSpace, args!["\t hello"], |r| {
            assert_eq!(Ok(Value::String("hello".to_owned())), r);
        });
    }

    #[test]
    fn normalize_space_removes_trailing_space() {
        evaluate_literal(NormalizeSpace, args!["hello\r\n"], |r| {
            assert_eq!(Ok(Value::String("hello".to_owned())), r);
        });
    }

    #[test]
    fn normalize_space_squashes_intermediate_space() {
        evaluate_literal(NormalizeSpace, args!["hello\t\r\n world"], |r| {
            assert_eq!(Ok(Value::String("hello world".to_owned())), r);
        });
    }

    fn translate_test(s: &str, from: &str, to: &str) -> String {
        evaluate_literal(Translate, args![s, from, to], |r| match r {
            Ok(Value::String(s)) => s,
            r => panic!("translate failed: {:?}", r),
        })
    }

    #[test]
    fn translate_replaces_characters() {
        assert_eq!("イエ", translate_test("いえ", "あいうえお", "アイウエオ"));
    }

    #[test]
    fn translate_removes_characters_without_replacement() {
        assert_eq!("イ", translate_test("いえ", "あいうえお", "アイ"));
    }

    #[test]
    fn translate_replaces_each_char_only_once() {
        assert_eq!("b", translate_test("a", "ab", "bc"));
    }

    #[test]
    fn translate_uses_first_replacement() {
        assert_eq!("b", translate_test("a", "aa", "bc"));
    }

    #[test]
    fn translate_ignores_extra_replacements() {
        assert_eq!("b", translate_test("a", "a", "bc"));
    }

    #[test]
    fn boolean_converts_to_boolean() {
        evaluate_literal(BooleanFn, args!["false"], |r| {
            assert_eq!(Ok(Value::Boolean(true)), r);
        });
    }

    #[test]
    fn number_converts_to_number() {
        evaluate_literal(NumberFn, args![" -1.2 "], |r| {
            assert_eq!(Ok(Value::Number(-1.2)), r);
        });
    }

    #[test]
    fn number_fails_with_nan() {
        evaluate_literal(NumberFn, args![" nope "], |r| assert_number(f64::NAN, r));
    }

    #[test]
    fn sum_adds_up_nodeset() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let c = doc.create_comment("-32.0");
        let t = doc.create_text("98.7");

        let r = setup.evaluate(doc.root(), Sum, args![nodeset![c, t]]);

        assert_eq!(Ok(Value::Number(66.7)), r);
    }

    /// By default, NaN != NaN and -0.0 == 0.0. We don't want either
    /// of those to be true.
    struct PedanticNumber(f64);

    impl PedanticNumber {
        fn non_nan_key(&self) -> (bool, bool, f64) {
            (self.0.is_finite(), self.0.is_sign_positive(), self.0)
        }
    }

    impl PartialEq for PedanticNumber {
        fn eq(&self, other: &Self) -> bool {
            if self.0.is_nan() {
                other.0.is_nan()
            } else {
                self.non_nan_key() == other.non_nan_key()
            }
        }
    }

    impl fmt::Debug for PedanticNumber {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "{{ {}, NaN: {}, finite: {}, positive: {} }}",
                self.0,
                self.0.is_nan(),
                self.0.is_finite(),
                self.0.is_sign_positive()
            )
        }
    }

    fn assert_number(expected: f64, actual: Result<Value<'_>, Error>) {
        match actual {
            Ok(Value::Number(n)) => assert_eq!(PedanticNumber(n), PedanticNumber(expected)),
            _ => assert!(false, "{:?} did not evaluate correctly", actual),
        }
    }

    #[test]
    fn floor_rounds_down() {
        evaluate_literal(floor(), args![199.99], |r| assert_number(199.0, r));
    }

    #[test]
    fn ceiling_rounds_up() {
        evaluate_literal(ceiling(), args![199.99], |r| assert_number(200.0, r));
    }

    #[test]
    fn round_nan_to_nan() {
        evaluate_literal(round(), args![f64::NAN], |r| assert_number(f64::NAN, r));
    }

    #[test]
    fn round_pos_inf_to_pos_inf() {
        evaluate_literal(round(), args![f64::INFINITY], |r| {
            assert_number(f64::INFINITY, r)
        });
    }

    #[test]
    fn round_neg_inf_to_neg_inf() {
        evaluate_literal(round(), args![f64::NEG_INFINITY], |r| {
            assert_number(f64::NEG_INFINITY, r)
        });
    }

    #[test]
    fn round_pos_zero_to_pos_zero() {
        evaluate_literal(round(), args![0.0], |r| assert_number(0.0, r));
    }

    #[test]
    fn round_neg_zero_to_neg_zero() {
        evaluate_literal(round(), args![-0.0], |r| assert_number(-0.0, r));
    }

    #[test]
    fn round_neg_zero_point_five_to_neg_zero() {
        evaluate_literal(round(), args![-0.5], |r| assert_number(-0.0, r));
    }

    #[test]
    fn round_neg_five_to_neg_five() {
        evaluate_literal(round(), args![-5.0], |r| assert_number(-5.0, r));
    }

    #[test]
    fn round_pos_zero_point_five_to_pos_one() {
        evaluate_literal(round(), args![0.5], |r| assert_number(1.0, r));
    }
}

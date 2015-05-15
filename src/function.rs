use std::borrow::ToOwned;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::ops::Index;
use std::{error,fmt,iter};

use sxd_document::XmlChar;

use super::{EvaluationContext,Functions,Value};
use super::nodeset::Nodeset;

pub trait Function {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>;
}

#[derive(Copy,Clone,Debug,PartialEq,Hash)]
pub enum ArgumentType {
    Nodeset,
    Boolean,
    Number,
    String,
}

#[derive(Copy,Clone,Debug,PartialEq,Hash)]
pub enum Error {
    TooManyArguments{ expected: usize, actual: usize },
    NotEnoughArguments{ expected: usize, actual: usize },
    WrongType{ expected: ArgumentType, actual: ArgumentType },
}

impl Error {
    fn wrong_type(actual: &Value, expected: ArgumentType) -> Error {
        let actual = match *actual {
            Value::Nodeset(..) => ArgumentType::Nodeset,
            Value::String(..)  => ArgumentType::String,
            Value::Number(..)  => ArgumentType::Number,
            Value::Boolean(..) => ArgumentType::Boolean,
        };

        Error::WrongType {
            expected: expected,
            actual: actual
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        use self::Error::*;
        match *self {
            TooManyArguments{..}   => "too many arguments",
            NotEnoughArguments{..} => "not enough arguments",
            WrongType{..}          => "argument of wrong type",
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Error::*;
        match *self {
            TooManyArguments{expected, actual} => {
                write!(fmt, "too many arguments, expected {} but had {}", expected, actual)
            },
            NotEnoughArguments{expected, actual} => {
                write!(fmt, "not enough arguments, expected {} but had {}", expected, actual)
            },
            WrongType{expected, actual} => {
                write!(fmt, "argument was the wrong type, expected {:?} but had {:?}", expected, actual)
            },
        }
    }
}

struct Args<'d>(Vec<Value<'d>>);

impl<'d> Args<'d> {
    fn len(&self) -> usize { self.0.len() }

    fn at_least(&self, minimum: usize) -> Result<(), Error> {
        let actual = self.0.len();
        if actual < minimum {
            Err(Error::NotEnoughArguments{expected: minimum, actual: actual})
        } else {
            Ok(())
        }
    }

    fn at_most(&self, maximum: usize) -> Result<(), Error> {
        let actual = self.0.len();
        if actual > maximum {
            Err(Error::TooManyArguments{expected: maximum, actual: actual})
        } else {
            Ok(())
        }
    }

    fn exactly(&self, expected: usize) -> Result<(), Error> {
        let actual = self.0.len();
        if actual < expected {
            Err(Error::NotEnoughArguments{ expected: expected, actual: actual })
        } else if actual > expected {
            Err(Error::TooManyArguments{ expected: expected, actual: actual })
        } else {
            Ok(())
        }
    }

    fn into_strings(self) -> Result<Vec<String>, Error> {
        fn string_arg(v: Value) -> Result<String, Error> {
            match v {
                Value::String(s) => Ok(s),
                _ => Err(Error::wrong_type(&v, ArgumentType::String)),
            }
        }

        self.0.into_iter().map(string_arg).collect()
    }

    fn pop_boolean(&mut self) -> Result<bool, Error> {
        match self.0.pop().unwrap() {
            Value::Boolean(v) => Ok(v),
            a => Err(Error::wrong_type(&a, ArgumentType::Boolean)),
        }
    }

    fn pop_number(&mut self) -> Result<f64, Error> {
        match self.0.pop().unwrap() {
            Value::Number(v) => Ok(v),
            a => Err(Error::wrong_type(&a, ArgumentType::Number)),
        }
    }

    fn pop_string(&mut self) -> Result<String, Error> {
        match self.0.pop().unwrap() {
            Value::String(v) => Ok(v),
            a => Err(Error::wrong_type(&a, ArgumentType::String)),
        }
    }

    fn pop_nodeset(&mut self) -> Result<Nodeset<'d>, Error> {
        match self.0.pop().unwrap() {
            Value::Nodeset(v) => Ok(v),
            a => Err(Error::wrong_type(&a, ArgumentType::Nodeset)),
        }
    }

    fn pop_value_or_context_node<'_>(&mut self, context: &EvaluationContext<'_, 'd>) -> Value<'d> {
        self.0.pop()
            .unwrap_or_else(|| Value::Nodeset(nodeset![context.node]))
    }

    fn pop_string_value_or_context_node(&mut self, context: &EvaluationContext) -> Result<String, Error> {
        match self.0.pop() {
            Some(Value::String(s)) => Ok(s),
            Some(arg) => Err(Error::wrong_type(&arg, ArgumentType::String)),
            None => Ok(context.node.string_value()),
        }
    }


    fn pop_nodeset_or_context_node<'_>(&mut self, context: &EvaluationContext<'_, 'd>)
                                       -> Result<Nodeset<'d>, Error>
    {
        match self.0.pop() {
            Some(Value::Nodeset(ns)) => Ok(ns),
            Some(arg) => Err(Error::wrong_type(&arg, ArgumentType::Nodeset)),
            None => Ok(nodeset![context.node]),
        }
    }
}

impl<'d> Index<usize> for Args<'d> {
    type Output = Value<'d>;

    fn index(&self, index: usize) -> &Value<'d> { self.0.index(index) }
}

struct Last;

impl Function for Last {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let args = Args(args);
        try!(args.exactly(0));
        Ok(Value::Number(context.size() as f64))
    }
}

struct Position;

impl Function for Position {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let args = Args(args);
        try!(args.exactly(0));
        Ok(Value::Number(context.position() as f64))
    }
}

struct Count;

impl Function for Count {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.exactly(1));
        let arg = try!(args.pop_nodeset());
        Ok(Value::Number(arg.size() as f64))
    }
}

struct LocalName;

impl Function for LocalName {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.at_most(1));
        let arg = try!(args.pop_nodeset_or_context_node(context));
        let name =
            arg.document_order_first()
            .and_then(|n| n.expanded_name())
            .map(|q| q.local_part())
            .unwrap_or("");
        Ok(Value::String(name.to_owned()))
    }
}

struct NamespaceUri;

impl Function for NamespaceUri {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.at_most(1));
        let arg = try!(args.pop_nodeset_or_context_node(context));
        let name =
            arg.document_order_first()
            .and_then(|n| n.expanded_name())
            .and_then(|q| q.namespace_uri())
            .unwrap_or("");
        Ok(Value::String(name.to_owned()))
    }
}

struct Name;

impl Function for Name {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.at_most(1));
        let arg = try!(args.pop_nodeset_or_context_node(context));
        let name =
            arg.document_order_first()
            .and_then(|n| n.prefixed_name())
            .unwrap_or("".to_owned());
        Ok(Value::String(name))
    }
}

struct StringFn;

impl Function for StringFn {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.at_most(1));
        let arg = args.pop_value_or_context_node(context);
        Ok(Value::String(arg.string()))
    }
}


struct Concat;

impl Function for Concat {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let args = Args(args);
        try!(args.at_least(2));
        let args = try!(args.into_strings());
        Ok(Value::String(args.concat()))
    }
}

struct TwoStringPredicate(fn(&str, &str) -> bool);

impl Function for TwoStringPredicate {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let args = Args(args);
        try!(args.exactly(2));
        let args = try!(args.into_strings());
        let v = self.0(&args[0], &args[1]);
        Ok(Value::Boolean(v))
    }
}

fn starts_with() -> TwoStringPredicate {
    fn imp(a: &str, b: &str) -> bool { str::starts_with(a, b) };
    TwoStringPredicate(imp)
}
fn contains() -> TwoStringPredicate {
    fn imp(a: &str, b: &str) -> bool { str::contains(a, b) };
    TwoStringPredicate(imp)
}

struct SubstringCommon(for<'s> fn(&'s str, &'s str) -> &'s str);

impl Function for SubstringCommon {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let args = Args(args);
        try!(args.exactly(2));
        let args = try!(args.into_strings());
        let s = self.0(&*args[0], &*args[1]);
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
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.at_least(2));
        try!(args.at_most(3));

        let len = if args.len() == 3 {
            let len = try!(args.pop_number());
            round_ties_to_positive_infinity(len)
        } else {
            ::std::f64::INFINITY
        };

        let start = try!(args.pop_number());
        let start = round_ties_to_positive_infinity(start);
        let s = try!(args.pop_string());

        let chars = s.chars().enumerate();
        let selected_chars = chars.filter_map(|(p, s)| {
            let p = (p+1) as f64; // 1-based indexing
            if p >= start && p < start + len {
                Some(s)
            } else {
                None
            }
        }).collect() ;

        Ok(Value::String(selected_chars))
    }
}

struct StringLength;

impl Function for StringLength {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.at_most(1));
        let arg = try!(args.pop_string_value_or_context_node(context));
        Ok(Value::Number(arg.chars().count() as f64))
    }
}

struct NormalizeSpace;

impl Function for NormalizeSpace {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.at_most(1));
        let arg = try!(args.pop_string_value_or_context_node(context));
        // TODO: research itertools or another pure-iterator solution
        let s: Vec<_> = arg.split(XmlChar::is_space_char).filter(|s| !s.is_empty()).collect();
        let s = s.connect(" ");
        Ok(Value::String(s))
    }
}

struct Translate;

impl Function for Translate {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.exactly(3));

        let to = try!(args.pop_string());
        let from = try!(args.pop_string());
        let s = try!(args.pop_string());

        let mut replacements = HashMap::new();
        let pairs = from.chars().zip(to.chars().map(|c| Some(c)).chain(iter::repeat(None)));
        for (from, to) in pairs {
            if let Entry::Vacant(entry) = replacements.entry(from) {
                entry.insert(to);
            }
        }

        let s = s.chars().filter_map(|c| {
            replacements.get(&c).map(|&s| s).unwrap_or(Some(c))
        }).collect();

        Ok(Value::String(s))
    }
}

struct BooleanFn;

impl Function for BooleanFn {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let args = Args(args);
        try!(args.exactly(1));
        Ok(Value::Boolean(args[0].boolean()))
    }
}

struct Not;

impl Function for Not {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.exactly(1));
        let arg = try!(args.pop_boolean());
        Ok(Value::Boolean(!arg))
    }
}

struct BooleanLiteral(bool);

impl Function for BooleanLiteral {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let args = Args(args);
        try!(args.exactly(0));
        Ok(Value::Boolean(self.0))
    }
}

fn true_fn() -> BooleanLiteral { BooleanLiteral(true) }
fn false_fn() -> BooleanLiteral { BooleanLiteral(false) }

struct NumberFn;

impl Function for NumberFn {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.at_most(1));
        let arg = args.pop_value_or_context_node(context);
        Ok(Value::Number(arg.number()))
    }
}

struct Sum;

impl Function for Sum {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.exactly(1));
        let arg = try!(args.pop_nodeset());
        let r = arg.iter().map(|n| super::str_to_num(&n.string_value())).fold(0.0, |acc, i| acc + i);
        Ok(Value::Number(r))
    }
}

struct NumberConvert(fn(f64) -> f64);

impl Function for NumberConvert {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        let mut args = Args(args);
        try!(args.exactly(1));
        let arg = try!(args.pop_number());
        Ok(Value::Number(self.0(arg)))
    }
}

fn floor() -> NumberConvert { NumberConvert(f64::floor) }
fn ceiling() -> NumberConvert { NumberConvert(f64::ceil) }

// http://stackoverflow.com/a/28124775/155423
fn round_ties_to_positive_infinity(x: f64) -> f64 {
    let y = x.floor();
    if x == y {
        x
    } else {
        let z = (2.0*x-y).floor();
        z * x.signum() // Should use copysign
    }
}

fn round() -> NumberConvert { NumberConvert(round_ties_to_positive_infinity) }

pub fn register_core_functions(functions: &mut Functions) {
    functions.insert("last".to_owned(), Box::new(Last));
    functions.insert("position".to_owned(), Box::new(Position));
    functions.insert("count".to_owned(), Box::new(Count));
    functions.insert("local-name".to_owned(), Box::new(LocalName));
    functions.insert("namespace-uri".to_owned(), Box::new(NamespaceUri));
    functions.insert("name".to_owned(), Box::new(Name));
    functions.insert("string".to_owned(), Box::new(StringFn));
    functions.insert("concat".to_owned(), Box::new(Concat));
    functions.insert("starts-with".to_owned(), Box::new(starts_with()));
    functions.insert("contains".to_owned(), Box::new(contains()));
    functions.insert("substring-before".to_owned(), Box::new(substring_before()));
    functions.insert("substring-after".to_owned(), Box::new(substring_after()));
    functions.insert("substring".to_owned(), Box::new(Substring));
    functions.insert("string-length".to_owned(), Box::new(StringLength));
    functions.insert("normalize-space".to_owned(), Box::new(NormalizeSpace));
    functions.insert("translate".to_owned(), Box::new(Translate));
    functions.insert("boolean".to_owned(), Box::new(BooleanFn));
    functions.insert("not".to_owned(), Box::new(Not));
    functions.insert("true".to_owned(), Box::new(true_fn()));
    functions.insert("false".to_owned(), Box::new(false_fn()));
    functions.insert("number".to_owned(), Box::new(NumberFn));
    functions.insert("sum".to_owned(), Box::new(Sum));
    functions.insert("floor".to_owned(), Box::new(floor()));
    functions.insert("ceiling".to_owned(), Box::new(ceiling()));
    functions.insert("round".to_owned(), Box::new(round()));
}

#[cfg(test)]
mod test {
    use std::borrow::ToOwned;
    use std::collections::HashMap;

    use sxd_document::Package;

    use super::super::{EvaluationContext,LiteralValue,Value,Functions,Variables,Namespaces};
    use super::super::nodeset::Node;
    use super::{
        Function,
        Error,
        Last,
        Position,
        Count,
        LocalName,
        NamespaceUri,
        Name,
        StringFn,
        Concat,
        Substring,
        StringLength,
        NormalizeSpace,
        Translate,
        BooleanFn,
        NumberFn,
        Sum,
    };

    struct Setup<'d> {
        functions: Functions,
        variables: Variables<'d>,
        namespaces: Namespaces,
    }

    impl<'d> Setup<'d> {
        fn new() -> Setup<'d> {
            Setup {
                functions: HashMap::new(),
                variables: HashMap::new(),
                namespaces: HashMap::new(),
            }
        }

        fn evaluate<N, F>(&self, node: N, f: F, args: Vec<Value<'d>>)
            -> Result<Value<'d>, Error>
            where N: Into<Node<'d>>,
                  F: Function
        {
            let context = EvaluationContext::new(
                node, &self.functions, &self.variables, &self.namespaces
            );
            f.evaluate(&context, args)
        }
    }

    fn evaluate_literal<F>(f: F, args: Vec<LiteralValue>) -> Result<LiteralValue, Error>
        where F: Function
    {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let args = args.into_iter().map(|a| a.into_value()).collect();

        let r = setup.evaluate(doc.root(), f, args);

        r.map(|r| r.into_literal_value())
    }

    #[test]
    fn last_returns_context_size() {
        let r = evaluate_literal(Last, vec![]);
        assert_eq!(Ok(LiteralValue::Number(1.0)), r);
    }

    #[test]
    fn position_returns_context_position() {
        let r = evaluate_literal(Position, vec![]);

        assert_eq!(Ok(LiteralValue::Number(1.0)), r);
    }

    #[test]
    fn count_counts_nodes_in_nodeset() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let nodeset = nodeset![doc.root()];
        let r = setup.evaluate(doc.root(), Count, vec![Value::Nodeset(nodeset)]);

        assert_eq!(Ok(Value::Number(1.0)), r);
    }

    #[test]
    fn local_name_gets_name_of_element() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let e = doc.create_element(("uri", "wow"));
        doc.root().append_child(e);

        let nodeset = nodeset![e];
        let r = setup.evaluate(doc.root(), LocalName, vec![Value::Nodeset(nodeset)]);

        assert_eq!(Ok(Value::String("wow".to_owned())), r);
    }

    #[test]
    fn local_name_is_empty_for_empty_nodeset() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let nodeset = nodeset![];
        let r = setup.evaluate(doc.root(), LocalName, vec![Value::Nodeset(nodeset)]);

        assert_eq!(Ok(Value::String("".to_owned())), r);
    }

    #[test]
    fn namespace_uri_gets_uri_of_element() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let e = doc.create_element(("uri", "wow"));
        doc.root().append_child(e);

        let nodeset = nodeset![e];
        let r = setup.evaluate(doc.root(), NamespaceUri, vec![Value::Nodeset(nodeset)]);

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

        let nodeset = nodeset![e];
        let r = setup.evaluate(doc.root(), Name, vec![Value::Nodeset(nodeset)]);

        assert_eq!(Ok(Value::String("prefix:wow".to_owned())), r);
    }

    #[test]
    fn string_converts_to_string() {
        let args = vec![LiteralValue::Boolean(true)];
        let r = evaluate_literal(StringFn, args);

        assert_eq!(Ok(LiteralValue::String("true".to_owned())), r);
    }

    #[test]
    fn concat_combines_strings() {
        let args = vec![LiteralValue::String("hello".to_owned()),
                        LiteralValue::String(" ".to_owned()),
                        LiteralValue::String("world".to_owned())];
        let r = evaluate_literal(Concat, args);

        assert_eq!(Ok(LiteralValue::String("hello world".to_owned())), r);
    }

    #[test]
    fn starts_with_checks_prefixes() {
        let args = vec![LiteralValue::String("hello".to_owned()),
                        LiteralValue::String("he".to_owned())];
        let r = evaluate_literal(super::starts_with(), args);

        assert_eq!(Ok(LiteralValue::Boolean(true)), r);
    }

    #[test]
    fn contains_looks_for_a_needle() {
        let args = vec![LiteralValue::String("astronomer".to_owned()),
                        LiteralValue::String("ono".to_owned())];
        let r = evaluate_literal(super::contains(), args);

        assert_eq!(Ok(LiteralValue::Boolean(true)), r);
    }

    #[test]
    fn substring_before_slices_before() {
        let args = vec![LiteralValue::String("1999/04/01".to_owned()),
                        LiteralValue::String("/".to_owned())];
        let r = evaluate_literal(super::substring_before(), args);

        assert_eq!(Ok(LiteralValue::String("1999".to_owned())), r);
    }

    #[test]
    fn substring_after_slices_after() {
        let args = vec![LiteralValue::String("1999/04/01".to_owned()),
                        LiteralValue::String("/".to_owned())];
        let r = evaluate_literal(super::substring_after(), args);

        assert_eq!(Ok(LiteralValue::String("04/01".to_owned())), r);
    }

    #[test]
    fn substring_is_one_indexed() {
        let args = vec![LiteralValue::String("あいうえお".to_owned()),
                        LiteralValue::Number(2.0)];
        let r = evaluate_literal(Substring, args);

        assert_eq!(Ok(LiteralValue::String("いうえお".to_owned())), r);
    }

    #[test]
    fn substring_has_optional_length() {
        let args = vec![LiteralValue::String("あいうえお".to_owned()),
                        LiteralValue::Number(2.0),
                        LiteralValue::Number(3.0)];
        let r = evaluate_literal(Substring, args);

        assert_eq!(Ok(LiteralValue::String("いうえ".to_owned())), r);
    }

    fn substring_test(s: &str, start: f64, len: f64) -> String {
        let args = vec![LiteralValue::String(s.to_owned()),
                        LiteralValue::Number(start),
                        LiteralValue::Number(len)];

        match evaluate_literal(Substring, args) {
            Ok(LiteralValue::String(s)) => s,
            r => panic!("substring failed: {:?}", r),
        }
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
        assert_eq!("", substring_test("あいうえお", ::std::f64::NAN, 3.0));
    }

    #[test]
    fn substring_with_nan_len_is_empty() {
        assert_eq!("", substring_test("あいうえお", 1.0, ::std::f64::NAN));
    }

    #[test]
    fn substring_with_infinite_len_goes_to_end_of_string() {
        assert_eq!("あいうえお", substring_test("あいうえお", -42.0, ::std::f64::INFINITY));
    }

    #[test]
    fn substring_with_negative_infinity_start_is_empty() {
        assert_eq!("", substring_test("あいうえお", ::std::f64::NEG_INFINITY, ::std::f64::INFINITY));
    }

    #[test]
    fn string_length_counts_characters() {
        let args = vec![LiteralValue::String("日本語".to_owned())];
        let r = evaluate_literal(StringLength, args);

        assert_eq!(Ok(LiteralValue::Number(3.0)), r);
    }

    #[test]
    fn normalize_space_removes_leading_space() {
        let args = vec![LiteralValue::String("\t hello".to_owned())];
        let r = evaluate_literal(NormalizeSpace, args);

        assert_eq!(Ok(LiteralValue::String("hello".to_owned())), r);
    }

    #[test]
    fn normalize_space_removes_trailing_space() {
        let args = vec![LiteralValue::String("hello\r\n".to_owned())];
        let r = evaluate_literal(NormalizeSpace, args);

        assert_eq!(Ok(LiteralValue::String("hello".to_owned())), r);
    }

    #[test]
    fn normalize_space_squashes_intermediate_space() {
        let args = vec![LiteralValue::String("hello\t\r\n world".to_owned())];
        let r = evaluate_literal(NormalizeSpace, args);

        assert_eq!(Ok(LiteralValue::String("hello world".to_owned())), r);
    }

    fn translate_test(s: &str, from: &str, to: &str) -> String {
        let args = vec![LiteralValue::String(s.to_owned()),
                        LiteralValue::String(from.to_owned()),
                        LiteralValue::String(to.to_owned())];

        match evaluate_literal(Translate, args) {
            Ok(LiteralValue::String(s)) => s,
            r => panic!("translate failed: {:?}", r)
        }
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
        let args = vec![LiteralValue::String("false".to_owned())];
        let r = evaluate_literal(BooleanFn, args);

        assert_eq!(Ok(LiteralValue::Boolean(true)), r);
    }

    #[test]
    fn number_converts_to_number() {
        let args = vec![LiteralValue::String(" -1.2 ".to_owned())];
        let r = evaluate_literal(NumberFn, args);

        assert_eq!(Ok(LiteralValue::Number(-1.2)), r);
    }

    #[test]
    fn sum_adds_up_nodeset() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let c = doc.create_comment("-32.0");
        let t = doc.create_text("98.7");

        let nodeset = nodeset![c, t];
        let r = setup.evaluate(doc.root(), Sum, vec![Value::Nodeset(nodeset)]);

        assert_eq!(Ok(Value::Number(66.7)), r);
    }

    fn assert_number<F>(f: F, val: f64, expected: f64)
        where F: Function
    {
        let r = evaluate_literal(f, vec![LiteralValue::Number(val)]);

        if expected.is_nan() {
            match r {
                Ok(LiteralValue::Number(a)) => assert!(a.is_nan(), "{} should be NaN", a),
                _ => assert!(false, "{:?} did not evaluate correctly", r),
            }
        } else {
            assert_eq!(Ok(LiteralValue::Number(expected)), r);
        }
    }

    #[test]
    fn floor_rounds_down() {
        assert_number(super::floor(), 199.99, 199.0);
    }

    #[test]
    fn ceiling_rounds_up() {
        assert_number(super::ceiling(), 199.99, 200.0);
    }

    #[test]
    fn round_nan_to_nan() {
        assert_number(super::round(), ::std::f64::NAN, ::std::f64::NAN);
    }

    #[test]
    fn round_pos_inf_to_pos_inf() {
        assert_number(super::round(), ::std::f64::INFINITY, ::std::f64::INFINITY);
    }

    #[test]
    fn round_neg_inf_to_neg_inf() {
        assert_number(super::round(), ::std::f64::NEG_INFINITY, ::std::f64::NEG_INFINITY);
    }

    #[test]
    fn round_pos_zero_to_pos_zero() {
        assert_number(super::round(), 0.0, 0.0);
    }

    #[test]
    fn round_neg_zero_to_neg_zero() {
        assert_number(super::round(), -0.0, -0.0);
    }

    #[test]
    fn round_neg_zero_point_five_to_neg_zero() {
        assert_number(super::round(), -0.5, -0.0);
    }

    #[test]
    fn round_pos_zero_point_five_to_pos_one() {
        assert_number(super::round(), 0.5, 1.0);
    }
}

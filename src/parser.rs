use std::iter::Peekable;

use self::ParseErr::*;

use super::token::{Token,AxisName,NodeTestName};
use super::tokenizer::{TokenResult,TokenizerErr};
use super::axis;
use super::axis::{Axis,SubAxis,PrincipalNodeType};
use super::expression;
use super::expression::{SubExpression,LiteralValue};
use super::node_test;
use super::node_test::SubNodeTest;

#[allow(missing_copy_implementations)]
pub struct Parser;

impl Parser {
    pub fn new() -> Parser {
        Parser
    }
}

#[derive(Show,PartialEq,Clone)]
pub enum ParseErr {
    EmptyPredicate,
    ExtraUnparsedTokens,
    RanOutOfInput,
    RightHandSideExpressionMissing,
    TokenizerError(TokenizerErr),
    TrailingSlash,
    UnexpectedToken(Token),
}

pub type ParseResult = Result<Option<SubExpression>, ParseErr>;

type BinaryExpressionBuilder = fn(SubExpression, SubExpression) -> SubExpression;

struct BinaryRule {
    token: Token,
    builder: BinaryExpressionBuilder,
}

struct LeftAssociativeBinaryParser<I> {
    rules: Vec<BinaryRule>,
}

type TokenSource<'a, I> = &'a mut Peekable<TokenResult, I>;

trait XCompat {
    fn has_more_tokens(&mut self) -> bool;
    fn next_token_is(&mut self, token: &Token) -> bool;
    fn consume(&mut self, token: &Token) -> Result<(), ParseErr>;
}

impl<I> XCompat for Peekable<TokenResult, I>
    where I: Iterator<Item=TokenResult>
{
    fn has_more_tokens(&mut self) -> bool {
        self.peek().is_some()
    }

    fn next_token_is(&mut self, token: &Token) -> bool {
        match self.peek() {
            Some(&Ok(ref t)) => t == token,
            _ => false
        }
    }

    fn consume(&mut self, token: &Token) -> Result<(), ParseErr> {
        match self.next() {
            None => Err(RanOutOfInput),
            Some(Err(x)) => Err(TokenizerError(x)),
            Some(Ok(x)) =>
                if &x == token {
                    Ok(())
                } else {
                    Err(UnexpectedToken(x))
                },
        }
    }
}

/// Similar to `consume`, but can be used when the token carries a
/// single value.
macro_rules! consume_value(
    ($source:expr, Token::$token:ident) => (
        match $source.next() {
            None => return Err(RanOutOfInput),
            Some(Err(x)) => return Err(TokenizerError(x)),
            Some(Ok(Token::$token(x))) => x,
            Some(Ok(x)) => return Err(UnexpectedToken(x)),
        }
    );
);

/// Similar to `next_token_is`, but can be used when the token carries
/// a single value
macro_rules! next_token_is(
    ($source:expr, Token::$token:ident) => (
        match $source.peek() {
            Some(&Ok(Token::$token(_))) => true,
            _ => false,
        }
    );
);

impl<I> LeftAssociativeBinaryParser<I>
    where I: Iterator<Item=TokenResult>
{
    fn new(rules: Vec<BinaryRule>) -> LeftAssociativeBinaryParser<I> {
        LeftAssociativeBinaryParser {
            rules: rules,
        }
    }

    fn parse<F>(&self, source: TokenSource<I>, child_parse: F) -> ParseResult
        where F: Fn(TokenSource<I>) -> ParseResult
    {
        let left = try!(child_parse(source));

        let mut left = match left {
            None => return Ok(None),
            Some(x) => x,
        };

        while source.has_more_tokens() {
            let mut found = false;

            for rule in self.rules.iter() {
                if source.next_token_is(&rule.token) {
                    try!(source.consume(&rule.token));

                    let right = try!(child_parse(source));

                    let right = match right {
                        None => return Err(RightHandSideExpressionMissing),
                        Some(x) => x,
                    };

                    left = (rule.builder)(left, right);

                    found = true;
                    break;
                }
            }

            if !found { break; }
        }

        Ok(Some(left))
    }
}

fn first_matching_rule<I>(child_parses: &[&Fn(TokenSource<I>) -> ParseResult],
                          source: TokenSource<I>)
                          -> ParseResult
    where I: Iterator<Item=TokenResult>
{
    for child_parse in child_parses.iter_mut() {
        let expr = try!((*child_parse)(source));
        if expr.is_some() {
            return Ok(expr);
        }
    }

    Ok(None)
}

#[old_impl_check]
impl<I> Parser
    where I: Iterator<Item=TokenResult>
{
    fn parse_axis(&self, source: TokenSource<I>) -> Result<SubAxis, ParseErr> {
        if next_token_is!(source, Token::Axis) {
            let name = consume_value!(source, Token::Axis);

            match name {
                AxisName::Self => Ok(box axis::Self as SubAxis),
                AxisName::Parent => Ok(box axis::Parent as SubAxis),
                AxisName::Descendant => Ok(box axis::Descendant as SubAxis),
                AxisName::DescendantOrSelf => Ok(box axis::DescendantOrSelf as SubAxis),
                AxisName::Attribute => Ok(box axis::Attribute as SubAxis),
                AxisName::Ancestor => Ok(box axis::Ancestor as SubAxis),
                AxisName::AncestorOrSelf => Ok(box axis::AncestorOrSelf as SubAxis),
                AxisName::PrecedingSibling => Ok(box axis::PrecedingSibling as SubAxis),
                AxisName::FollowingSibling => Ok(box axis::FollowingSibling as SubAxis),
                AxisName::Preceding => Ok(box axis::Preceding as SubAxis),
                AxisName::Following => Ok(box axis::Following as SubAxis),
                _ => unimplemented!(),
            }
        } else {
            Ok(box axis::Child as SubAxis)
        }
    }

    fn parse_node_test(&self, source: TokenSource<I>) -> Result<Option<SubNodeTest>, ParseErr> {
        if next_token_is!(source, Token::NodeTest) {
            let name = consume_value!(source, Token::NodeTest);

            match name {
                NodeTestName::Node => Ok(Some(box node_test::Node as SubNodeTest)),
                NodeTestName::Text => Ok(Some(box node_test::Text as SubNodeTest)),
                _ => unimplemented!(),
            }
        } else {
            Ok(None)
        }
    }

    fn default_node_test(&self, source: TokenSource<I>, axis: &Axis)
                         -> Result<Option<SubNodeTest>,ParseErr>
    {
        if next_token_is!(source, Token::NameTest) {
            let name = consume_value!(source, Token::NameTest);

            let test = match axis.principal_node_type() {
                PrincipalNodeType::Attribute =>
                    box node_test::Attribute::new(name) as SubNodeTest,
                PrincipalNodeType::Element =>
                    box node_test::Element::new(name) as SubNodeTest,
            };

            Ok(Some(test))
        } else {
            Ok(None)
        }
    }

    fn parse_variable_reference(&self, source: TokenSource<I>) -> ParseResult {
        if next_token_is!(source, Token::Variable) {
            let name = consume_value!(source, Token::Variable);
            Ok(Some(box expression::Variable { name: name } as SubExpression))
        } else {
            Ok(None)
        }
    }

    fn parse_string_literal(&self, source: TokenSource<I>) -> ParseResult {
        if next_token_is!(source, Token::Literal) {
            let value = consume_value!(source, Token::Literal);
            Ok(Some(box expression::Literal { value: LiteralValue::StringLiteral(value) } as SubExpression))
        } else {
            Ok(None)
        }
    }

    fn parse_numeric_literal(&self, source: TokenSource<I>) -> ParseResult {
        if next_token_is!(source, Token::Number) {
            let value = consume_value!(source, Token::Number);
            Ok(Some(box expression::Literal { value: LiteralValue::NumberLiteral(value) } as SubExpression))
        } else {
            Ok(None)
        }
    }

    fn parse_function_call(&self, source: TokenSource<I>) -> ParseResult {
        if next_token_is!(source, Token::Function) {
            let name = consume_value!(source, Token::Function);

            let mut arguments = Vec::new();

            try!(source.consume(&Token::LeftParen));
            while ! source.next_token_is(&Token::RightParen) {
                let arg = try!(self.parse_expression(source));
                match arg {
                    Some(arg) => arguments.push(arg),
                    None => break,
                }
            }
            try!(source.consume(&Token::RightParen));

            Ok(Some(box expression::Function{ name: name, arguments: arguments } as SubExpression))
        } else {
            Ok(None)
        }
    }

    fn parse_primary_expression(&self, source: TokenSource<I>) -> ParseResult {
        let mut child_parses = vec![
            |src: TokenSource<I>| self.parse_variable_reference(src),
            |src: TokenSource<I>| self.parse_string_literal(src),
            |src: TokenSource<I>| self.parse_numeric_literal(src),
            |src: TokenSource<I>| self.parse_function_call(src),
        ];

        first_matching_rule(&mut child_parses, source)
    }

    fn parse_predicate_expression(&self, source: TokenSource<I>) -> ParseResult {
        if source.next_token_is(&Token::LeftBracket) {
            try!(source.consume(&Token::LeftBracket));

            match try!(self.parse_expression(source)) {
                Some(predicate) => {
                    try!(source.consume(&Token::RightBracket));
                    Ok(Some(predicate))
                },
                None => Err(EmptyPredicate),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_predicates(&self, source: TokenSource<I>)
                        -> Result<Vec<SubExpression>,ParseErr>
    {
        let mut predicates = Vec::new();

        while let Some(predicate) = try!(self.parse_predicate_expression(source)) {
            predicates.push(predicate)
        }

        Ok(predicates)
    }

    fn parse_step(&self, source: TokenSource<I>) -> Result<Option<expression::Step>, ParseErr> {
        let axis = try!(self.parse_axis(source));

        let node_test = match try!(self.parse_node_test(source)) {
            Some(test) => Some(test),
            None => try!(self.default_node_test(source, &*axis)),
        };

        let node_test = match node_test {
            Some(test) => test,
            None => return Ok(None),
        };

        let predicates = try!(self.parse_predicates(source));

        Ok(Some(expression::Step::new(axis, node_test, predicates)))
    }

    fn parse_relative_location_path_raw(&self,
                                        source: TokenSource<I>,
                                        start_point: SubExpression) -> ParseResult
    {
        match try!(self.parse_step(source)) {
            Some(step) => {
                let mut steps = vec![step];

                while source.next_token_is(&Token::Slash) {
                    try!(source.consume(&Token::Slash));

                    match try!(self.parse_step(source)) {
                        Some(next) => steps.push(next),
                        None => return Err(TrailingSlash),
                    }
                }

                Ok(Some(expression::Path::new(start_point, steps)))
            },
            None => Ok(None),
        }
    }

    fn parse_relative_location_path(&self, source: TokenSource<I>) -> ParseResult {
        let start_point = box expression::ContextNode;
        self.parse_relative_location_path_raw(source, start_point)
    }

    fn parse_absolute_location_path(&self, source: TokenSource<I>) -> ParseResult {
        if source.next_token_is(&Token::Slash) {
            try!(source.consume(&Token::Slash));

            let start_point = box expression::RootNode;
            match try!(self.parse_relative_location_path_raw(source, start_point)) {
                Some(expr) => Ok(Some(expr)),
                None => Ok(Some(box expression::RootNode as SubExpression)),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_location_path(&self, source: TokenSource<I>) -> ParseResult {
        let mut child_parses = vec![
            |source: TokenSource<I>| self.parse_relative_location_path(source),
            |source: TokenSource<I>| self.parse_absolute_location_path(source),
        ];

        first_matching_rule(&mut child_parses, source)
    }

    fn parse_filter_expression(&self, source: TokenSource<I>) -> ParseResult {
        match try!(self.parse_primary_expression(source)) {
            Some(expr) => {
                let mut predicates = try!(self.parse_predicates(source));

                Ok(Some(predicates.drain().fold(expr, |expr, pred| {
                    expression::Filter::new(expr, pred)
                })))
            },
            None => Ok(None),
        }
    }

    fn parse_path_expression(&self, source: TokenSource<I>) -> ParseResult {
        let expr = try!(self.parse_location_path(source));
        if expr.is_some() {
            return Ok(expr);
        } // TODO: investigate if this is a pattern

        match try!(self.parse_filter_expression(source)) {
            Some(expr) =>
                if source.next_token_is(&Token::Slash) {
                    try!(source.consume(&Token::Slash));

                    match try!(self.parse_relative_location_path_raw(source, expr)) {
                        Some(expr) => Ok(Some(expr)),
                        None => Err(TrailingSlash),
                    }
                } else {
                    Ok(Some(expr))
                },
            None => Ok(None),
        }
    }

    fn parse_union_expression(&self, source: TokenSource<I>) -> ParseResult {
        let rules = vec![
            BinaryRule { token: Token::Pipe, builder: expression::Union::new }
        ];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_path_expression(source))
    }

    fn parse_unary_expression(&self, source: TokenSource<I>) -> ParseResult {
        let expr = try!(self.parse_union_expression(source));
        if expr.is_some() {
            return Ok(expr);
        }

        if source.next_token_is(&Token::MinusSign) {
            try!(source.consume(&Token::MinusSign));

            let expr = try!(self.parse_unary_expression(source));

            match expr {
                Some(expr) => {
                    let expr: SubExpression = box expression::Negation { expression: expr };
                    Ok(Some(expr))
                },
                None => Err(RightHandSideExpressionMissing),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_multiplicative_expression(&self, source: TokenSource<I>) -> ParseResult {
        let rules = vec![
            BinaryRule { token: Token::Multiply,  builder: expression::Math::multiplication },
            BinaryRule { token: Token::Divide,    builder: expression::Math::division },
            BinaryRule { token: Token::Remainder, builder: expression::Math::remainder }
        ];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_unary_expression(source))
    }

    fn parse_additive_expression(&self, source: TokenSource<I>) -> ParseResult {
        let rules = vec![
            BinaryRule { token: Token::PlusSign,  builder: expression::Math::addition },
            BinaryRule { token: Token::MinusSign, builder: expression::Math::subtraction }
        ];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_multiplicative_expression(source))
    }

    fn parse_relational_expression(&self, source: TokenSource<I>) -> ParseResult {
        let rules = vec![
            BinaryRule { token: Token::LessThan,
                         builder: expression::Relational::less_than },
            BinaryRule { token: Token::LessThanOrEqual,
                         builder: expression::Relational::less_than_or_equal },
            BinaryRule { token: Token::GreaterThan,
                         builder: expression::Relational::greater_than },
            BinaryRule { token: Token::GreaterThanOrEqual,
                         builder: expression::Relational::greater_than_or_equal },
        ];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_additive_expression(source))
    }

    fn parse_equality_expression(&self, source: TokenSource<I>) -> ParseResult {
        let rules = vec![
            BinaryRule { token: Token::Equal,    builder: expression::Equal::new },
            BinaryRule { token: Token::NotEqual, builder: expression::NotEqual::new },
        ];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_relational_expression(source))
    }

    fn parse_and_expression(&self, source: TokenSource<I>) -> ParseResult {
        let rules = vec![
            BinaryRule { token: Token::And, builder: expression::And::new }
        ];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_equality_expression(source))
    }

    fn parse_or_expression(&self, source: TokenSource<I>) -> ParseResult {
        let rules = vec![
            BinaryRule { token: Token::Or, builder: expression::Or::new }
        ];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_and_expression(source))
    }

    fn parse_expression(&self, source: TokenSource<I>) -> ParseResult {
        self.parse_or_expression(source)
    }

    pub fn parse(&self, source: I) -> ParseResult {
        let mut source = source.peekable();

        let expr = try!(self.parse_or_expression(&mut source));

        if source.has_more_tokens() {
            return Err(ExtraUnparsedTokens);
        }

        Ok(expr)
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use std::num::Float;

    use document::Package;
    use document::dom4::{Document,Root,Element,Text};

    use super::super::Value::{Boolean,Number,String,Nodes};
    use super::super::{Functions,Variables,Namespaces};
    use super::super::{Value,EvaluationContext};

    use super::super::nodeset::ToNode;

    use super::super::node_test;
    use super::super::token::{Token,AxisName,NodeTestName};
    use super::super::tokenizer::{TokenResult,TokenizerErr};

    use super::super::function::register_core_functions;

    use super::super::expression::{Expression,SubExpression};

    use super::super::parser::{Parser,ParseResult};
    use super::super::parser::ParseErr::{
        EmptyPredicate,
        ExtraUnparsedTokens,
        RanOutOfInput,
        RightHandSideExpressionMissing,
        TokenizerError,
        TrailingSlash,
        UnexpectedToken,
    };

    macro_rules! tokens(
        ($($e:expr),*) => ({
            // leading _ to allow empty construction without a warning.
            let mut _temp: Vec<TokenResult> = ::std::vec::Vec::new();
            $(_temp.push(Ok($e));)*
            _temp
        });
        ($($e:expr),+,) => (tokens!($($e),+))
    );

    fn name_test(local_part: &str) -> Token {
        Token::NameTest(node_test::NameTest {
            prefix: None,
            local_part: local_part.to_string()
        })
    }

    trait ApproxEq {
        fn is_approx_eq(&self, other: &Self) -> bool;
    }

    impl ApproxEq for f64 {
        fn is_approx_eq(&self, other: &f64) -> bool {
            (*self - *other).abs() < 1.0e-6
        }
    }

    impl<'d> ApproxEq for Value<'d> {
        fn is_approx_eq(&self, other: &Value<'d>) -> bool {
            match (self, other) {
                (&Number(ref x), &Number(ref y)) => x.is_approx_eq(y),
                _ => panic!("It's nonsensical to compare these quantities"),
            }
        }
    }

    macro_rules! assert_approx_eq(
        ($a:expr, $b:expr) => ({
            let (a, b) = (&$a, &$b);
            assert!(a.is_approx_eq(b),
                    "{} is not approximately equal to {}", *a, *b);
        })
    );

    struct TestDoc<'d>(Document<'d>);

    impl<'d> TestDoc<'d> {
        fn root(&'d self) -> Root<'d> {
            let &TestDoc(ref doc) = self;
            doc.root()
        }

        fn top_node(&'d self) -> Element<'d> {
            let &TestDoc(ref doc) = self;

            let kids = doc.root().children();
            match kids.len() {
                0 => {
                    let n = doc.create_element("the-top-node");
                    doc.root().append_child(n);
                    n
                },
                1 => {
                    kids[0].element().expect("not an element")
                },
                _ => panic!("Too many top nodes"),
            }
        }

        fn add_top_child(&'d self, name: &str) -> Element<'d> {
            self.add_child(self.top_node(), name)
        }

        fn add_child(&'d self, parent: Element<'d>, name: &str) -> Element<'d> {
            let &TestDoc(ref doc) = self;

            let n = doc.create_element(name);
            parent.append_child(n);
            n
        }

        fn add_text(&'d self, parent: Element<'d>, value: &str) -> Text<'d> {
            let &TestDoc(ref doc) = self;

            let tn = doc.create_text(value);
            parent.append_child(tn);
            tn
        }
    }

    struct Exercise<'d> {
        doc: &'d TestDoc<'d>,
        functions: Functions,
        variables: Variables<'d>,
        namespaces: Namespaces,
        parser: Parser,
    }

    impl<'d> Exercise<'d> {
        fn new(doc: &'d TestDoc<'d>) -> Exercise<'d> {
            let mut functions = HashMap::new();
            register_core_functions(&mut functions);

            Exercise {
                doc: doc,
                functions: functions,
                variables: HashMap::new(),
                namespaces: HashMap::new(),
                parser: Parser::new(),
            }
        }

        fn add_var(&mut self, name: &str, value: Value<'d>) {
            self.variables.insert(name.to_string(), value);
        }

        fn parse_raw(&self, tokens: Vec<TokenResult>) -> ParseResult {
            self.parser.parse(tokens.into_iter())
        }

        fn parse(&self, tokens: Vec<TokenResult>) -> SubExpression {
            self.parse_raw(tokens).unwrap().unwrap()
        }

        fn evaluate(&'d self, expr: &Expression) -> Value<'d> {
            self.evaluate_on(expr, self.doc.top_node())
        }

        fn evaluate_on<N : ToNode<'d>>(&self, expr: &Expression, node: N) -> Value<'d> {
            let node = node.to_node();
            let mut context = EvaluationContext::new(node,
                                                     &self.functions,
                                                     &self.variables,
                                                     &self.namespaces);
            context.next(node);
            expr.evaluate(&context)
        }
    }

    #[test]
    fn parses_string_as_child() {
        let tokens = tokens![name_test("hello")];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let hello = doc.add_top_child("hello");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![hello]), ex.evaluate_on(&*expr, doc.top_node()));
    }

    #[test]
    fn parses_two_strings_as_grandchild() {
        let tokens = tokens![
            name_test("hello"),
            Token::Slash,
            name_test("world")
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let hello = doc.add_top_child("hello");
        let world = doc.add_child(hello, "world");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![world]), ex.evaluate_on(&*expr, doc.top_node()));
    }

    #[test]
    fn parses_self_axis() {
        let tokens = tokens![
            Token::Axis(AxisName::Self),
            name_test("the-top-node")
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![doc.top_node()]), ex.evaluate_on(&*expr, doc.top_node()));
    }

    #[test]
    fn parses_parent_axis() {
        let tokens = tokens![
            Token::Axis(AxisName::Parent),
            name_test("the-top-node")
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let hello = doc.add_top_child("hello");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![doc.top_node()]), ex.evaluate_on(&*expr, hello));
    }

    #[test]
    fn parses_descendant_axis() {
        let tokens = tokens![
            Token::Axis(AxisName::Descendant),
            name_test("two")
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let two = doc.add_child(one, "two");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![two]), ex.evaluate_on(&*expr, doc.top_node()));
    }

    #[test]
    fn parses_descendant_or_self_axis() {
        let tokens = tokens![
            Token::Axis(AxisName::DescendantOrSelf),
            name_test("*")
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let two = doc.add_child(one, "two");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![one, two]), ex.evaluate_on(&*expr, one));
    }

    #[test]
    fn parses_attribute_axis() {
        let tokens = tokens![
            Token::Axis(AxisName::Attribute),
            name_test("*")
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let attr = one.set_attribute_value("hello", "world");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![attr]), ex.evaluate_on(&*expr, one));
    }

    #[test]
    fn parses_child_with_same_name_as_an_axis() {
        let tokens = tokens![name_test("self")];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let element = doc.add_top_child("self");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![element]), ex.evaluate_on(&*expr, doc.top_node()));
    }

    #[test]
    fn parses_node_node_test() {
        let tokens = tokens![Token::NodeTest(NodeTestName::Node)];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let two = doc.add_child(one, "two");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![two]), ex.evaluate_on(&*expr, one));
    }

    #[test]
    fn parses_text_node_test() {
        let tokens = tokens![Token::NodeTest(NodeTestName::Text)];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let text = doc.add_text(one, "text");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![text]), ex.evaluate_on(&*expr, one));
    }

    #[test]
    fn parses_axis_and_node_test() {
        let tokens = tokens![
            Token::Axis(AxisName::Self),
            Token::NodeTest(NodeTestName::Text),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let text = doc.add_text(one, "text");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![text]), ex.evaluate_on(&*expr, text));
    }

    #[test]
    fn numeric_predicate_selects_indexed_node() {
        let tokens = tokens![
            name_test("*"),
            Token::LeftBracket,
            Token::Number(2.0),
            Token::RightBracket
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        doc.add_top_child("first");
        let second = doc.add_top_child("second");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![second]), ex.evaluate_on(&*expr, doc.top_node()));
    }

    #[test]
    fn string_literal() {
        let tokens = tokens![Token::Literal("string".to_string())];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(String("string".to_string()), ex.evaluate(&*expr));
    }

    #[test]
    fn predicate_accepts_any_expression() {
        let tokens = tokens![
            name_test("*"),
            Token::LeftBracket,
            Token::Function("true".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Or,
            Token::Function("false".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::RightBracket
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let first = doc.add_top_child("first");
        let second = doc.add_top_child("second");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![first, second]), ex.evaluate_on(&*expr, doc.top_node()));
    }

    #[test]
    fn true_function_predicate_selects_all_nodes() {
        let tokens = tokens![
            name_test("*"),
            Token::LeftBracket,
            Token::Function("true".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::RightBracket
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let first = doc.add_top_child("first");
        let second = doc.add_top_child("second");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![first, second]), ex.evaluate_on(&*expr, doc.top_node()));
    }

    #[test]
    fn false_function_predicate_selects_no_nodes() {
        let tokens = tokens![
            name_test("*"),
            Token::LeftBracket,
            Token::Function("false".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::RightBracket
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        doc.add_top_child("first");
        doc.add_top_child("second");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![]), ex.evaluate_on(&*expr, doc.top_node()));
    }

    #[test]
    fn multiple_predicates() {
        let tokens = tokens![
            name_test("*"),
            Token::LeftBracket,
            Token::Number(2.0),
            Token::RightBracket,
            Token::LeftBracket,
            Token::Number(1.0),
            Token::RightBracket
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        doc.add_top_child("first");
        let second = doc.add_top_child("second");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![second]), ex.evaluate_on(&*expr, doc.top_node()));
    }

    #[test]
    fn functions_accept_arguments() {
        let tokens = tokens![
            Token::Function("not".to_string()),
            Token::LeftParen,
            Token::Function("true".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::RightParen,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(&*expr));
    }

    #[test]
    fn functions_accept_any_expression_as_an_argument() {
        let tokens = tokens![
            Token::Function("not".to_string()),
            Token::LeftParen,
            Token::Function("true".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Or,
            Token::Function("false".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::RightParen,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(&*expr));
    }

    #[test]
    fn numeric_literal() {
        let tokens = tokens![Token::Number(3.2)];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(3.2), ex.evaluate(&*expr));
    }

    #[test]
    fn addition_of_two_numbers() {
        let tokens = tokens![
            Token::Number(1.1),
            Token::PlusSign,
            Token::Number(2.2)
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(3.3), ex.evaluate(&*expr));
    }

    #[test]
    fn addition_of_multiple_numbers() {
        let tokens = tokens![
            Token::Number(1.1),
            Token::PlusSign,
            Token::Number(2.2),
            Token::PlusSign,
            Token::Number(3.3)
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(6.6), ex.evaluate(&*expr));
    }

    #[test]
    fn subtraction_of_two_numbers() {
        let tokens = tokens![
            Token::Number(1.1),
            Token::MinusSign,
            Token::Number(2.2),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(-1.1), ex.evaluate(&*expr));
    }

    #[test]
    fn additive_expression_is_left_associative() {
        let tokens = tokens![
            Token::Number(1.1),
            Token::MinusSign,
            Token::Number(2.2),
            Token::MinusSign,
            Token::Number(3.3),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(-4.4), ex.evaluate(&*expr));
    }

    #[test]
    fn multiplication_of_two_numbers() {
        let tokens = tokens![
            Token::Number(1.1),
            Token::Multiply,
            Token::Number(2.2),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(2.42), ex.evaluate(&*expr));
    }

    #[test]
    fn division_of_two_numbers() {
        let tokens = tokens![
            Token::Number(7.1),
            Token::Divide,
            Token::Number(0.1),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(71.0), ex.evaluate(&*expr));
    }

    #[test]
    fn remainder_of_two_numbers() {
        let tokens = tokens![
            Token::Number(7.1),
            Token::Remainder,
            Token::Number(3.0),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(1.1), ex.evaluate(&*expr));
    }

    #[test]
    fn unary_negation() {
        let tokens = tokens![
            Token::MinusSign,
            Token::Number(7.2),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(-7.2), ex.evaluate(&*expr));
    }

    #[test]
    fn repeated_unary_negation() {
        let tokens = tokens![
            Token::MinusSign,
            Token::MinusSign,
            Token::MinusSign,
            Token::Number(7.2),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(-7.2), ex.evaluate(&*expr));
    }

    #[test]
    fn top_level_function_call() {
        let tokens = tokens![
            Token::Function("true".to_string()),
            Token::LeftParen,
            Token::RightParen,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(true), ex.evaluate(&*expr));
    }

    #[test]
    fn or_expression() {
        let tokens = tokens![
            Token::Function("true".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Or,
            Token::Function("false".to_string()),
            Token::LeftParen,
            Token::RightParen,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(true), ex.evaluate(&*expr));
    }

    #[test]
    fn and_expression() {
        let tokens = tokens![
            Token::Number(1.2),
            Token::And,
            Token::Number(0.0),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(&*expr));
    }

    #[test]
    fn equality_expression() {
        let tokens = tokens![
            Token::Number(1.2),
            Token::Equal,
            Token::Number(1.1),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(&*expr));
    }

    #[test]
    fn inequality_expression() {
        let tokens = tokens![
            Token::Number(1.2),
            Token::NotEqual,
            Token::Number(1.2),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(&*expr));
    }

    #[test]
    fn less_than_expression() {
        let tokens = tokens![
            Token::Number(1.2),
            Token::LessThan,
            Token::Number(1.2),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(&*expr));
    }

    #[test]
    fn less_than_or_equal_expression() {
        let tokens = tokens![
            Token::Number(1.2),
            Token::LessThanOrEqual,
            Token::Number(1.2),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(true), ex.evaluate(&*expr));
    }

    #[test]
    fn greater_than_expression() {
        let tokens = tokens![
            Token::Number(1.2),
            Token::GreaterThan,
            Token::Number(1.2),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(&*expr));
    }

    #[test]
    fn greater_than_or_equal_expression() {
        let tokens = tokens![
            Token::Number(1.2),
            Token::GreaterThanOrEqual,
            Token::Number(1.2),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(true), ex.evaluate(&*expr));
    }

    #[test]
    fn variable_reference() {
        let tokens = tokens![Token::Variable("variable-name".to_string())];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let mut ex = Exercise::new(&doc);
        ex.add_var("variable-name", Number(12.3));
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(12.3), ex.evaluate(&*expr));
    }

    #[test]
    fn filter_expression() {
        let tokens = tokens![
            Token::Variable("variable".to_string()),
            Token::LeftBracket,
            Token::Number(0.0),
            Token::RightBracket,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let value = nodeset![
            doc.add_top_child("first-node"),
            doc.add_top_child("second-node"),
        ];

        let mut ex = Exercise::new(&doc);
        ex.add_var("variable", Nodes(value));

        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![]), ex.evaluate(&*expr));
    }

    #[test]
    fn filter_expression_and_relative_path() {
        let tokens = tokens![
            Token::Variable("variable".to_string()),
            Token::Slash,
            name_test("child"),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let parent = doc.add_top_child("parent");
        let child = doc.add_child(parent, "child");

        let value = nodeset![parent];

        let mut ex = Exercise::new(&doc);
        ex.add_var("variable", Nodes(value));

        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![child]), ex.evaluate(&*expr));
    }

    #[test]
    fn union_expression() {
        let tokens = tokens![
            Token::Variable("variable1".to_string()),
            Token::Pipe,
            Token::Variable("variable2".to_string()),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let node1 = doc.add_top_child("first-node");
        let node2 = doc.add_top_child("second-node");

        let mut ex = Exercise::new(&doc);
        ex.add_var("variable1", Nodes(nodeset![node1]));
        ex.add_var("variable2", Nodes(nodeset![node2]));

        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![node1, node2]), ex.evaluate(&*expr));
    }

    #[test]
    fn absolute_path_expression() {
        let tokens = tokens![
            Token::Slash,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let node1 = doc.add_top_child("first-node");
        let node2 = doc.add_child(node1, "second-node");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![doc.root()]), ex.evaluate_on(&*expr, node2));
    }

    #[test]
    fn absolute_path_with_child_expression() {
        let tokens = tokens![
            Token::Slash,
            name_test("*"),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let node1 = doc.add_top_child("first-node");
        let node2 = doc.add_child(node1, "second-node");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Nodes(nodeset![doc.top_node()]), ex.evaluate_on(&*expr, node2));
    }

    #[test]
    fn unexpected_token_is_reported_as_an_error() {
        let tokens = tokens![
            Token::Function("does-not-matter".to_string()),
            Token::RightParen
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parser.parse(tokens.into_iter());
        assert_eq!(Some(UnexpectedToken(Token::RightParen)), res.err());
    }

    #[test]
    fn binary_operator_without_right_hand_side_is_reported_as_an_error() {
        let tokens = tokens![
            Token::Literal("left".to_string()),
            Token::And
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parse_raw(tokens);
        assert_eq!(Some(RightHandSideExpressionMissing), res.err());
    }

    #[test]
    fn unary_operator_without_right_hand_side_is_reported_as_an_error() {
        let tokens = tokens![
            Token::MinusSign,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parser.parse(tokens.into_iter());
        assert_eq!(Some(RightHandSideExpressionMissing), res.err());
    }

    #[test]
    fn empty_predicate_is_reported_as_an_error() {
        let tokens = tokens![
            name_test("*"),
            Token::LeftBracket,
            Token::RightBracket,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parse_raw(tokens);
        assert_eq!(Some(EmptyPredicate), res.err());
    }

    #[test]
    fn relative_path_with_trailing_slash_is_reported_as_an_error() {
        let tokens = tokens![
            name_test("*"),
            Token::Slash,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parse_raw(tokens);
        assert_eq!(Some(TrailingSlash), res.err());
    }

    #[test]
    fn filter_expression_with_trailing_slash_is_reported_as_an_error() {
        let tokens = tokens![
            Token::Variable("variable".to_string()),
            Token::Slash,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parse_raw(tokens);
        assert_eq!(Some(TrailingSlash), res.err());
    }

    #[test]
    fn running_out_of_input_is_reported_as_an_error() {
        let tokens = tokens![Token::Function("func".to_string())];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parse_raw(tokens);
        assert_eq!(Some(RanOutOfInput), res.err());
    }

    #[test]
    fn having_extra_tokens_is_reported_as_an_error() {
        let tokens = tokens![Token::LeftBracket];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parse_raw(tokens);
        assert_eq!(Some(ExtraUnparsedTokens), res.err());
    }

    #[test]
    fn a_tokenizer_error_is_reported_as_an_error() {
        let tokens = vec![
            Ok(Token::Function("func".to_string())),
            Err(TokenizerErr::UnableToCreateToken)
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parse_raw(tokens);
        assert_eq!(Some(TokenizerError(TokenizerErr::UnableToCreateToken)), res.err());
    }
}

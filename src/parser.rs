use std::iter::Peekable;

use self::Error::*;

use crate::axis::{Axis, AxisLike, PrincipalNodeType};
use crate::expression::{self, SubExpression};
use crate::node_test::{self, SubNodeTest};
use crate::token::{AxisName, NodeTestName, Token};
use crate::tokenizer::{self, TokenResult};
use crate::Value;

#[allow(missing_copy_implementations)]
pub struct Parser;

impl Parser {
    pub fn new() -> Parser {
        Parser
    }
}

quick_error! {
    #[derive(Debug, Clone, PartialEq)]
    pub enum Error {
        EmptyPredicate {
            description("empty predicate")
        }
        ExtraUnparsedTokens {
            description("extra unparsed tokens")
        }
        RanOutOfInput {
            description("ran out of input")
        }
        RightHandSideExpressionMissing {
            description("right hand side of expression is missing")
        }
        ArgumentMissing {
            description("function argument is missing")
        }
        Tokenizer(err: tokenizer::Error) {
            from()
            cause(err)
            description(err.description())
            display("tokenizer error: {}", err)
        }
        TrailingSlash {
            description("trailing slash")
        }
        UnexpectedToken(token: Token) {
            from()
            description("unexpected token")
            display("unexpected token: {:?}", token)
        }
    }
}

pub type ParseResult = Result<Option<SubExpression>, Error>;

type BinaryExpressionBuilder = fn(SubExpression, SubExpression) -> SubExpression;

struct BinaryRule {
    token: Token,
    builder: BinaryExpressionBuilder,
}

struct LeftAssociativeBinaryParser {
    rules: Vec<BinaryRule>,
}

type TokenSource<'a, I> = &'a mut Peekable<I>;

trait XCompat {
    fn has_more_tokens(&mut self) -> bool;
    fn next_token_is(&mut self, token: &Token) -> bool;
    fn consume(&mut self, token: &Token) -> Result<(), Error>;
}

impl<I> XCompat for Peekable<I>
where
    I: Iterator<Item = TokenResult>,
{
    fn has_more_tokens(&mut self) -> bool {
        self.peek().is_some()
    }

    fn next_token_is(&mut self, token: &Token) -> bool {
        match self.peek() {
            Some(&Ok(ref t)) => t == token,
            _ => false,
        }
    }

    fn consume(&mut self, token: &Token) -> Result<(), Error> {
        match self.next() {
            None => Err(RanOutOfInput),
            Some(Err(x)) => Err(Tokenizer(x)),
            Some(Ok(x)) => {
                if &x == token {
                    Ok(())
                } else {
                    Err(UnexpectedToken(x))
                }
            }
        }
    }
}

/// Similar to `consume`, but can be used when the token carries a
/// single value.
macro_rules! consume_value(
    ($source:expr, Token::$token:ident) => (
        match $source.next() {
            None => return Err(RanOutOfInput),
            Some(Err(x)) => return Err(Tokenizer(x)),
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

impl LeftAssociativeBinaryParser {
    fn new(rules: Vec<BinaryRule>) -> LeftAssociativeBinaryParser {
        LeftAssociativeBinaryParser { rules: rules }
    }

    fn parse<F, I>(&self, source: TokenSource<'_, I>, child_parse: F) -> ParseResult
    where
        F: Fn(TokenSource<'_, I>) -> ParseResult,
        I: Iterator<Item = TokenResult>,
    {
        let left = child_parse(source)?;

        let mut left = match left {
            None => return Ok(None),
            Some(x) => x,
        };

        while source.has_more_tokens() {
            let mut found = false;

            for rule in &self.rules {
                if source.next_token_is(&rule.token) {
                    source.consume(&rule.token)?;

                    let right = child_parse(source)?;

                    let right = match right {
                        None => return Err(RightHandSideExpressionMissing),
                        Some(x) => x,
                    };

                    left = (rule.builder)(left, right);

                    found = true;
                    break;
                }
            }

            if !found {
                break;
            }
        }

        Ok(Some(left))
    }
}

type Rule<'a, I> = dyn Fn(TokenSource<'_, I>) -> ParseResult + 'a;
fn first_matching_rule<I>(child_parses: &[&Rule<'_, I>], source: TokenSource<'_, I>) -> ParseResult
where
    I: Iterator<Item = TokenResult>,
{
    for child_parse in child_parses.iter() {
        let expr = (*child_parse)(source)?;
        if expr.is_some() {
            return Ok(expr);
        }
    }

    Ok(None)
}

impl Parser {
    fn parse_axis<I>(&self, source: TokenSource<'_, I>) -> Result<Axis, Error>
    where
        I: Iterator<Item = TokenResult>,
    {
        if next_token_is!(source, Token::Axis) {
            let name = consume_value!(source, Token::Axis);

            match name {
                AxisName::Child => Ok(Axis::Child),
                AxisName::SelfAxis => Ok(Axis::SelfAxis),
                AxisName::Parent => Ok(Axis::Parent),
                AxisName::Descendant => Ok(Axis::Descendant),
                AxisName::DescendantOrSelf => Ok(Axis::DescendantOrSelf),
                AxisName::Attribute => Ok(Axis::Attribute),
                AxisName::Namespace => Ok(Axis::Namespace),
                AxisName::Ancestor => Ok(Axis::Ancestor),
                AxisName::AncestorOrSelf => Ok(Axis::AncestorOrSelf),
                AxisName::PrecedingSibling => Ok(Axis::PrecedingSibling),
                AxisName::FollowingSibling => Ok(Axis::FollowingSibling),
                AxisName::Preceding => Ok(Axis::Preceding),
                AxisName::Following => Ok(Axis::Following),
            }
        } else {
            Ok(Axis::Child)
        }
    }

    fn parse_node_test<I>(&self, source: TokenSource<'_, I>) -> Result<Option<SubNodeTest>, Error>
    where
        I: Iterator<Item = TokenResult>,
    {
        if next_token_is!(source, Token::NodeTest) {
            let name = consume_value!(source, Token::NodeTest);

            match name {
                NodeTestName::Node => Ok(Some(Box::new(node_test::Node))),
                NodeTestName::Text => Ok(Some(Box::new(node_test::Text))),
                NodeTestName::Comment => Ok(Some(Box::new(node_test::Comment))),
                NodeTestName::ProcessingInstruction(target) => Ok(Some(Box::new(
                    node_test::ProcessingInstruction::new(target),
                ))),
            }
        } else {
            Ok(None)
        }
    }

    fn default_node_test<I>(
        &self,
        source: TokenSource<'_, I>,
        axis: Axis,
    ) -> Result<Option<SubNodeTest>, Error>
    where
        I: Iterator<Item = TokenResult>,
    {
        if next_token_is!(source, Token::NameTest) {
            let name = consume_value!(source, Token::NameTest);

            let test: SubNodeTest = match axis.principal_node_type() {
                PrincipalNodeType::Attribute => Box::new(node_test::Attribute::new(name)),
                PrincipalNodeType::Element => Box::new(node_test::Element::new(name)),
                PrincipalNodeType::Namespace => Box::new(node_test::Namespace::new(name)),
            };

            Ok(Some(test))
        } else {
            Ok(None)
        }
    }

    fn parse_nested_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        if source.next_token_is(&Token::LeftParen) {
            source.consume(&Token::LeftParen)?;
            let result = self.parse_expression(source)?;
            source.consume(&Token::RightParen)?;
            Ok(result)
        } else {
            Ok(None)
        }
    }

    fn parse_variable_reference<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        if next_token_is!(source, Token::Variable) {
            let name = consume_value!(source, Token::Variable);
            Ok(Some(Box::new(expression::Variable { name: name })))
        } else {
            Ok(None)
        }
    }

    fn parse_string_literal<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        if next_token_is!(source, Token::Literal) {
            let value = consume_value!(source, Token::Literal);
            Ok(Some(Box::new(expression::Literal::from(Value::String(
                value,
            )))))
        } else {
            Ok(None)
        }
    }

    fn parse_numeric_literal<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        if next_token_is!(source, Token::Number) {
            let value = consume_value!(source, Token::Number);
            Ok(Some(Box::new(expression::Literal::from(Value::Number(
                value,
            )))))
        } else {
            Ok(None)
        }
    }

    fn parse_function_args_tail<I>(
        &self,
        source: TokenSource<'_, I>,
        mut arguments: Vec<SubExpression>,
    ) -> Result<Vec<SubExpression>, Error>
    where
        I: Iterator<Item = TokenResult>,
    {
        while source.next_token_is(&Token::Comma) {
            source.consume(&Token::Comma)?;

            match self.parse_expression(source)? {
                Some(arg) => arguments.push(arg),
                None => return Err(ArgumentMissing),
            }
        }

        Ok(arguments)
    }

    fn parse_function_args<I>(
        &self,
        source: TokenSource<'_, I>,
    ) -> Result<Vec<SubExpression>, Error>
    where
        I: Iterator<Item = TokenResult>,
    {
        let mut arguments = Vec::new();

        match self.parse_expression(source)? {
            Some(arg) => arguments.push(arg),
            None => return Ok(arguments),
        }

        self.parse_function_args_tail(source, arguments)
    }

    fn parse_function_call<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        if next_token_is!(source, Token::Function) {
            let name = consume_value!(source, Token::Function);

            source.consume(&Token::LeftParen)?;
            let arguments = self.parse_function_args(source)?;
            source.consume(&Token::RightParen)?;

            Ok(Some(Box::new(expression::Function {
                name: name,
                arguments: arguments,
            })))
        } else {
            Ok(None)
        }
    }

    fn parse_primary_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let rules: &[&Rule<'_, I>] = &[
            &|src: TokenSource<'_, I>| self.parse_variable_reference(src),
            &|src: TokenSource<'_, I>| self.parse_nested_expression(src),
            &|src: TokenSource<'_, I>| self.parse_string_literal(src),
            &|src: TokenSource<'_, I>| self.parse_numeric_literal(src),
            &|src: TokenSource<'_, I>| self.parse_function_call(src),
        ];

        first_matching_rule(rules, source)
    }

    fn parse_predicate_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        if source.next_token_is(&Token::LeftBracket) {
            source.consume(&Token::LeftBracket)?;

            match self.parse_expression(source)? {
                Some(predicate) => {
                    source.consume(&Token::RightBracket)?;
                    Ok(Some(predicate))
                }
                None => Err(EmptyPredicate),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_predicates<I>(&self, source: TokenSource<'_, I>) -> Result<Vec<SubExpression>, Error>
    where
        I: Iterator<Item = TokenResult>,
    {
        let mut predicates = Vec::new();

        while let Some(predicate) = self.parse_predicate_expression(source)? {
            predicates.push(predicate)
        }

        Ok(predicates)
    }

    fn parse_step<I>(&self, source: TokenSource<'_, I>) -> Result<Option<expression::Step>, Error>
    where
        I: Iterator<Item = TokenResult>,
    {
        let axis = self.parse_axis(source)?;

        let node_test = match self.parse_node_test(source)? {
            Some(test) => Some(test),
            None => self.default_node_test(source, axis)?,
        };

        let node_test = match node_test {
            Some(test) => test,
            None => return Ok(None),
        };

        let predicates = self.parse_predicates(source)?;

        Ok(Some(expression::Step::new(axis, node_test, predicates)))
    }

    fn parse_relative_location_path_raw<I>(
        &self,
        source: TokenSource<'_, I>,
        start_point: SubExpression,
    ) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        match self.parse_step(source)? {
            Some(step) => {
                let mut steps = vec![step];

                while source.next_token_is(&Token::Slash) {
                    source.consume(&Token::Slash)?;

                    match self.parse_step(source)? {
                        Some(next) => steps.push(next),
                        None => return Err(TrailingSlash),
                    }
                }

                Ok(Some(expression::Path::new(start_point, steps)))
            }
            None => Ok(None),
        }
    }

    fn parse_relative_location_path<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let start_point = Box::new(expression::ContextNode);
        self.parse_relative_location_path_raw(source, start_point)
    }

    fn parse_absolute_location_path<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        if source.next_token_is(&Token::Slash) {
            source.consume(&Token::Slash)?;

            let start_point = Box::new(expression::RootNode);
            match self.parse_relative_location_path_raw(source, start_point)? {
                Some(expr) => Ok(Some(expr)),
                None => Ok(Some(Box::new(expression::RootNode))),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_location_path<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let rules: &[&Rule<'_, I>] = &[
            &|source: TokenSource<'_, I>| self.parse_relative_location_path(source),
            &|source: TokenSource<'_, I>| self.parse_absolute_location_path(source),
        ];

        first_matching_rule(rules, source)
    }

    fn parse_filter_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        match self.parse_primary_expression(source)? {
            Some(expr) => {
                let predicates = self.parse_predicates(source)?;

                Ok(Some(predicates.into_iter().fold(expr, |expr, pred| {
                    expression::Filter::new(expr, pred)
                })))
            }
            None => Ok(None),
        }
    }

    fn parse_path_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let expr = self.parse_location_path(source)?;
        if expr.is_some() {
            return Ok(expr);
        } // TODO: investigate if this is a pattern

        match self.parse_filter_expression(source)? {
            Some(expr) => {
                if source.next_token_is(&Token::Slash) {
                    source.consume(&Token::Slash)?;

                    match self.parse_relative_location_path_raw(source, expr)? {
                        Some(expr) => Ok(Some(expr)),
                        None => Err(TrailingSlash),
                    }
                } else {
                    Ok(Some(expr))
                }
            }
            None => Ok(None),
        }
    }

    fn parse_union_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let rules = vec![BinaryRule {
            token: Token::Pipe,
            builder: expression::Union::new,
        }];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_path_expression(source))
    }

    fn parse_unary_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let expr = self.parse_union_expression(source)?;
        if expr.is_some() {
            return Ok(expr);
        }

        if source.next_token_is(&Token::MinusSign) {
            source.consume(&Token::MinusSign)?;

            let expr = self.parse_unary_expression(source)?;

            match expr {
                Some(expr) => {
                    let expr: SubExpression = Box::new(expression::Negation { expression: expr });
                    Ok(Some(expr))
                }
                None => Err(RightHandSideExpressionMissing),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_multiplicative_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let rules = vec![
            BinaryRule {
                token: Token::Multiply,
                builder: expression::Math::multiplication,
            },
            BinaryRule {
                token: Token::Divide,
                builder: expression::Math::division,
            },
            BinaryRule {
                token: Token::Remainder,
                builder: expression::Math::remainder,
            },
        ];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_unary_expression(source))
    }

    fn parse_additive_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let rules = vec![
            BinaryRule {
                token: Token::PlusSign,
                builder: expression::Math::addition,
            },
            BinaryRule {
                token: Token::MinusSign,
                builder: expression::Math::subtraction,
            },
        ];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| {
            self.parse_multiplicative_expression(source)
        })
    }

    fn parse_relational_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let rules = vec![
            BinaryRule {
                token: Token::LessThan,
                builder: expression::Relational::less_than,
            },
            BinaryRule {
                token: Token::LessThanOrEqual,
                builder: expression::Relational::less_than_or_equal,
            },
            BinaryRule {
                token: Token::GreaterThan,
                builder: expression::Relational::greater_than,
            },
            BinaryRule {
                token: Token::GreaterThanOrEqual,
                builder: expression::Relational::greater_than_or_equal,
            },
        ];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_additive_expression(source))
    }

    fn parse_equality_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let rules = vec![
            BinaryRule {
                token: Token::Equal,
                builder: expression::Equal::new,
            },
            BinaryRule {
                token: Token::NotEqual,
                builder: expression::NotEqual::new,
            },
        ];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_relational_expression(source))
    }

    fn parse_and_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let rules = vec![BinaryRule {
            token: Token::And,
            builder: expression::And::new,
        }];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_equality_expression(source))
    }

    fn parse_or_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let rules = vec![BinaryRule {
            token: Token::Or,
            builder: expression::Or::new,
        }];

        let parser = LeftAssociativeBinaryParser::new(rules);
        parser.parse(source, |source| self.parse_and_expression(source))
    }

    fn parse_expression<I>(&self, source: TokenSource<'_, I>) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        self.parse_or_expression(source)
    }

    pub fn parse<I>(&self, source: I) -> ParseResult
    where
        I: Iterator<Item = TokenResult>,
    {
        let mut source = source.peekable();

        let expr = self.parse_or_expression(&mut source)?;

        if source.has_more_tokens() {
            return Err(ExtraUnparsedTokens);
        }

        Ok(expr)
    }
}

#[cfg(test)]
mod test {
    use std::borrow::ToOwned;

    use sxd_document::dom::{self, Document, Element, Root, Text};
    use sxd_document::Package;

    use crate::context::{self, Context};
    use crate::expression::{Expression, SubExpression};
    use crate::node_test;
    use crate::nodeset::Node;
    use crate::token::{AxisName, NodeTestName, Token};
    use crate::tokenizer::{self, TokenResult};
    use crate::Value;
    use crate::Value::{Boolean, Number, String};

    use super::Error::*;
    use super::{ParseResult, Parser};

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
            local_part: local_part.to_owned(),
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
                    "{:?} is not approximately equal to {:?}", *a, *b);
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
                }
                1 => kids[0].element().expect("not an element"),
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

        fn add_comment(&'d self, parent: Element<'d>, value: &str) -> dom::Comment<'d> {
            let cn = self.0.create_comment(value);
            parent.append_child(cn);
            cn
        }

        fn add_processing_instruction(
            &'d self,
            parent: Element<'d>,
            name: &str,
            value: Option<&str>,
        ) -> dom::ProcessingInstruction<'d> {
            let pi = self.0.create_processing_instruction(name, value);
            parent.append_child(pi);
            pi
        }
    }

    struct Exercise<'d> {
        doc: &'d TestDoc<'d>,
        context: Context<'d>,
        parser: Parser,
    }

    impl<'d> Exercise<'d> {
        fn new(doc: &'d TestDoc<'d>) -> Exercise<'d> {
            Exercise {
                doc: doc,
                context: Context::new(),
                parser: Parser::new(),
            }
        }

        fn parse_raw(&self, tokens: Vec<TokenResult>) -> ParseResult {
            self.parser.parse(tokens.into_iter())
        }

        fn parse(&self, tokens: Vec<TokenResult>) -> SubExpression {
            self.parse_raw(tokens)
                .expect("Unable to parse expression")
                .expect("Expression missing")
        }

        fn evaluate<E>(&self, expr: E) -> Value<'d>
        where
            E: Expression,
        {
            self.evaluate_on(expr, self.doc.top_node())
        }

        fn evaluate_on<E, N>(&self, expr: E, node: N) -> Value<'d>
        where
            E: Expression,
            N: Into<Node<'d>>,
        {
            let context = context::Evaluation::new(&self.context, node.into());
            expr.evaluate(&context)
                .expect("Unable to evaluate expression")
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

        assert_eq!(nodeset![hello], ex.evaluate_on(expr, doc.top_node()));
    }

    #[test]
    fn parses_two_strings_as_grandchild() {
        let tokens = tokens![name_test("hello"), Token::Slash, name_test("world")];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let hello = doc.add_top_child("hello");
        let world = doc.add_child(hello, "world");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(nodeset![world], ex.evaluate_on(expr, doc.top_node()));
    }

    #[test]
    fn parses_self_axis() {
        let tokens = tokens![Token::Axis(AxisName::SelfAxis), name_test("the-top-node")];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(
            nodeset![doc.top_node()],
            ex.evaluate_on(expr, doc.top_node())
        );
    }

    #[test]
    fn parses_parent_axis() {
        let tokens = tokens![Token::Axis(AxisName::Parent), name_test("the-top-node")];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let hello = doc.add_top_child("hello");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(nodeset![doc.top_node()], ex.evaluate_on(expr, hello));
    }

    #[test]
    fn parses_child_axis() {
        let tokens = tokens![Token::Axis(AxisName::Child), name_test("*")];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let two = doc.add_child(one, "two");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(nodeset![two], ex.evaluate_on(expr, one));
    }

    #[test]
    fn parses_descendant_axis() {
        let tokens = tokens![Token::Axis(AxisName::Descendant), name_test("two")];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let two = doc.add_child(one, "two");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(nodeset![two], ex.evaluate_on(expr, doc.top_node()));
    }

    #[test]
    fn parses_descendant_or_self_axis() {
        let tokens = tokens![Token::Axis(AxisName::DescendantOrSelf), name_test("*")];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let two = doc.add_child(one, "two");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(nodeset![one, two], ex.evaluate_on(expr, one));
    }

    #[test]
    fn parses_attribute_axis() {
        let tokens = tokens![Token::Axis(AxisName::Attribute), name_test("*")];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let attr = one.set_attribute_value("hello", "world");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(nodeset![attr], ex.evaluate_on(expr, one));
    }

    #[test]
    fn parses_namespace_axis() {
        let tokens = tokens![Token::Axis(AxisName::Namespace), name_test("prefix")];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        one.register_prefix("prefix", "uri");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        match ex.evaluate_on(expr, one) {
            Value::Nodeset(ns) => {
                assert_eq!(1, ns.size());
                match ns.into_iter().next() {
                    Some(Node::Namespace(ns)) => {
                        assert_eq!("prefix", ns.prefix());
                        assert_eq!("uri", ns.uri());
                    }
                    _ => panic!("Not a namespace node"),
                }
            }
            _ => panic!("Did not get the namespace node"),
        }
    }

    #[test]
    fn parses_child_with_same_name_as_an_axis() {
        let tokens = tokens![name_test("self")];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let element = doc.add_top_child("self");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(nodeset![element], ex.evaluate_on(expr, doc.top_node()));
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

        assert_eq!(nodeset![two], ex.evaluate_on(expr, one));
    }

    #[test]
    fn parses_comment_node_test() {
        let tokens = tokens![Token::NodeTest(NodeTestName::Comment)];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let two = doc.add_comment(one, "two");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(nodeset![two], ex.evaluate_on(expr, one));
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

        assert_eq!(nodeset![text], ex.evaluate_on(expr, one));
    }

    #[test]
    fn parses_processing_instruction_node_test() {
        let tokens = tokens![Token::NodeTest(NodeTestName::ProcessingInstruction(Some(
            "name".to_owned()
        )))];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let two = doc.add_processing_instruction(one, "name", None);

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(nodeset![two], ex.evaluate_on(expr, one));
    }

    #[test]
    fn parses_axis_and_node_test() {
        let tokens = tokens![
            Token::Axis(AxisName::SelfAxis),
            Token::NodeTest(NodeTestName::Text),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let one = doc.add_top_child("one");
        let text = doc.add_text(one, "text");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(nodeset![text], ex.evaluate_on(expr, text));
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

        assert_eq!(nodeset![second], ex.evaluate_on(expr, doc.top_node()));
    }

    #[test]
    fn string_literal() {
        let tokens = tokens![Token::Literal("string".to_owned())];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(String("string".to_owned()), ex.evaluate(expr));
    }

    #[test]
    fn predicate_accepts_any_expression() {
        let tokens = tokens![
            name_test("*"),
            Token::LeftBracket,
            Token::Function("true".into()),
            Token::LeftParen,
            Token::RightParen,
            Token::Or,
            Token::Function("false".into()),
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

        assert_eq!(
            nodeset![first, second],
            ex.evaluate_on(expr, doc.top_node())
        );
    }

    #[test]
    fn true_function_predicate_selects_all_nodes() {
        let tokens = tokens![
            name_test("*"),
            Token::LeftBracket,
            Token::Function("true".into()),
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

        assert_eq!(
            nodeset![first, second],
            ex.evaluate_on(expr, doc.top_node())
        );
    }

    #[test]
    fn false_function_predicate_selects_no_nodes() {
        let tokens = tokens![
            name_test("*"),
            Token::LeftBracket,
            Token::Function("false".into()),
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

        assert_eq!(nodeset![], ex.evaluate_on(expr, doc.top_node()));
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

        assert_eq!(nodeset![second], ex.evaluate_on(expr, doc.top_node()));
    }

    #[test]
    fn functions_accept_arguments() {
        let tokens = tokens![
            Token::Function("not".into()),
            Token::LeftParen,
            Token::Function("true".into()),
            Token::LeftParen,
            Token::RightParen,
            Token::RightParen,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(expr));
    }

    #[test]
    fn functions_accept_any_expression_as_an_argument() {
        let tokens = tokens![
            Token::Function("not".into()),
            Token::LeftParen,
            Token::Function("true".into()),
            Token::LeftParen,
            Token::RightParen,
            Token::Or,
            Token::Function("false".into()),
            Token::LeftParen,
            Token::RightParen,
            Token::RightParen,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(expr));
    }

    #[test]
    fn numeric_literal() {
        let tokens = tokens![Token::Number(3.2)];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(3.2), ex.evaluate(expr));
    }

    #[test]
    fn addition_of_two_numbers() {
        let tokens = tokens![Token::Number(1.1), Token::PlusSign, Token::Number(2.2)];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(3.3), ex.evaluate(expr));
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

        assert_approx_eq!(Number(6.6), ex.evaluate(expr));
    }

    #[test]
    fn subtraction_of_two_numbers() {
        let tokens = tokens![Token::Number(1.1), Token::MinusSign, Token::Number(2.2),];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(-1.1), ex.evaluate(expr));
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

        assert_approx_eq!(Number(-4.4), ex.evaluate(expr));
    }

    #[test]
    fn multiplication_of_two_numbers() {
        let tokens = tokens![Token::Number(1.1), Token::Multiply, Token::Number(2.2),];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(2.42), ex.evaluate(expr));
    }

    #[test]
    fn division_of_two_numbers() {
        let tokens = tokens![Token::Number(7.1), Token::Divide, Token::Number(0.1),];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(71.0), ex.evaluate(expr));
    }

    #[test]
    fn remainder_of_two_numbers() {
        let tokens = tokens![Token::Number(7.1), Token::Remainder, Token::Number(3.0),];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(1.1), ex.evaluate(expr));
    }

    #[test]
    fn unary_negation() {
        let tokens = tokens![Token::MinusSign, Token::Number(7.2),];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(-7.2), ex.evaluate(expr));
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

        assert_approx_eq!(Number(-7.2), ex.evaluate(expr));
    }

    #[test]
    fn top_level_function_call() {
        let tokens = tokens![
            Token::Function("true".into()),
            Token::LeftParen,
            Token::RightParen,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(true), ex.evaluate(expr));
    }

    #[test]
    fn or_expression() {
        let tokens = tokens![
            Token::Function("true".into()),
            Token::LeftParen,
            Token::RightParen,
            Token::Or,
            Token::Function("false".into()),
            Token::LeftParen,
            Token::RightParen,
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(true), ex.evaluate(expr));
    }

    #[test]
    fn and_expression() {
        let tokens = tokens![Token::Number(1.2), Token::And, Token::Number(0.0),];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(expr));
    }

    #[test]
    fn equality_expression() {
        let tokens = tokens![Token::Number(1.2), Token::Equal, Token::Number(1.1),];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(expr));
    }

    #[test]
    fn inequality_expression() {
        let tokens = tokens![Token::Number(1.2), Token::NotEqual, Token::Number(1.2),];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(expr));
    }

    #[test]
    fn less_than_expression() {
        let tokens = tokens![Token::Number(1.2), Token::LessThan, Token::Number(1.2),];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(expr));
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

        assert_eq!(Boolean(true), ex.evaluate(expr));
    }

    #[test]
    fn greater_than_expression() {
        let tokens = tokens![Token::Number(1.2), Token::GreaterThan, Token::Number(1.2),];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(Boolean(false), ex.evaluate(expr));
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

        assert_eq!(Boolean(true), ex.evaluate(expr));
    }

    #[test]
    fn nested_expression() {
        let tokens = tokens![Token::LeftParen, Token::Number(1.1), Token::RightParen,];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(1.1), ex.evaluate(expr));
    }

    #[test]
    fn variable_reference() {
        let tokens = tokens![Token::Variable("variable-name".into())];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let mut ex = Exercise::new(&doc);
        ex.context.set_variable("variable-name", 12.3);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(12.3), ex.evaluate(expr));
    }

    #[test]
    fn variable_reference_prefixed_name() {
        let tokens = tokens![Token::Variable(("ns", "variable-name").into())];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let mut ex = Exercise::new(&doc);
        ex.context.set_namespace("ns", "uri:vars");
        ex.context.set_variable(("uri:vars", "variable-name"), 12.3);
        let expr = ex.parse(tokens);

        assert_approx_eq!(Number(12.3), ex.evaluate(expr));
    }

    #[test]
    fn filter_expression() {
        let tokens = tokens![
            Token::Variable("variable".into()),
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
        ex.context.set_variable("variable", value);

        let expr = ex.parse(tokens);

        assert_eq!(nodeset![], ex.evaluate(expr));
    }

    #[test]
    fn filter_expression_and_relative_path() {
        let tokens = tokens![
            Token::Variable("variable".into()),
            Token::Slash,
            name_test("child"),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let parent = doc.add_top_child("parent");
        let child = doc.add_child(parent, "child");

        let value = nodeset![parent];

        let mut ex = Exercise::new(&doc);
        ex.context.set_variable("variable", value);

        let expr = ex.parse(tokens);

        assert_eq!(nodeset![child], ex.evaluate(expr));
    }

    #[test]
    fn union_expression() {
        let tokens = tokens![
            Token::Variable("variable1".into()),
            Token::Pipe,
            Token::Variable("variable2".into()),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let node1 = doc.add_top_child("first-node");
        let node2 = doc.add_top_child("second-node");

        let mut ex = Exercise::new(&doc);
        ex.context.set_variable("variable1", nodeset![node1]);
        ex.context.set_variable("variable2", nodeset![node2]);

        let expr = ex.parse(tokens);

        assert_eq!(nodeset![node1, node2], ex.evaluate(expr));
    }

    #[test]
    fn absolute_path_expression() {
        let tokens = tokens![Token::Slash,];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let node1 = doc.add_top_child("first-node");
        let node2 = doc.add_child(node1, "second-node");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(nodeset![doc.root()], ex.evaluate_on(expr, node2));
    }

    #[test]
    fn absolute_path_with_child_expression() {
        let tokens = tokens![Token::Slash, name_test("*"),];

        let package = Package::new();
        let doc = TestDoc(package.as_document());
        let node1 = doc.add_top_child("first-node");
        let node2 = doc.add_child(node1, "second-node");

        let ex = Exercise::new(&doc);
        let expr = ex.parse(tokens);

        assert_eq!(nodeset![doc.top_node()], ex.evaluate_on(expr, node2));
    }

    #[test]
    fn unexpected_token_is_reported_as_an_error() {
        let tokens = tokens![Token::Function("does-not-matter".into()), Token::RightParen];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parser.parse(tokens.into_iter());
        assert_eq!(Some(UnexpectedToken(Token::RightParen)), res.err());
    }

    #[test]
    fn binary_operator_without_right_hand_side_is_reported_as_an_error() {
        let tokens = tokens![Token::Literal("left".to_owned()), Token::And];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parse_raw(tokens);
        assert_eq!(Some(RightHandSideExpressionMissing), res.err());
    }

    #[test]
    fn unary_operator_without_right_hand_side_is_reported_as_an_error() {
        let tokens = tokens![Token::MinusSign,];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parser.parse(tokens.into_iter());
        assert_eq!(Some(RightHandSideExpressionMissing), res.err());
    }

    #[test]
    fn empty_predicate_is_reported_as_an_error() {
        let tokens = tokens![name_test("*"), Token::LeftBracket, Token::RightBracket,];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parse_raw(tokens);
        assert_eq!(Some(EmptyPredicate), res.err());
    }

    #[test]
    fn relative_path_with_trailing_slash_is_reported_as_an_error() {
        let tokens = tokens![name_test("*"), Token::Slash,];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parse_raw(tokens);
        assert_eq!(Some(TrailingSlash), res.err());
    }

    #[test]
    fn filter_expression_with_trailing_slash_is_reported_as_an_error() {
        let tokens = tokens![Token::Variable("variable".into()), Token::Slash,];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parse_raw(tokens);
        assert_eq!(Some(TrailingSlash), res.err());
    }

    #[test]
    fn running_out_of_input_is_reported_as_an_error() {
        let tokens = tokens![Token::Function("func".into())];

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
            Ok(Token::Function("func".into())),
            Err(tokenizer::Error::UnableToCreateToken),
        ];

        let package = Package::new();
        let doc = TestDoc(package.as_document());

        let ex = Exercise::new(&doc);
        let res = ex.parse_raw(tokens);
        assert_eq!(
            Some(Tokenizer(tokenizer::Error::UnableToCreateToken)),
            res.err()
        );
    }
}

use std::string;

use crate::node_test;
use crate::OwnedPrefixedName;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AxisName {
    Ancestor,
    AncestorOrSelf,
    Attribute,
    Child,
    Descendant,
    DescendantOrSelf,
    Following,
    FollowingSibling,
    Namespace,
    Parent,
    Preceding,
    PrecedingSibling,
    SelfAxis,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeTestName {
    Comment,
    Text,
    ProcessingInstruction(Option<string::String>),
    Node,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    And,
    AtSign,
    Comma,
    CurrentNode,
    Divide,
    DoubleSlash,
    Equal,
    GreaterThan,
    GreaterThanOrEqual,
    LeftBracket,
    LeftParen,
    LessThan,
    LessThanOrEqual,
    Literal(string::String),
    MinusSign,
    Multiply,
    NotEqual,
    Number(f64),
    Or,
    ParentNode,
    Pipe,
    PlusSign,
    Remainder,
    RightBracket,
    RightParen,
    Slash,

    // Specializations
    Axis(AxisName),
    Function(OwnedPrefixedName),
    NameTest(node_test::NameTest),
    NodeTest(NodeTestName),
    Variable(OwnedPrefixedName),
}

impl Token {
    pub fn precedes_node_test(&self) -> bool {
        matches!(self, Token::AtSign | Token::Axis(..))
    }

    pub fn precedes_expression(&self) -> bool {
        matches!(self, Token::LeftParen | Token::LeftBracket)
    }

    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            Token::Slash
                | Token::DoubleSlash
                | Token::PlusSign
                | Token::MinusSign
                | Token::Pipe
                | Token::Equal
                | Token::NotEqual
                | Token::LessThan
                | Token::LessThanOrEqual
                | Token::GreaterThan
                | Token::GreaterThanOrEqual
                | Token::And
                | Token::Or
                | Token::Remainder
                | Token::Divide
                | Token::Multiply
        )
    }
}

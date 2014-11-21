use std::string;
use self::XPathToken::*;

#[deriving(PartialEq,Show,Clone)]
pub enum XPathToken {
    And,
    AtSign,
    CurrentNode,
    Divide,
    DollarSign,
    DoubleColon,
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
    PrefixedName(string::String, string::String),
    Remainder,
    RightBracket,
    RightParen,
    Slash,
    String(string::String),

    // Specializations
    Axis(string::String),
    Function(string::String),
    NodeTest(string::String),
}

impl XPathToken {
    pub fn precedes_node_test(& self) -> bool {
        match *self {
            AtSign |
            DoubleColon => true,
            _ => false,
        }
    }

    pub fn precedes_expression(& self) -> bool {
        match *self {
            LeftParen |
            LeftBracket => true,
            _ => false,
        }
    }

    pub fn is_operator(& self) -> bool {
        match *self {
            Slash |
            DoubleSlash |
            PlusSign |
            MinusSign |
            Pipe |
            Equal |
            NotEqual |
            LessThan |
            LessThanOrEqual |
            GreaterThan |
            GreaterThanOrEqual |
            And |
            Or |
            Remainder |
            Divide |
            Multiply => true,
            _ => false,
        }
    }
}

use super::XPathFunction;
use super::XPathEvaluationContext;
use super::Functions;
use super::{XPathValue,Boolean};

struct True;

impl XPathFunction for True {
    fn evaluate(&self,
                _context: &XPathEvaluationContext,
                _args: Vec<XPathValue>) -> XPathValue
    {
        Boolean(true)
    }
}

struct False;

impl XPathFunction for False {
    fn evaluate(&self,
                _context: &XPathEvaluationContext,
                _args: Vec<XPathValue>) -> XPathValue
    {
        Boolean(false)
    }
}

struct Not;

impl XPathFunction for Not {
    fn evaluate(&self,
                _context: &XPathEvaluationContext,
                args: Vec<XPathValue>) -> XPathValue
    {
        // TODO: verify arguments
        let arg = &args[0];
        Boolean(!arg.boolean())
    }
}

pub fn register_core_functions(functions: &mut Functions) {
    functions.insert("true".to_string(), box True);
    functions.insert("false".to_string(), box False);
    functions.insert("not".to_string(), box Not);
}

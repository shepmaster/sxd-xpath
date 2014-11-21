use super::XPathFunction;
use super::XPathEvaluationContext;
use super::Functions;
use super::XPathValue;
use super::XPathValue::Boolean;

struct True;

impl XPathFunction for True {
    fn evaluate<'a, 'd>(&self,
                        _context: &XPathEvaluationContext<'a, 'd>,
                        _args: Vec<XPathValue<'d>>) -> XPathValue<'d>
    {
        Boolean(true)
    }
}

struct False;

impl XPathFunction for False {
    fn evaluate<'a, 'd>(&self,
                        _context: &XPathEvaluationContext<'a, 'd>,
                        _args: Vec<XPathValue<'d>>) -> XPathValue<'d>
    {
        Boolean(false)
    }
}

struct Not;

impl XPathFunction for Not {
    fn evaluate<'a, 'd>(&self,
                        _context: &XPathEvaluationContext<'a, 'd>,
                        args: Vec<XPathValue<'d>>) -> XPathValue<'d>
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

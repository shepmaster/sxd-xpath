use super::Function;
use super::XPathEvaluationContext;
use super::Functions;
use super::Value;
use super::Value::Boolean;

struct True;

impl Function for True {
    fn evaluate<'a, 'd>(&self,
                        _context: &XPathEvaluationContext<'a, 'd>,
                        _args: Vec<Value<'d>>) -> Value<'d>
    {
        Boolean(true)
    }
}

struct False;

impl Function for False {
    fn evaluate<'a, 'd>(&self,
                        _context: &XPathEvaluationContext<'a, 'd>,
                        _args: Vec<Value<'d>>) -> Value<'d>
    {
        Boolean(false)
    }
}

struct Not;

impl Function for Not {
    fn evaluate<'a, 'd>(&self,
                        _context: &XPathEvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Value<'d>
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

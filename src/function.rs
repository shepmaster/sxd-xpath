use super::{Function,EvaluationContext,Functions,Value};

struct Not;

impl Function for Not {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Value<'d>
    {
        // TODO: verify arguments
        let arg = &args[0];
        match arg {
            &Value::Boolean(v) => Value::Boolean(!v),
            _ => panic!("Expected a boolean"),
        }
    }
}

struct True;

impl Function for True {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        _args: Vec<Value<'d>>) -> Value<'d>
    {
        Value::Boolean(true)
    }
}

struct False;

impl Function for False {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        _args: Vec<Value<'d>>) -> Value<'d>
    {
        Value::Boolean(false)
    }
}

pub fn register_core_functions(functions: &mut Functions) {
    functions.insert("not".to_string(), box Not);
    functions.insert("true".to_string(), box True);
    functions.insert("false".to_string(), box False);
}

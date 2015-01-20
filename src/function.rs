use super::{EvaluationContext,Functions,Value};

pub trait Function {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>;
}

#[derive(Copy,Clone,Show,PartialEq,Hash)]
pub enum ArgumentType {
    Nodeset,
    Boolean,
    Number,
    String,
}

#[derive(Copy,Clone,Show,PartialEq,Hash)]
pub enum Error {
    TooManyArguments{ expected: usize, actual: usize },
    NotEnoughArguments{ expected: usize, actual: usize },
    WrongType{ expected: ArgumentType, actual: ArgumentType },
}

impl Error {
    fn wrong_type(actual: &Value, expected: ArgumentType) -> Error {
        let actual = match *actual {
            Value::Nodes(..)   => ArgumentType::Nodeset,
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

fn exact_arg_count<T>(args: &Vec<T>, expected: usize) -> Result<(), Error> {
    let actual = args.len();
    if actual < expected {
        Err(Error::NotEnoughArguments{ expected: expected, actual: actual })
    } else if actual > expected {
        Err(Error::TooManyArguments{ expected: expected, actual: actual })
    } else {
        Ok(())
    }
}

struct Last;

impl Function for Last {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        try!(exact_arg_count(&args, 0));
        Ok(Value::Number(context.size() as f64))
    }
}

struct Position;

impl Function for Position {
    fn evaluate<'a, 'd>(&self,
                        context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        try!(exact_arg_count(&args, 0));
        Ok(Value::Number(context.position() as f64))
    }
}

struct Count;

impl Function for Count {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        try!(exact_arg_count(&args, 1));
        let arg = &args[0];
        match arg {
            &Value::Nodes(ref nodeset) => Ok(Value::Number(nodeset.size() as f64)),
            _ => Err(Error::wrong_type(arg, ArgumentType::Nodeset)),
        }
    }
}

struct Not;

impl Function for Not {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        try!(exact_arg_count(&args, 1));
        let arg = &args[0];
        match arg {
            &Value::Boolean(v) => Ok(Value::Boolean(!v)),
            _ => Err(Error::wrong_type(arg, ArgumentType::Boolean)),
        }
    }
}

struct True;

impl Function for True {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        try!(exact_arg_count(&args, 0));
        Ok(Value::Boolean(true))
    }
}

struct False;

impl Function for False {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        try!(exact_arg_count(&args, 0));
        Ok(Value::Boolean(false))
    }
}

pub fn register_core_functions(functions: &mut Functions) {
    functions.insert("last".to_string(), box Last);
    functions.insert("position".to_string(), box Position);
    functions.insert("count".to_string(), box Count);
    functions.insert("not".to_string(), box Not);
    functions.insert("true".to_string(), box True);
    functions.insert("false".to_string(), box False);
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use document::Package;
    use super::super::{EvaluationContext,Function,Value};
    use super::{Last,Position,Count};

    #[test]
    fn last_returns_context_size() {
        let package = Package::new();
        let doc = package.as_document();

        let functions = HashMap::new();
        let variables = HashMap::new();
        let namespaces = HashMap::new();

        let context = EvaluationContext::new(doc.root(), &functions, &variables, &namespaces);
        let args = vec![];
        let r = Last.evaluate(&context, args);

        assert_eq!(Ok(Value::Number(1.0)), r);
    }

    #[test]
    fn position_returns_context_position() {
        let package = Package::new();
        let doc = package.as_document();

        let functions = HashMap::new();
        let variables = HashMap::new();
        let namespaces = HashMap::new();

        let context = EvaluationContext::new(doc.root(), &functions, &variables, &namespaces);
        let args = vec![];
        let r = Position.evaluate(&context, args);

        assert_eq!(Ok(Value::Number(1.0)), r);
    }

    #[test]
    fn count_counts_nodes_in_nodeset() {
        let package = Package::new();
        let doc = package.as_document();
        let nodeset = nodeset![doc.root()];

        let functions = HashMap::new();
        let variables = HashMap::new();
        let namespaces = HashMap::new();

        let context = EvaluationContext::new(doc.root(), &functions, &variables, &namespaces);
        let args = vec![Value::Nodes(nodeset)];
        let r = Count.evaluate(&context, args);

        assert_eq!(Ok(Value::Number(1.0)), r);
    }
}

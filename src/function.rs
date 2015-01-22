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

fn minimum_arg_count<T>(args: &Vec<T>, minimum: usize) -> Result<(), Error> {
    let actual = args.len();
    if actual < minimum {
        Err(Error::NotEnoughArguments{expected: minimum, actual: actual})
    } else {
        Ok(())
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

fn string_args(args: Vec<Value>) -> Result<Vec<String>, Error> {
    fn string_arg(v: Value) -> Result<String, Error> {
        match v {
            Value::String(s) => Ok(s),
            _ => Err(Error::wrong_type(&v, ArgumentType::String)),
        }
    }

    args.into_iter().map(string_arg).collect()
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

struct Concat;

impl Function for Concat {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        try!(minimum_arg_count(&args, 2));
        let args = try!(string_args(args));
        Ok(Value::String(args.concat()))
    }
}

struct StartsWith;

impl Function for StartsWith {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        try!(exact_arg_count(&args, 2));
        let args = try!(string_args(args));
        let v = args[0].starts_with(&*args[1]);
        Ok(Value::Boolean(v))
    }
}

struct Contains;

impl Function for Contains {
    fn evaluate<'a, 'd>(&self,
                        _context: &EvaluationContext<'a, 'd>,
                        args: Vec<Value<'d>>) -> Result<Value<'d>, Error>
    {
        try!(exact_arg_count(&args, 2));
        let args = try!(string_args(args));
        let v = args[0].contains(&*args[1]);
        Ok(Value::Boolean(v))
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
    functions.insert("concat".to_string(), box Concat);
    functions.insert("starts-with".to_string(), box StartsWith);
    functions.insert("contains".to_string(), box Contains);
    functions.insert("not".to_string(), box Not);
    functions.insert("true".to_string(), box True);
    functions.insert("false".to_string(), box False);
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use document::Package;
    use super::super::{EvaluationContext,Value,Functions,Variables,Namespaces};
    use super::super::nodeset::ToNode;
    use super::{Function,Error,Last,Position,Count,Concat,StartsWith,Contains};

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
            where N: ToNode<'d>,
                  F: Function
        {
            let context = EvaluationContext::new(
                node, &self.functions, &self.variables, &self.namespaces
            );
            f.evaluate(&context, args)
        }
    }

    #[test]
    fn last_returns_context_size() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let r = setup.evaluate(doc.root(), Last, vec![]);

        assert_eq!(Ok(Value::Number(1.0)), r);
    }

    #[test]
    fn position_returns_context_position() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let r = setup.evaluate(doc.root(), Position, vec![]);

        assert_eq!(Ok(Value::Number(1.0)), r);
    }

    #[test]
    fn count_counts_nodes_in_nodeset() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let nodeset = nodeset![doc.root()];
        let r = setup.evaluate(doc.root(), Count, vec![Value::Nodes(nodeset)]);

        assert_eq!(Ok(Value::Number(1.0)), r);
    }

    #[test]
    fn concat_combines_strings() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let args = vec![Value::String("hello".to_string()),
                        Value::String(" ".to_string()),
                        Value::String("world".to_string())];
        let r = setup.evaluate(doc.root(), Concat, args);

        assert_eq!(Ok(Value::String("hello world".to_string())), r);
    }

    #[test]
    fn starts_with_checks_prefixes() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let args = vec![Value::String("hello".to_string()),
                        Value::String("he".to_string())];
        let r = setup.evaluate(doc.root(), StartsWith, args);

        assert_eq!(Ok(Value::Boolean(true)), r);
    }

    #[test]
    fn contains_looks_for_a_needle() {
        let package = Package::new();
        let doc = package.as_document();
        let setup = Setup::new();

        let args = vec![Value::String("astronomer".to_string()),
                        Value::String("ono".to_string())];
        let r = setup.evaluate(doc.root(), Contains, args);

        assert_eq!(Ok(Value::Boolean(true)), r);
    }
}

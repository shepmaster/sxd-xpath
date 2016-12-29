use std::collections::HashMap;
use std::iter;

use ::Value;
use ::nodeset::{self, Node, Nodeset};
use ::function;

/// A mapping of names to XPath functions.
type Functions = HashMap<String, Box<function::Function + 'static>>;
/// A mapping of names to XPath variables.
type Variables<'d> = HashMap<String, Value<'d>>;
/// A mapping of namespace prefixes to namespace URIs.
type Namespaces = HashMap<String, String>;

/// Reusable parts of the context which are distinct from the context node.
pub struct Core<'d> {
    functions: Functions,
    variables: Variables<'d>,
    namespaces: Namespaces,
}

impl<'d> Core<'d> {
    /// Registers the core XPath 1.0 functions.
    pub fn new() -> Self {
        let mut context = Self::without_core_functions();
        function::register_core_functions(&mut context);
        context
    }

    /// No functions, variables or namespaces will be defined.
    pub fn without_core_functions() -> Self {
        Core {
            functions: Default::default(),
            variables: Default::default(),
            namespaces: Default::default(),
        }
    }

    /// Register a function within the context
    pub fn set_function<F>(&mut self, name: &str, function: F)
        where F: function::Function + 'static,
    {
        self.functions.insert(name.into(), Box::new(function));
    }

    /// Register a variable within the context
    pub fn set_variable(&mut self, name: &str, value: Value<'d>) {
        self.variables.insert(name.into(), value);
    }

    /// Register a namespace prefix within the context
    pub fn set_namespace(&mut self, prefix: &str, uri: &str) {
        self.namespaces.insert(prefix.into(), uri.into());
    }

    /// Convert into a complete `Context`
    pub fn with_context_node<N>(self, context_node: N) -> Context<'d>
        where N: Into<Node<'d>>,
    {
        Context {
            node: context_node.into(),
            core: self,
        }
    }

    /// Enhance this context with a context node
    pub fn borrow_with_context_node<'a, N>(&'a self, context_node: N) -> Borrowed<'a, 'd>
        where N: Into<Node<'d>>,
    {
        Borrowed {
            node: context_node.into(),
            core: self,
        }
    }
}

impl<'d> Default for Core<'d> {
    fn default() -> Self {
        Core::new()
    }
}

/// Contains the context in which XPath expressions are executed. The
/// context contains functions, variables, namespaces and the context
/// node.
///
/// ### Examples
///
/// A complete example showing all optional settings.
///
/// ```
/// extern crate sxd_document;
/// extern crate sxd_xpath;
///
/// use std::collections::HashMap;
/// use sxd_document::parser;
/// use sxd_xpath::{Factory, Value};
/// use sxd_xpath::context::{self, Context};
/// use sxd_xpath::function::{self, Function};
///
/// struct Sigmoid;
/// impl Function for Sigmoid {
///     fn evaluate<'a, 'd>(&self,
///                         _context: &context::Evaluation<'a, 'd>,
///                         args: Vec<Value<'d>>)
///                         -> Result<Value<'d>, function::Error>
///     {
///         let mut args = function::Args(args);
///         args.exactly(1)?;
///         let val = args.pop_number()?;
///
///         let computed = (1.0 + (-val).exp()).recip();
///
///         Ok(Value::Number(computed))
///     }
/// }
///
/// fn main() {
///     let package = parser::parse("<thing xmlns:ns0='net:brain' ns0:bonus='1' />")
///         .expect("failed to parse XML");
///     let document = package.as_document();
///     let node = document.root().children()[0];
///
///     let mut context = Context::new(node);
///     context.set_function("sigmoid", Sigmoid);
///     context.set_variable("t", Value::Number(2.0));
///     context.set_namespace("neural", "net:brain");
///
///     let xpath = "sigmoid(@neural:bonus + $t)";
///
///     let factory = Factory::new();
///     let xpath = factory.build(xpath).expect("Could not compile XPath");
///     let xpath = xpath.expect("No XPath was compiled");
///
///     let value = xpath.evaluate(&context).expect("XPath evaluation failed");
///
///     assert_eq!(0.952, (value.number() * 1000.0).trunc() / 1000.0);
/// }
/// ```
///
/// Note that we are using a custom function (`sigmoid`), a variable
/// (`$t`), a namespace (`neural:`), and the current node is not the
/// root of the tree but the top-most element.
///
pub struct Context<'d> {
    node: Node<'d>,
    core: Core<'d>,
}

impl<'d> Context<'d> {
    /// Registers the core XPath 1.0 function but no variables or namespaces.
    pub fn new<N>(context_node: N) -> Self
        where N: Into<Node<'d>>,
    {
        Core::new().with_context_node(context_node)
    }

    /// Updates the context node.
    pub fn with_context_node<N>(&mut self, context_node: N)
        where N: Into<Node<'d>>,
    {
        self.node = context_node.into();
    }

    /// Register a function within the context
    pub fn set_function<F>(&mut self, name: &str, function: F)
        where F: function::Function + 'static,
    {
        self.core.set_function(name, function);
    }

    /// Register a variable within the context
    pub fn set_variable(&mut self, name: &str, value: Value<'d>) {
        self.core.set_variable(name, value);
    }

    /// Register a namespace prefix within the context
    pub fn set_namespace(&mut self, prefix: &str, uri: &str) {
        self.core.set_namespace(prefix, uri);
    }
}

/// Augments a `Core` context with a context node. Useful when the
/// context outlives the document.
#[derive(Copy, Clone)]
pub struct Borrowed<'a, 'd: 'a> {
    node: Node<'d>,
    core: &'a Core<'d>,
}

impl<'a, 'd> Borrowed<'a, 'd> {
    /// Updates the context node.
    pub fn with_context_node<N>(&mut self, context_node: N)
        where N: Into<Node<'d>>,
    {
        self.node = context_node.into();
    }
}

#[derive(Copy, Clone)]
pub struct Evaluation<'a, 'd : 'a> {
    pub node: Node<'d>,
    functions: &'a Functions,
    variables: &'a Variables<'d>,
    namespaces: &'a Namespaces,
    position: usize,
    size: usize,
}

impl<'a, 'd> Evaluation<'a, 'd> {
    pub fn new(node: Node<'d>,
               functions: &'a Functions,
               variables: &'a Variables<'d>,
               namespaces: &'a Namespaces)
               -> Evaluation<'a, 'd>
    {
        Evaluation {
            node: node,
            functions: functions,
            variables: variables,
            namespaces: namespaces,
            position: 1,
            size: 1,
        }
    }

    pub fn new_context_for<N>(&self, node: N) -> Evaluation<'a, 'd>
        where N: Into<Node<'d>>
    {
        Evaluation {
            node: node.into(),
            .. *self
        }
    }

    pub fn position(&self) -> usize {
        self.position
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn function_for_name(&self, name: &str) -> Option<&'a function::Function> {
        self.functions.get(name).map(AsRef::as_ref)
    }

    pub fn value_of(&self, name: &str) -> Option<&Value<'d>> {
        self.variables.get(name)
    }

    pub fn namespace_for(&self, prefix: &str) -> Option<&str> {
        self.namespaces.get(prefix).map(|ns| &ns[..])
    }

    pub fn predicate_iter(self, nodes: Nodeset<'d>) -> EvaluationPredicateIter<'a, 'd> {
        let sz = nodes.size();
        EvaluationPredicateIter {
            parent: self,
            nodes: nodes.into_iter().enumerate(),
            size: sz,
        }
    }
}

impl<'a, 'd> From<&'a Context<'d>> for Evaluation<'a, 'd> {
    fn from(other: &'a Context<'d>) -> Self {
        Evaluation::new(other.node,
                        &other.core.functions,
                        &other.core.variables,
                        &other.core.namespaces)
    }
}

impl<'a, 'd> From<Borrowed<'a, 'd>> for Evaluation<'a, 'd> {
    fn from(other: Borrowed<'a, 'd>) -> Self {
        Evaluation::new(other.node,
                        &other.core.functions,
                        &other.core.variables,
                        &other.core.namespaces)
    }
}

pub struct EvaluationPredicateIter<'a, 'd: 'a> {
    parent: Evaluation<'a, 'd>,
    nodes: iter::Enumerate<nodeset::IntoIter<'d>>,
    size: usize,
}

impl<'a, 'd> Iterator for EvaluationPredicateIter<'a, 'd> {
    type Item = Evaluation<'a, 'd>;

    fn next(&mut self) -> Option<Evaluation<'a, 'd>> {
        self.nodes.next().map(|(idx, node)| {
            Evaluation {
                node: node,
                position: idx + 1,
                size: self.size,
                .. self.parent
            }
        })
    }
}

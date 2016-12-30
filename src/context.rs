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

/// The context during evaluation of an XPath expression. Used when
/// implementing custom functions.
#[derive(Copy, Clone)]
pub struct Evaluation<'a, 'd : 'a> {
    /// The context node
    pub node: Node<'d>,
    /// The context position
    pub position: usize,
    /// The context size
    pub size: usize,
    functions: &'a Functions,
    variables: &'a Variables<'d>,
    namespaces: &'a Namespaces,
}

impl<'a, 'd> Evaluation<'a, 'd> {
    fn new(node: Node<'d>,
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

    /// Creates a new context node using the provided node
    pub fn new_context_for<N>(&self, node: N) -> Evaluation<'a, 'd>
        where N: Into<Node<'d>>
    {
        Evaluation {
            node: node.into(),
            .. *self
        }
    }

    /// Looks up the function with the given name
    pub fn function_for_name(&self, name: &str) -> Option<&'a function::Function> {
        self.functions.get(name).map(AsRef::as_ref)
    }

    /// Looks up the value of the variable
    pub fn value_of(&self, name: &str) -> Option<&Value<'d>> {
        self.variables.get(name)
    }

    /// Looks up the namespace URI for the given prefix
    pub fn namespace_for(&self, prefix: &str) -> Option<&str> {
        self.namespaces.get(prefix).map(String::as_str)
    }

    /// Yields a new `Evaluation` context for each node in the nodeset.
    pub fn new_contexts_for(self, nodes: Nodeset<'d>) -> EvaluationNodesetIter<'a, 'd> {
        let sz = nodes.size();
        EvaluationNodesetIter {
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

/// An iterator for the contexts of each node in a nodeset
pub struct EvaluationNodesetIter<'a, 'd: 'a> {
    parent: Evaluation<'a, 'd>,
    nodes: iter::Enumerate<nodeset::IntoIter<'d>>,
    size: usize,
}

impl<'a, 'd> Iterator for EvaluationNodesetIter<'a, 'd> {
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

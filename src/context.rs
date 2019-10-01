//! Support for the various types of contexts before and during XPath
//! evaluation.

use sxd_document::QName;

use std::collections::HashMap;
use std::iter;

use crate::function;
use crate::nodeset::{Node, OrderedNodes};
use crate::{OwnedQName, Value};

/// A mapping of names to XPath functions.
type Functions = HashMap<OwnedQName, Box<dyn function::Function + 'static>>;
/// A mapping of names to XPath variables.
type Variables<'d> = HashMap<OwnedQName, Value<'d>>;
/// A mapping of namespace prefixes to namespace URIs.
type Namespaces = HashMap<String, String>;

/// Contains the context in which XPath expressions are executed. The
/// context contains functions, variables, and namespace mappings.
///
/// ### Examples
///
/// A complete example showing all optional settings.
///
/// ```
/// use std::collections::HashMap;
/// use sxd_document::parser;
/// use sxd_xpath::{Factory, Context, Value};
/// use sxd_xpath::{context, function};
///
/// struct Sigmoid;
/// impl function::Function for Sigmoid {
///     fn evaluate<'c, 'd>(&self,
///                         _context: &context::Evaluation<'c, 'd>,
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
///     let mut context = Context::new();
///     context.set_function("sigmoid", Sigmoid);
///     context.set_variable("t", 2.0);
///     context.set_namespace("neural", "net:brain");
///
///     let xpath = "sigmoid(@neural:bonus + $t)";
///
///     let factory = Factory::new();
///     let xpath = factory.build(xpath).expect("Could not compile XPath");
///     let xpath = xpath.expect("No XPath was compiled");
///
///     let value = xpath.evaluate(&context, node).expect("XPath evaluation failed");
///
///     assert_eq!(0.952, (value.number() * 1000.0).trunc() / 1000.0);
/// }
/// ```
///
/// Note that we are using a custom function (`sigmoid`), a variable
/// (`$t`), and a namespace (`neural:`). The current node is passed to
/// the `evaluate` method and is not the root of the tree but the
/// top-most element.
///
pub struct Context<'d> {
    functions: Functions,
    variables: Variables<'d>,
    namespaces: Namespaces,
}

impl<'d> Context<'d> {
    /// Registers the core XPath 1.0 functions.
    pub fn new() -> Self {
        let mut context = Self::without_core_functions();
        function::register_core_functions(&mut context);
        context
    }

    /// No functions, variables or namespaces will be defined.
    pub fn without_core_functions() -> Self {
        Context {
            functions: Default::default(),
            variables: Default::default(),
            namespaces: Default::default(),
        }
    }

    /// Register a function within the context
    pub fn set_function<N, F>(&mut self, name: N, function: F)
    where
        N: Into<OwnedQName>,
        F: function::Function + 'static,
    {
        self.functions.insert(name.into(), Box::new(function));
    }

    /// Register a variable within the context
    pub fn set_variable<N, V>(&mut self, name: N, value: V)
    where
        N: Into<OwnedQName>,
        V: Into<Value<'d>>,
    {
        self.variables.insert(name.into(), value.into());
    }

    /// Register a namespace prefix within the context
    pub fn set_namespace(&mut self, prefix: &str, uri: &str) {
        self.namespaces.insert(prefix.into(), uri.into());
    }
}

impl<'d> Default for Context<'d> {
    fn default() -> Self {
        Context::new()
    }
}

/// The context during evaluation of an XPath expression.
///
/// Clients of this library will use this when implementing custom
/// functions.
///
/// # Lifetimes
///
/// We track two separate lifetimes: that of the user-provided context
/// (`'c`) and that of the document (`'d`). This allows the
/// user-provided context to live shorter than the document.
#[derive(Copy, Clone)]
pub struct Evaluation<'c, 'd> {
    /// The context node
    pub node: Node<'d>,
    /// The context position
    pub position: usize,
    /// The context size
    pub size: usize,
    functions: &'c Functions,
    variables: &'c Variables<'d>,
    namespaces: &'c Namespaces,
}

impl<'c, 'd> Evaluation<'c, 'd> {
    /// Prepares the context used while evaluating the XPath expression
    pub fn new(context: &'c Context<'d>, node: Node<'d>) -> Evaluation<'c, 'd> {
        Evaluation {
            node: node,
            functions: &context.functions,
            variables: &context.variables,
            namespaces: &context.namespaces,
            position: 1,
            size: 1,
        }
    }

    /// Creates a new context node using the provided node
    pub fn new_context_for<N>(&self, node: N) -> Evaluation<'c, 'd>
    where
        N: Into<Node<'d>>,
    {
        Evaluation {
            node: node.into(),
            ..*self
        }
    }

    /// Looks up the function with the given name
    pub fn function_for_name(&self, name: QName<'_>) -> Option<&'c dyn function::Function> {
        // FIXME: remove allocation
        let name = name.into();
        self.functions.get(&name).map(AsRef::as_ref)
    }

    /// Looks up the value of the variable
    pub fn value_of(&self, name: QName<'_>) -> Option<&Value<'d>> {
        // FIXME: remove allocation
        let name = name.into();
        self.variables.get(&name)
    }

    /// Looks up the namespace URI for the given prefix
    pub fn namespace_for(&self, prefix: &str) -> Option<&str> {
        self.namespaces.get(prefix).map(String::as_str)
    }

    /// Yields a new `Evaluation` context for each node in the nodeset.
    pub fn new_contexts_for(self, nodes: OrderedNodes<'d>) -> EvaluationNodesetIter<'c, 'd> {
        let sz = nodes.size();
        EvaluationNodesetIter {
            parent: self,
            nodes: Vec::from(nodes).into_iter().enumerate(),
            size: sz,
        }
    }
}

/// An iterator for the contexts of each node in a nodeset
pub struct EvaluationNodesetIter<'c, 'd> {
    parent: Evaluation<'c, 'd>,
    nodes: iter::Enumerate<::std::vec::IntoIter<Node<'d>>>,
    size: usize,
}

impl<'c, 'd> Iterator for EvaluationNodesetIter<'c, 'd> {
    type Item = Evaluation<'c, 'd>;

    fn next(&mut self) -> Option<Evaluation<'c, 'd>> {
        self.nodes.next().map(|(idx, node)| Evaluation {
            node: node,
            position: idx + 1,
            size: self.size,
            ..self.parent
        })
    }
}

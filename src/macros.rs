/// Convenience constructor for a nodeset
#[macro_export]
macro_rules! nodeset(
    ($($e:expr),*) => ({
        // leading _ to allow empty construction without a warning.
        let mut _temp = $crate::nodeset::Nodeset::new();
        $(_temp.add($e);)*
        _temp
    });
    ($($e:expr),+,) => (nodeset!($($e),+))
);


/// Convenience constructor for an OrderedNodes
macro_rules! ordered_nodes {
    ( $($val:expr,)* ) => {
        $crate::nodeset::OrderedNodes::from(vec![
            $( $crate::nodeset::Node::from($val), )*
        ])
    };
    ( $($val:expr),* ) => {
        ordered_nodes![$($val, )*]
    };
}

use nom::types::CompleteStr;

use ast::expression::core::Expression;
use ast::helpers::lo_name;

named!(pub access_function<CompleteStr, Expression>,
    do_parse!(
        char!('.') >>
        name: lo_name >>
        (Expression::AccessFunction(name))
    )
);

#[cfg(test)]
mod tests {

    use ast::expression::*;
    use nom::types::CompleteStr;

    #[test]
    fn simple_access_function() {
        assert_eq!(
            access_function(CompleteStr(".property")),
            Ok((
                CompleteStr(""),
                Expression::AccessFunction("property".to_string())
            ))
        );
    }
}

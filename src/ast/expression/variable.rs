use nom::types::CompleteStr;

use ast::expression::core::Expression;
use ast::helpers::lo_name;

named!(pub variable<CompleteStr, Expression>,
  map!(
    map!(lo_name, |v| vec!(v)),
    Expression::Variable
  )
);

#[cfg(test)]
mod tests {

    use ast::expression::*;
    use nom::types::CompleteStr;

    #[test]
    fn simple_variable() {
        assert_eq!(
            variable(CompleteStr("abc")),
            Ok((
                CompleteStr(""),
                Expression::Variable(vec!["abc".to_string()])
            ))
        );
    }

    #[test]
    fn simple_variable_with_digits() {
        assert_eq!(
            variable(CompleteStr("abc123")),
            Ok((
                CompleteStr(""),
                Expression::Variable(vec!["abc123".to_string()])
            ))
        );
    }

    #[test]
    fn single_letter_variable() {
        assert_eq!(
            variable(CompleteStr("a")),
            Ok((CompleteStr(""), Expression::Variable(vec!["a".to_string()])))
        );
    }

    #[test]
    fn invalid_variable_1() {
        assert!(variable(CompleteStr("Abc")).is_err());
    }

    #[test]
    fn invalid_variable_2() {
        assert!(variable(CompleteStr("3bc")).is_err());
    }
}

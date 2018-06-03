use nom::types::CompleteStr;

use ast::expression::core::Expression;
use ast::helpers::{lo_name, up_name};

named!(lower_case<CompleteStr, Expression>,
  map!(
    lo_name,
    |v| Expression::Variable(vec!(v))
  )
);

named!(upper_with_dots<CompleteStr, Expression>,
  map!(
    separated_nonempty_list!(char!('.'), up_name),
    |v| Expression::Variable(v)
  )
);

named!(pub variable<CompleteStr, Expression>,
  alt!(
      lower_case
    | upper_with_dots
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
    fn upper_letter_variable() {
        assert_eq!(
            variable(CompleteStr("Abc")),
            Ok((
                CompleteStr(""),
                Expression::Variable(vec!["Abc".to_string()])
            ))
        );
    }

    #[test]
    fn invalid_variable_2() {
        assert!(variable(CompleteStr("3bc")).is_err());
    }
}

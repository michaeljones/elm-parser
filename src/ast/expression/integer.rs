use ast::expression::core::Expression;

use nom::digit;
use nom::types::CompleteStr;

named!(pub integer<CompleteStr, Expression>,
  do_parse!(
    sign: map!(
      opt!(alt!(char!('+') | char!('-'))),
      |opt| opt.map(|c| c.to_string()).unwrap_or_else(String::new)
    ) >>
    number: digit >>
    (Expression::Integer(sign.to_string() + number.0))
  )
);

#[cfg(test)]
mod tests {

    use ast::expression::*;
    use nom::types::CompleteStr;

    #[test]
    fn int_literal() {
        assert_eq!(
            integer(CompleteStr("101")),
            Ok((CompleteStr(""), Expression::Integer("101".to_string())))
        );
    }

    #[test]
    fn postive_int_literal() {
        assert_eq!(
            integer(CompleteStr("+15")),
            Ok((CompleteStr(""), Expression::Integer("+15".to_string())))
        );
    }

    #[test]
    fn negative_int_literal() {
        assert_eq!(
            integer(CompleteStr("-18")),
            Ok((CompleteStr(""), Expression::Integer("-18".to_string())))
        );
    }
}

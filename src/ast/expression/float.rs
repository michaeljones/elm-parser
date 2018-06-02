use ast::expression::core::Expression;

use nom::digit;
use nom::types::CompleteStr;

named!(pub float<CompleteStr, Expression>,
  do_parse!(
    sign: map!(
        opt!(alt!(char!('+') | char!('-'))),
        |opt| opt.map(|c| c.to_string()).unwrap_or("".to_string())
    ) >>
    start: digit >>
    char!('.') >>
    end: digit >>
    (Expression::Float(sign.to_string() + start.0 + "." + end.0))
  )
);

#[cfg(test)]
mod tests {

    use ast::expression::*;
    use nom::types::CompleteStr;

    #[test]
    fn float_literal() {
        assert_eq!(
            float(CompleteStr("1.01")),
            Ok((CompleteStr(""), Expression::Float("1.01".to_string())))
        );
    }

    #[test]
    fn postive_float_literal() {
        assert_eq!(
            float(CompleteStr("+1.5")),
            Ok((CompleteStr(""), Expression::Float("+1.5".to_string())))
        );
    }

    #[test]
    fn negative_float_literal() {
        assert_eq!(
            float(CompleteStr("-1.8")),
            Ok((CompleteStr(""), Expression::Float("-1.8".to_string())))
        );
    }
}

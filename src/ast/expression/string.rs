use ast::expression::core::Expression;

use nom::types::CompleteStr;

named!(single_string<CompleteStr, String>,
  map!(
    delimited!(
      char!('"'),
      re_matches!(r#"^(\\\\|\\"|[^"\n])*"#),
      char!('"')
    ),
    // There is a match above so we'll have a single entry in the vec
    |v| v[0].to_string()
  )
);

named!(multi_string<CompleteStr, String>,
  map!(
    delimited!(tag!(r#"""""#), re_matches!(r#"^([^"]*)"#), tag!(r#"""""#)),
    // There is a match above so we'll have a single entry in the vec
    |v| v[0].to_string()
  )
);

named!(pub string<CompleteStr, Expression>,
  map!(
    alt!(
        multi_string
      | single_string
    ),
    |c| Expression::String(c.to_string())
  )
);

#[cfg(test)]
mod tests {

    use ast::expression::*;
    use nom::types::CompleteStr;

    #[test]
    fn empty_string() {
        assert_eq!(
            string(CompleteStr(r#""""#)),
            Ok((CompleteStr(""), Expression::String("".to_string())))
        );
    }

    #[test]
    fn simple_string() {
        assert_eq!(
            string(CompleteStr(r#""hello""#)),
            Ok((CompleteStr(""), Expression::String("hello".to_string())))
        );
    }

    #[test]
    fn escaped_string() {
        assert_eq!(
            string(CompleteStr(r#""hello, \"world\"""#)),
            Ok((
                CompleteStr(""),
                Expression::String(r#"hello, \"world\""#.to_string())
            ))
        );
    }

    #[test]
    fn double_escaped_string() {
        assert_eq!(
            string(CompleteStr("\"\\\\\"")),
            Ok((CompleteStr(""), Expression::String("\\\\".to_string())))
        );
    }

    #[test]
    fn empty_triple_quote_string() {
        assert_eq!(
            string(CompleteStr(r#""""""""#)),
            Ok((CompleteStr(""), Expression::String("".to_string())))
        );
    }

    #[test]
    fn simple_multi_line_string() {
        assert_eq!(
            string(CompleteStr("\"\"\"hello\nworld\"\"\"")),
            Ok((
                CompleteStr(""),
                Expression::String("hello\nworld".to_string())
            ))
        );
    }
}

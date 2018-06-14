use ast::statement::core::Statement;

use nom::types::CompleteStr;

named!(single_line_comment<CompleteStr, Statement>,
  map!(
    preceded!(tag!("--"), re_matches!(r"^(.*)$")),
    |v| Statement::Comment(v[0].to_string())
  )
);

named!(multi_line_comment<CompleteStr, Statement>,
  map!(
    preceded!(
        tag!("{-"),
        take_until_and_consume!("-}")
    ),
    |s| Statement::Comment(s.to_string())
  )
);

named!(pub comment<CompleteStr, Statement>,
  alt!(
      single_line_comment
    | multi_line_comment
  )
);

#[cfg(test)]
mod tests {

    use ast::statement::comment::*;
    use ast::statement::core::*;
    use nom::types::CompleteStr;

    #[test]
    fn simple_comment() {
        assert_eq!(
            comment(CompleteStr("-- abc")),
            Ok((CompleteStr(""), Statement::Comment(" abc".to_string())))
        );
    }

    #[test]
    fn comment_with_trailing_white_space() {
        assert_eq!(
            comment(CompleteStr("-- abc  ")),
            Ok((CompleteStr(""), Statement::Comment(" abc  ".to_string())))
        );
    }

    #[test]
    fn open_close_comment() {
        assert_eq!(
            comment(CompleteStr("{- cba -}")),
            Ok((CompleteStr(""), Statement::Comment(" cba ".to_string())))
        );
    }

    #[test]
    fn multi_line_comment() {
        assert_eq!(
            comment(CompleteStr("{- c\nb\na -}")),
            Ok((CompleteStr(""), Statement::Comment(" c\nb\na ".to_string())))
        );
    }
}

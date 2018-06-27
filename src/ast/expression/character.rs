use ast::expression::core::Expression;

use std;

use nom::anychar;
use nom::types::CompleteStr;

named!(character_content<CompleteStr, char>,
  alt!(
      // Look for hex codes starting with \\x
      map_res!(
        do_parse!(
          tag!("\\x") >>
          code: take!(2) >>
          (u8::from_str_radix(code.0, 16))
        ),
        |r: Result<u8, std::num::ParseIntError>| {
            println!("{:?}", r);
            r.map(|u| u as char)
        }
      )
    | anychar
  )
);

named!(pub character<CompleteStr, Expression>,
  map!(
    delimited!(char!('\''),
    character_content,
    char!('\'')),
    Expression::Character
  )
);

#[cfg(test)]
mod tests {

    use ast::expression::*;
    use nom::types::CompleteStr;

    #[test]
    fn character_literal() {
        assert_eq!(
            character(CompleteStr("'a'")),
            Ok((CompleteStr(""), Expression::Character('a')))
        );
    }

    #[test]
    fn newline_literal() {
        assert_eq!(
            character(CompleteStr("'\n'")),
            Ok((CompleteStr(""), Expression::Character('\n')))
        );
    }

    #[test]
    fn charcode_literal() {
        println!("{:?}", u8::from_str_radix("23", 16));
        assert_eq!(
            character(CompleteStr("'\\x23'")),
            Ok((CompleteStr(""), Expression::Character('#')))
        );
    }

    #[test]
    fn character_literals_must_contain_one_character() {
        assert!(character(CompleteStr("''")).is_err())
    }
}

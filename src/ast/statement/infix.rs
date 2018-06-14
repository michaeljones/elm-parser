use ast::binop::Assoc;
use ast::expression::{expression, term};
use ast::helpers::{lo_name, operator, spaces, spaces_and_newlines};
use ast::statement::core::Statement;
use ast::statement::type_::{type_, type_annotation, type_constructor};

use nom::digit;
use nom::types::CompleteStr;

named!(pub infix_declaration<CompleteStr, Statement>,
  do_parse!(
    type_: alt!(
        value!(Assoc::L, tag!("infixl"))
      | value!(Assoc::R, tag!("infixr"))
      | value!(Assoc::N, tag!("infix"))
    ) >>
    spaces >>
    value: map_res!(digit, |s: CompleteStr| s.to_string().parse::<i64>()) >>
    spaces >>
    name: alt!(
        dbg!(lo_name)
      | dbg!(operator)
    ) >>
    (Statement::InfixDeclaration(type_, value, name))
  )
);

#[cfg(test)]
mod tests {

    use ast::expression::Expression;
    use ast::statement::core::*;
    use ast::statement::infix::*;
    use nom::types::CompleteStr;

    #[test]
    fn non() {
        assert_eq!(
            infix_declaration(CompleteStr("infix 9 :-")),
            Ok((
                CompleteStr(""),
                Statement::InfixDeclaration(Assoc::N, 9, ":-".to_string())
            ))
        );
    }

    #[test]
    fn left() {
        assert_eq!(
            infix_declaration(CompleteStr("infixl 8 :=:")),
            Ok((
                CompleteStr(""),
                Statement::InfixDeclaration(Assoc::L, 8, ":=:".to_string())
            ))
        );
    }

    #[test]
    fn right() {
        assert_eq!(
            infix_declaration(CompleteStr("infixr 7 -+-")),
            Ok((
                CompleteStr(""),
                Statement::InfixDeclaration(Assoc::R, 7, "-+-".to_string())
            ))
        );
    }
}

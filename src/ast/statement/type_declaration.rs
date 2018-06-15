use ast::helpers::{spaces, spaces_and_newlines};
use ast::statement::core::Statement;
use ast::statement::type_::{type_, type_annotation, type_constructor};

use nom::types::CompleteStr;

named_args!(pub type_alias_declaration(indentation: u32)<CompleteStr, Statement>,
  do_parse!(
    tag!("type") >>
    spaces >>
    tag!("alias") >>
    spaces >>
    name: call!(type_, indentation) >>
    spaces >>
    char!('=') >>
    spaces_and_newlines >>
    declaration: call!(type_annotation, indentation) >>
    (Statement::TypeAliasDeclaration(name, declaration))
  )
);

named_args!(pub type_declaration(indentation: u32)<CompleteStr, Statement>,
  do_parse!(
    tag!("type") >>
    spaces >>
    name: call!(type_, indentation) >>
    spaces >>
    char!('=') >>
    spaces_and_newlines >>
    definition: separated_nonempty_list!(
      delimited!(spaces_and_newlines, char!('|'), spaces),
      call!(type_constructor, indentation)
    ) >>
    (Statement::TypeDeclaration(name, definition))
  )
);

#[cfg(test)]
mod tests {

    use ast::statement::core::*;
    use ast::statement::type_declaration::*;
    use nom::types::CompleteStr;

    #[test]
    fn can_parse_empty_record_aliases() {
        assert_eq!(
            type_alias_declaration(CompleteStr("type alias A = {}"), 0),
            Ok((
                CompleteStr(""),
                Statement::TypeAliasDeclaration(
                    Type::TypeConstructor(vec!["A".to_string()], vec![]),
                    Type::TypeRecord(vec![])
                )
            ))
        );
    }

    #[test]
    fn can_parse_aliases_of_unit() {
        assert_eq!(
            type_alias_declaration(CompleteStr("type alias A = ()"), 0),
            Ok((
                CompleteStr(""),
                Statement::TypeAliasDeclaration(
                    Type::TypeConstructor(vec!["A".to_string()], vec![]),
                    Type::TypeTuple(vec![])
                )
            ))
        );
    }

    #[test]
    fn can_parse_simple_type_declaration() {
        assert_eq!(
            type_declaration(CompleteStr("type A = A"), 0),
            Ok((
                CompleteStr(""),
                Statement::TypeDeclaration(
                    Type::TypeConstructor(vec!["A".to_string()], vec![]),
                    vec![Type::TypeConstructor(vec!["A".to_string()], vec![])]
                )
            ))
        );
    }

    #[test]
    fn can_parse_multi_type_declaration() {
        assert_eq!(
            type_declaration(CompleteStr("type A = A | B | C"), 0),
            Ok((
                CompleteStr(""),
                Statement::TypeDeclaration(
                    Type::TypeConstructor(vec!["A".to_string()], vec![]),
                    vec![
                        Type::TypeConstructor(vec!["A".to_string()], vec![]),
                        Type::TypeConstructor(vec!["B".to_string()], vec![]),
                        Type::TypeConstructor(vec!["C".to_string()], vec![]),
                    ]
                )
            ))
        );
    }

    #[test]
    fn can_parse_multiline_type_declaration() {
        assert_eq!(
            type_declaration(CompleteStr("type A = A | B\n| C\n| D"), 0),
            Ok((
                CompleteStr(""),
                Statement::TypeDeclaration(
                    Type::TypeConstructor(vec!["A".to_string()], vec![]),
                    vec![
                        Type::TypeConstructor(vec!["A".to_string()], vec![]),
                        Type::TypeConstructor(vec!["B".to_string()], vec![]),
                        Type::TypeConstructor(vec!["C".to_string()], vec![]),
                        Type::TypeConstructor(vec!["D".to_string()], vec![]),
                    ]
                )
            ))
        );
    }
}

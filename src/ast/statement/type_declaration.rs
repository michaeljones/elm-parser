use ast::helpers::{spaces, spaces_or_new_lines_and_indent, IR};
use ast::statement::core::Statement;
use ast::statement::type_::{type_, type_annotation, type_constructor};

use nom::types::CompleteStr;

named!(pub type_alias_declaration<CompleteStr, Statement>,
  do_parse!(
    tag!("type") >>
    spaces >>
    tag!("alias") >>
    spaces >>
    name: call!(type_, 0) >>
    spaces >>
    char!('=') >>
    call!(spaces_or_new_lines_and_indent, 1, IR::GTE) >>
    declaration: call!(type_annotation, 1) >>
    (Statement::TypeAliasDeclaration(name, declaration))
  )
);

named_args!(pub type_declaration(indentation: u32)<CompleteStr, Statement>,
  do_parse!(
    tag!("type") >>
    spaces >>
    name: call!(type_, indentation) >>
    new_indent: call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    char!('=') >>
    call!(spaces_or_new_lines_and_indent, new_indent, IR::GTE) >>
    definition: separated_nonempty_list!(
      delimited!(call!(spaces_or_new_lines_and_indent, new_indent, IR::GTE), char!('|'), spaces),
      call!(type_constructor, new_indent)
    ) >>
    (Statement::TypeDeclaration(name, definition))
  )
);

#[cfg(test)]
mod tests {

    use ast::statement::core::*;
    use ast::statement::type_declaration::*;
    use nom::types::CompleteStr;

    fn tvar(name: &str) -> Type {
        Type::TypeVariable(name.to_string())
    }

    fn tapp(a: Type, b: Type) -> Type {
        Type::TypeApplication(Box::new(a), Box::new(b))
    }

    fn tcon(name: &str, v: Vec<Type>) -> Type {
        Type::TypeConstructor(vec![name.to_string()], v)
    }

    #[test]
    fn can_parse_empty_record_aliases() {
        assert_eq!(
            type_alias_declaration(CompleteStr("type alias A = {}")),
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
            type_alias_declaration(CompleteStr("type alias A = ()")),
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
    fn multi_line_record() {
        assert_eq!(
            type_alias_declaration(CompleteStr(
                "type alias A =\n { a : String\n , b : String\n }"
            )),
            Ok((
                CompleteStr(""),
                Statement::TypeAliasDeclaration(
                    Type::TypeConstructor(vec!["A".to_string()], vec![]),
                    Type::TypeRecord(vec![
                        (
                            "a".to_string(),
                            Type::TypeConstructor(vec!["String".to_string()], vec![]),
                        ),
                        (
                            "b".to_string(),
                            Type::TypeConstructor(vec!["String".to_string()], vec![]),
                        ),
                    ])
                )
            ))
        );
    }

    #[test]
    fn multi_line_record_with_tuple_list() {
        assert_eq!(
            type_alias_declaration(CompleteStr(
                "type alias A =\n { a : String\n , b : List ( String, String )\n }"
            )),
            Ok((
                CompleteStr(""),
                Statement::TypeAliasDeclaration(
                    Type::TypeConstructor(vec!["A".to_string()], vec![]),
                    Type::TypeRecord(vec![
                        (
                            "a".to_string(),
                            Type::TypeConstructor(vec!["String".to_string()], vec![]),
                        ),
                        (
                            "b".to_string(),
                            Type::TypeConstructor(
                                vec!["List".to_string()],
                                vec![Type::TypeTuple(vec![
                                    tcon("String", vec![]),
                                    tcon("String", vec![]),
                                ])],
                            ),
                        ),
                    ])
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
            type_declaration(CompleteStr("type A\n  = A | B\n  | C\n  | D"), 0),
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

    #[test]
    fn can_parse_complex_function_alias() {
        assert_eq!(
            type_alias_declaration(CompleteStr(
                "type alias LanguageHelper a b =
    ({ b
        | commonErrorName : a
        , commonErrorDescription : a
        , kpiErrorCalculationType : a
        , kpiErrorAggregationType : a
     }
     -> a
    )
    -> String"
            )),
            Ok((
                CompleteStr(""),
                Statement::TypeAliasDeclaration(
                    tcon("LanguageHelper", vec![tvar("a"), tvar("b")]),
                    tapp(
                        tapp(
                            Type::TypeRecordConstructor(
                                Box::new(tvar("b")),
                                vec![
                                    ("commonErrorName".to_string(), tvar("a")),
                                    ("commonErrorDescription".to_string(), tvar("a")),
                                    ("kpiErrorCalculationType".to_string(), tvar("a")),
                                    ("kpiErrorAggregationType".to_string(), tvar("a")),
                                ],
                            ),
                            tvar("a"),
                        ),
                        tcon("String", vec![]),
                    )
                )
            ))
        );
    }

    // #[test]
    // fn fails_with_trailing_character() {
    //     assert!(type_declaration(CompleteStr("type A\n  = A | B\n  | C\n  | D\n\nf"), 0).is_err());
    // }
}

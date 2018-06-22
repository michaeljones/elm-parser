use ast::helpers::{lo_name, spaces, spaces_and_newlines, spaces_or_new_lines_and_indent, up_name,
                   Name, IR};
use ast::statement::core::Type;

use nom::types::CompleteStr;

named!(type_variable<CompleteStr, Type>,
  map!(re_matches!(r#"^([a-z][a-z1-9_]*)"#), |c| Type::TypeVariable(c[0].to_string()))
);

named!(type_constant<CompleteStr, Type>,
  map!(
    separated_nonempty_list!(
      char!('.'),
      up_name
    ),
    |v| Type::TypeConstructor(v, vec![])
  )
);

named_args!(type_tuple(indentation: u32)<CompleteStr, Type>,
  map!(
    delimited!(
      preceded!(char!('('), opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE))),
      separated_list!(
        delimited!(
            opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE)),
            char!(','),
            opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE))
        ),
        call!(type_, indentation)
      ),
      terminated!(opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE)), char!(')'))
    ),
    |v| Type::TypeTuple(v)
  )
);

named_args!(type_record_pair(indentation: u32)<CompleteStr, (Name, Type)>,
  do_parse!(
    name: lo_name >>
    opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE)) >>
    char!(':') >>
    opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE)) >>
    type_annotation: call!(type_annotation, indentation) >>
    ((name, type_annotation))
  )
);

named_args!(type_record_pairs(indentation: u32)<CompleteStr, Vec<(Name, Type)>>,
  separated_list!(
    delimited!(
        opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE)),
        char!(','),
        opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE))
    ),
    call!(type_record_pair, indentation)
  )
);

named_args!(type_record_constructor(indentation: u32)<CompleteStr, Type>,
  delimited!(
    char!('{'),
    do_parse!(
      spaces >>
      var: type_variable >>
      spaces >>
      char!('|') >>
      record_pairs: call!(type_record_pairs, indentation) >>
      (Type::TypeRecordConstructor(Box::new(var), record_pairs))
    ),
    char!('}')
  )
);

named_args!(type_record(indentation: u32)<CompleteStr, Type>,
  delimited!(
    preceded!(char!('{'), opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE))),
    map!(
      call!(type_record_pairs, indentation),
      Type::TypeRecord
    ),
    terminated!(opt!(call!(spaces_or_new_lines_and_indent, indentation, IR::GTE)), char!('}'))
  )
);

named_args!(type_parameter(indentation: u32)<CompleteStr, Type>,
  alt!(
      type_variable
    | type_constant
    | call!(type_record_constructor, indentation)
    | call!(type_record, indentation)
    | call!(type_tuple, indentation)
    | delimited!(char!('('), call!(type_annotation, indentation), char!(')'))
  )
);

named_args!(pub type_constructor(indentation: u32)<CompleteStr, Type>,
  do_parse!(
    first: separated_nonempty_list!(
      char!('.'),
      up_name
    ) >>
    second: opt!(
      many0!(
        preceded!(
          call!(spaces_or_new_lines_and_indent, indentation, IR::GTE),
          call!(type_parameter, indentation)
        )
      )
    ) >>
    (Type::TypeConstructor(first, second.unwrap_or(vec![])))
  )
);

named_args!(pub type_(indentation: u32)<CompleteStr, Type>,
  alt!(
      call!(type_constructor, indentation)
    | type_variable
    | call!(type_record_constructor, indentation)
    | call!(type_record, indentation)
    | call!(type_tuple, indentation)
    | delimited!(char!('('), call!(type_annotation, indentation), char!(')'))
  )
);

named_args!(pub type_annotation(indentation: u32)<CompleteStr, Type>,
  map_res!(
    separated_nonempty_list!(
      delimited!(spaces_and_newlines, tag!("->"), spaces_and_newlines),
      call!(type_, indentation)
    ),
    |mut v: Vec<Type>| {
        if v.len() == 0 {
            Err("Empty type".to_string())
        }
        else if v.len() == 1 {
            Ok(v[0].clone())
        } else {
            let last = v.pop();
            let second_to_last = v.pop();
            match (second_to_last, last) {
                (Some(t1), Some(t2)) => {
                    let mut app = Type::TypeApplication(Box::new(t1), Box::new(t2));
                    for entry in v.iter().rev() {
                        app = Type::TypeApplication(Box::new(entry.clone()), Box::new(app))
                    }
                    Ok(app)
                }
                _ => {
                    Err("Expected at least 2 entries for match".to_string())
                }
            }
        }
    }
  )
);

#[cfg(test)]
mod tests {

    use ast::statement::core::*;
    use ast::statement::type_::*;
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
    fn constant() {
        assert_eq!(
            type_annotation(CompleteStr("Int"), 0),
            Ok((
                CompleteStr(""),
                Type::TypeConstructor(vec!["Int".to_string()], vec![])
            ))
        );
    }

    #[test]
    fn variables() {
        assert_eq!(
            type_annotation(CompleteStr("a"), 0),
            Ok((CompleteStr(""), tvar("a")))
        );
    }

    #[test]
    fn variables_with_numbers() {
        assert_eq!(
            type_annotation(CompleteStr("a1"), 0),
            Ok((CompleteStr(""), tvar("a1")))
        );
    }

    #[test]
    fn empty_record() {
        assert_eq!(
            type_annotation(CompleteStr("{}"), 0),
            Ok((CompleteStr(""), Type::TypeRecord(vec![])))
        );
    }

    #[test]
    fn empty_tuple() {
        assert_eq!(
            type_annotation(CompleteStr("()"), 0),
            Ok((CompleteStr(""), Type::TypeTuple(vec![])))
        );
    }

    #[test]
    fn basic_record() {
        assert_eq!(
            type_annotation(CompleteStr("{ a : String }"), 0),
            Ok((
                CompleteStr(""),
                Type::TypeRecord(vec![("a".to_string(), tcon("String", vec![]))])
            ))
        );
    }

    #[test]
    fn two_entry_record() {
        assert_eq!(
            type_annotation(CompleteStr("{ a : String, b : String }"), 0),
            Ok((
                CompleteStr(""),
                Type::TypeRecord(vec![
                    ("a".to_string(), tcon("String", vec![])),
                    ("b".to_string(), tcon("String", vec![])),
                ])
            ))
        );
    }

    #[test]
    fn multi_line_record() {
        assert_eq!(
            type_annotation(
                CompleteStr(
                    "{
                 a : String
               , b : String
}"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                Type::TypeRecord(vec![
                    ("a".to_string(), tcon("String", vec![])),
                    ("b".to_string(), tcon("String", vec![])),
                ])
            ))
        );
    }

    #[test]
    fn multi_line_record_with_comments() {
        assert_eq!(
            type_annotation(
                CompleteStr(
                    "{
                 -- First comment
                 a : String

                 -- Second comment
               , b : String -- Third comment
}"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                Type::TypeRecord(vec![
                    ("a".to_string(), tcon("String", vec![])),
                    ("b".to_string(), tcon("String", vec![])),
                ])
            ))
        );
    }

    #[test]
    fn list_of_tuples() {
        assert_eq!(
            type_annotation(CompleteStr("List ( String, String )"), 0),
            Ok((
                CompleteStr(""),
                tcon(
                    "List",
                    vec![Type::TypeTuple(vec![
                        tcon("String", vec![]),
                        tcon("String", vec![]),
                    ])]
                )
            ))
        );
    }

    #[test]
    fn application() {
        assert_eq!(
            type_annotation(CompleteStr("a -> b"), 0),
            Ok((CompleteStr(""), tapp(tvar("a"), tvar("b"))))
        );
    }

    #[test]
    fn application_associativity() {
        assert_eq!(
            type_annotation(CompleteStr("a -> b -> c"), 0),
            Ok((CompleteStr(""), tapp(tvar("a"), tapp(tvar("b"), tvar("c")))))
        );
    }

    #[test]
    fn application_parens() {
        assert_eq!(
            type_annotation(CompleteStr("(a -> b) -> c"), 0),
            Ok((CompleteStr(""), tapp(tapp(tvar("a"), tvar("b")), tvar("c"))))
        );
    }

    #[test]
    fn qualified_types() {
        assert_eq!(
            type_annotation(CompleteStr("Html.App Msg"), 0),
            Ok((
                CompleteStr(""),
                Type::TypeConstructor(
                    vec!["Html".to_string(), "App".to_string()],
                    vec![Type::TypeConstructor(vec!["Msg".to_string()], vec![])]
                )
            ))
        );
    }

    #[test]
    fn multi_line_type() {
        assert_eq!(
            type_annotation(
                CompleteStr(
                    "CssModules.Helpers
        { a : String
        , b : String
        }
        msg"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                Type::TypeConstructor(
                    vec!["CssModules".to_string(), "Helpers".to_string()],
                    vec![
                        Type::TypeRecord(vec![
                            (
                                "a".to_string(),
                                Type::TypeConstructor(vec!["String".to_string()], vec![]),
                            ),
                            (
                                "b".to_string(),
                                Type::TypeConstructor(vec!["String".to_string()], vec![]),
                            ),
                        ]),
                        Type::TypeVariable("msg".to_string()),
                    ]
                )
            ))
        );
    }
}

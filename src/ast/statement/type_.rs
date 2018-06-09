use ast::helpers::{lo_name, spaces, up_name, Name};
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

named!(type_tuple<CompleteStr, Type>,
  map!(
    delimited!(
      char!('('),
      separated_nonempty_list!(
        tag!(", "),
        type_
      ),
      char!(')')
    ),
    |v| Type::TypeTuple(v)
  )
);

named!(type_record_pair<CompleteStr, (Name, Type)>,
  do_parse!(
    name: lo_name >>
    char!(':') >>
    type_annotation: type_annotation >>
    ((name, type_annotation))
  )
);

named!(type_record_pairs<CompleteStr, Vec<(Name, Type)>>,
  separated_nonempty_list!(
    tag!(", "),
    type_record_pair
  )
);

named!(type_record_constructor<CompleteStr, Type>,
  delimited!(
    char!('{'),
    do_parse!(
      spaces >>
      var: type_variable >>
      spaces >>
      char!('|') >>
      record_pairs: type_record_pairs >>
      (Type::TypeRecordConstructor(Box::new(var), record_pairs))
    ),
    char!('}')
  )
);

named!(type_record<CompleteStr, Type>,
  delimited!(
    char!('{'),
    map!(
      type_record_pairs,
      Type::TypeRecord
    ),
    char!('}')
  )
);

named!(type_parameter<CompleteStr, Type>,
  alt!(
      type_variable
    | type_constant
    | type_record_constructor
    | type_record
    | type_tuple
    | delimited!(char!('('), type_annotation, char!(')'))
  )
);

named!(type_constructor<CompleteStr, Type>,
  do_parse!(
    first: separated_nonempty_list!(
      char!('.'),
      up_name
    ) >>
    second: opt!(preceded!(spaces, many0!(type_parameter))) >>
    (Type::TypeConstructor(first, second.unwrap_or(vec![])))
  )
);

named!(type_<CompleteStr, Type>,
  alt!(
      type_constructor
    | type_variable
    | type_record_constructor
    | type_record
    | type_tuple
    | delimited!(char!('('), type_annotation, char!(')'))
  )
);

named!(type_annotation<CompleteStr, Type>,
  map_res!(
    separated_nonempty_list!(
      tag!(" -> "),
      type_
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

    #[test]
    fn constant() {
        assert_eq!(
            type_annotation(CompleteStr("Int")),
            Ok((
                CompleteStr(""),
                Type::TypeConstructor(vec!["Int".to_string()], vec![])
            ))
        );
    }

    #[test]
    fn variables() {
        assert_eq!(
            type_annotation(CompleteStr("a")),
            Ok((CompleteStr(""), tvar("a")))
        );
    }

    #[test]
    fn variables_with_numbers() {
        assert_eq!(
            type_annotation(CompleteStr("a1")),
            Ok((CompleteStr(""), tvar("a1")))
        );
    }

    #[test]
    fn application() {
        assert_eq!(
            type_annotation(CompleteStr("a -> b")),
            Ok((CompleteStr(""), tapp(tvar("a"), tvar("b"))))
        );
    }

    #[test]
    fn application_associativity() {
        assert_eq!(
            type_annotation(CompleteStr("a -> b -> c")),
            Ok((CompleteStr(""), tapp(tvar("a"), tapp(tvar("b"), tvar("c")))))
        );
    }

    #[test]
    fn application_parens() {
        assert_eq!(
            type_annotation(CompleteStr("(a -> b) -> c")),
            Ok((CompleteStr(""), tapp(tapp(tvar("a"), tvar("b")), tvar("c"))))
        );
    }

    #[test]
    fn qualified_types() {
        assert_eq!(
            type_annotation(CompleteStr("Html.App Msg")),
            Ok((
                CompleteStr(""),
                Type::TypeConstructor(
                    vec!["Html".to_string(), "App".to_string()],
                    vec![Type::TypeConstructor(vec!["Msg".to_string()], vec![])]
                )
            ))
        );
    }
}

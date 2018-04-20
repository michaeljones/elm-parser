use nom::types::CompleteStr;

use nom;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Function(Vec<Type>),
    Single(String),
}

fn combine(first: Type, rest: Vec<Vec<Type>>) -> Vec<Type> {
    if rest.is_empty() {
        vec![first]
    } else {
        let mut flattened: Vec<Type> = rest.iter().flat_map(|e| e.clone()).collect();
        flattened.insert(0, first);
        flattened
    }
}

named!(inner_type<CompleteStr, Vec<Type>>,
  do_parse!(
      first: map!(nom::alphanumeric, |v| Type::Single(v.0.to_string())) >>
      rest: many0!(
          do_parse!(
              tag!(" -> ") >>
              name: type_choice >>
              (name)
          )
      ) >>
      (combine(first, rest))
  )
);

named!(type_choice<CompleteStr, Vec<Type>>,
  alt!(
        inner_type
      | do_parse!(
            tag!("(") >>
            function: inner_type >>
            tag!(")") >>
            rest: many0!(
                do_parse!(
                    tag!(" -> ") >>
                    name: type_choice >>
                    (name)
                )
            ) >>
            (combine(Type::Function(function), rest))
        )
  )
);

fn choose_type(data: Vec<Type>) -> Type {
    if data.len() == 1 {
        data[0].clone()
    } else {
        Type::Function(data)
    }
}

named!(pub type_<CompleteStr, Type>,
  map!(type_choice, choose_type)
);

#[test]
fn parse_elm_type() {
    assert_eq!(
        type_(CompleteStr("Int -> Int")),
        Ok((
            CompleteStr(""),
            Type::Function(vec![
                Type::Single("Int".to_string()),
                Type::Single("Int".to_string()),
            ],)
        ))
    );

    assert_eq!(
        type_(CompleteStr("String -> Float -> String")),
        Ok((
            CompleteStr(""),
            Type::Function(vec![
                Type::Single("String".to_string()),
                Type::Single("Float".to_string()),
                Type::Single("String".to_string()),
            ],)
        ))
    );

    assert_eq!(
        type_(CompleteStr("String -> (Float -> String) -> Int")),
        Ok((
            CompleteStr(""),
            Type::Function(vec![
                Type::Single("String".to_string()),
                Type::Function(vec![
                    Type::Single("Float".to_string()),
                    Type::Single("String".to_string()),
                ]),
                Type::Single("Int".to_string()),
            ],)
        ))
    );

    assert_eq!(
        type_(CompleteStr("(Int -> String) -> (Float -> String) -> Int")),
        Ok((
            CompleteStr(""),
            Type::Function(vec![
                Type::Function(vec![
                    Type::Single("Int".to_string()),
                    Type::Single("String".to_string()),
                ]),
                Type::Function(vec![
                    Type::Single("Float".to_string()),
                    Type::Single("String".to_string()),
                ]),
                Type::Single("Int".to_string()),
            ],)
        ))
    );

    assert_eq!(
        type_(CompleteStr("Int -> (Float -> String)")),
        Ok((
            CompleteStr(""),
            Type::Function(vec![
                Type::Single("Int".to_string()),
                Type::Function(vec![
                    Type::Single("Float".to_string()),
                    Type::Single("String".to_string()),
                ]),
            ],)
        ))
    );
}

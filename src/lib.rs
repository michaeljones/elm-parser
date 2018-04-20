#[macro_use]
extern crate nom;

use nom::multispace;

use nom::types::CompleteStr;

#[derive(Debug, PartialEq)]
pub enum ModuleExposing {
    All,
    Named(Vec<String>),
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Function(FunctionDetails),
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: String,
    pub exposing: ModuleExposing,
    pub contents: Vec<Declaration>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionSignature {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, PartialEq)]
pub struct FunctionImplementation {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDetails {
    pub signature: Option<FunctionSignature>,
    pub implementation: FunctionImplementation,
}

// Type
//

#[derive(Debug, PartialEq)]
pub enum Type {
    Function(Vec<Type>),
    Single(String),
}

fn combine(first: Type, mut rest: Vec<Type>) -> Type {
    if rest.is_empty() {
        first
    } else {
        rest.insert(0, first);
        Type::Function(rest)
    }
}

named!(type_<CompleteStr, Type>,
  do_parse!(
      first: map!(nom::alpha1, |v| Type::Single(v.0.to_string())) >>
      rest: many0!(
          do_parse!(
              tag!(" -> ") >>
              name: map!(nom::alphanumeric, |v| Type::Single(v.0.to_string())) >>
              (name)
          )
      ) >>
      (combine(first, rest))
  )
);

named!(module_name<CompleteStr, &str>,
  map!(take_until_s!(" "), |c| c.0)
);

fn to_module_exposing(string: CompleteStr) -> Result<ModuleExposing, String> {
    if string == CompleteStr("..") {
        return Ok(ModuleExposing::All);
    }
    return Ok(ModuleExposing::Named(vec![]));
}

named!(module_exposing<CompleteStr, ModuleExposing>,
  map_res!(
      delimited!(char!('('), take!(2), char!(')')),
      to_module_exposing
  )
);

named!(function_signature<CompleteStr, FunctionSignature>,
  do_parse!(
      name: take_while!(|c| c != ' ') >>
      multispace >>
      tag!(":") >>
      multispace >>
      type_: type_ >>
      (FunctionSignature {
          name: name.0.to_string(),
          type_: type_
      })
  )
);

named!(function_implementation<CompleteStr, FunctionImplementation>,
  do_parse!(
      name: take_while!(|c| c != ' ') >>
      tag!(" =\n") >>
      multispace >>
      tag!("1") >>
      (FunctionImplementation {
          name: name.0.to_string()
      })
  )
);

named!(function<CompleteStr, Declaration>,
  do_parse!(
      signature: opt!(
          do_parse!(
              signature: function_signature >>
              char!('\n') >>
              (signature)
          )
      ) >>
      implementation: function_implementation >>
      (Declaration::Function(FunctionDetails {
          signature: signature,
          implementation: implementation
      })
      )
  )
);

fn filter_none(data: Vec<Option<Declaration>>) -> Vec<Declaration> {
    data.into_iter().filter_map(|c| c).collect()
}

named!(contents<CompleteStr, Vec<Declaration>>,
      map!(
          many1!(
              alt!(
                  map!(char!('\n'), |_| None) |
                  map!(function, Some)
              )
          ),
          filter_none
      )
);

named!(pub elm_module<CompleteStr, Module>,
  do_parse!(
    tag!("module ") >>
    name: module_name >>
    tag!(" exposing ") >>
    exposing: module_exposing >>
    contents: contents >>
    (Module {
        name: name.to_string(),
        exposing: exposing,
        contents: contents
    })
  )
);

#[test]
fn parse_elm_file() {
    assert_eq!(
        elm_module(CompleteStr(include_str!("../examples/Basic.elm"))),
        Ok((
            CompleteStr(""),
            Module {
                name: "Basic".to_string(),
                exposing: ModuleExposing::All,
                contents: vec![
                    Declaration::Function(FunctionDetails {
                        signature: Some(FunctionSignature {
                            name: "a".to_string(),
                            type_: Type::Single("Int".to_string()),
                        }),
                        implementation: FunctionImplementation {
                            name: "a".to_string(),
                        },
                    }),
                ],
            }
        ))
    );
}

#[test]
fn parse_elm_module_declaration() {
    assert_eq!(
        elm_module(CompleteStr("module Basic exposing (..)\n")),
        Ok((
            CompleteStr(""),
            Module {
                name: "Basic".to_string(),
                exposing: ModuleExposing::All,
                contents: vec![],
            }
        ))
    );
}

// #[cfg(test)]
// named!(testingalpha<CompleteStr, CompleteStr>,
//     map!(nom::alphanumeric, |c| c)
// );

#[test]
fn parse_elm_type() {
    //   assert_eq!(
    //       testingalpha(CompleteStr("Int")),
    //       Ok((CompleteStr(""), CompleteStr("Int")))
    //   );
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
        type_(CompleteStr("Int -> Int -> String")),
        Ok((
            CompleteStr(""),
            Type::Function(vec![
                Type::Single("Int".to_string()),
                Type::Single("Int".to_string()),
                Type::Single("String".to_string()),
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
}

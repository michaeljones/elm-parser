#[cfg(test)]
#[macro_use]
extern crate nom;

#[cfg(test)]
use nom::{multispace, alpha1};

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

#[cfg(test)]
fn combine(first: Type, mut rest: Vec<Type>) -> Type {
    if rest.is_empty() {
        first
    } else {
        rest.insert(0, first);
        Type::Function(rest)
    }
}

#[cfg(test)]
named!(type_<&str, Type>,
  complete!(
  dbg_dmp!(
  do_parse!(
      first: map!(nom::alpha1, |v| Type::Single(v.to_string())) >>
      rest: many0!(
          dbg_dmp!(
          complete!(
          do_parse!(
              tag!(" -> ") >>
              name: map!(many1!(nom::alpha1), |v| Type::Single(v.join(""))) >>
              (name)
          )
          )
          )
      ) >>
      (combine(first, rest))
  )
  )
  )
);

#[cfg(test)]
named!(module_name<&str, &str>,
  take_until_s!(" ")
);

#[cfg(test)]
fn to_module_exposing(string: &str) -> Result<ModuleExposing, String> {
    if string == ".." {
        return Ok(ModuleExposing::All);
    }
    return Ok(ModuleExposing::Named(vec![]));
}

#[cfg(test)]
named!(module_exposing<&str, ModuleExposing>,
  map_res!(
      delimited!(char!('('), take!(2), char!(')')),
      to_module_exposing
  )
);

#[cfg(test)]
named!(function_signature<&str, FunctionSignature>,
  do_parse!(
      name: take_while!(|c| c != ' ') >>
      multispace >>
      tag!(":") >>
      multispace >>
      type_: type_ >>
      (FunctionSignature {
          name: name.to_string(),
          type_: type_
      })
  )
);

#[cfg(test)]
named!(function_implementation<&str, FunctionImplementation>,
  do_parse!(
      name: take_while!(|c| c != ' ') >>
      tag!(" =\n") >>
      multispace >>
      tag!("1") >>
      (FunctionImplementation { 
          name: name.to_string()
      })
  )
);

#[cfg(test)]
named!(function<&str, Declaration>,
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

#[cfg(test)]
fn filter_none(data: Vec<Option<Declaration>>) -> Vec<Declaration> {
    data.into_iter().filter_map(|c| c).collect()
}

#[cfg(test)]
named!(contents<&str, Vec<Declaration>>,
      map!(
          many1!(
              alt_complete!(
                  map!(char!('\n'), |_| None) |
                  map!(function, Some)
              )
          ),
          filter_none
      )
);

#[cfg(test)]
named!(elm_module<&str, Module>,
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
        elm_module(include_str!("../examples/Basic.elm")),
        Ok((
            "",
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
        elm_module("module Basic exposing (..)\n"),
        Ok((
            "",
            Module {
                name: "Basic".to_string(),
                exposing: ModuleExposing::All,
                contents: vec![],
            }
        ))
    );
}

#[cfg(test)]
named!(testingalpha<&str, String>,
    map!(many1!(nom::alpha), |c| c.join(""))
);

#[test]
fn parse_elm_type() {
    // assert_eq!(testingalpha("Int"), Ok(("", "Int")));
    assert_eq!(
        type_("Int -> Int"),
        Ok((
            "",
            Type::Function(vec![
                Type::Single("Int".to_string()),
                Type::Single("Int".to_string()),
            ],)
        ))
    );
    assert_eq!(
        type_("Int -> Int -> String"),
        Ok((
            "",
            Type::Function(vec![
                Type::Single("Int".to_string()),
                Type::Single("Int".to_string()),
                Type::Single("String".to_string()),
            ],)
        ))
    );
}

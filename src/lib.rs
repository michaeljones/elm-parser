#[macro_use]
extern crate nom;
extern crate regex;

/*
use nom::{alphanumeric, multispace};

use nom::types::CompleteStr;

mod types;
mod expression;
mod import;
*/

mod ast;

/*
use types::type_;
use expression::expression;
use import::imports;

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
    pub imports: Vec<import::Import>,
    pub contents: Vec<Declaration>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionSignature {
    pub name: String,
    pub type_: types::Type,
}

#[derive(Debug, PartialEq)]
pub struct FunctionImplementation {
    pub name: String,
    pub args: Vec<String>,
    pub expression: expression::Expression,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDetails {
    pub signature: Option<FunctionSignature>,
    pub implementation: FunctionImplementation,
}

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
      args: many0!(
          do_parse!(
              tag!(" ") >>
              arg_name: alphanumeric >>
              (arg_name.0.to_string())
          )
      ) >> 
      tag!(" =\n") >>
      multispace >>
      expression: call!(expression, 0) >>
      (FunctionImplementation {
          name: name.0.to_string(),
          args: args,
          expression: expression
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
                    map!(char!('\n'), |_| None)
                  | map!(function, Some)
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
    opt!(multispace) >>
    imports: imports >>
    opt!(multispace) >>
    contents: contents >>
    (Module {
        name: name.to_string(),
        exposing: exposing,
        imports: imports,
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
                imports: vec![
                    import::Import {
                        name: vec!["String".to_string()],
                        alias: None,
                        exposing: import::Exposing::None,
                    },
                ],
                contents: vec![
                    Declaration::Function(FunctionDetails {
                        signature: Some(FunctionSignature {
                            name: "a".to_string(),
                            type_: types::Type::Single("Int".to_string()),
                        }),
                        implementation: FunctionImplementation {
                            name: "a".to_string(),
                            args: vec![],
                            expression: expression::Expression::Int("1".to_string()),
                        },
                    }),
                ],
            }
        ))
    );
}
*/

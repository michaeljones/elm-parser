#[cfg(test)]
#[macro_use]
extern crate nom;

#[cfg(test)]
use nom::multispace;

#[derive(Debug, PartialEq)]
pub enum ModuleExposing {
    All,
    Named(Vec<String>),
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Function(String),
}

#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: String,
    pub exposing: ModuleExposing,
    pub contents: Vec<Declaration>,
}

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
named!(function<&str, Declaration>,
  do_parse!(
      name: take_while!(|c| c != ' ') >>
      tag!(" =\n") >>
      multispace >>
      tag!("1") >>
      (Declaration::Function(name.to_string()))
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
fn parse_elm() {
    assert_eq!(
        elm_module(include_str!("../examples/Basic.elm")),
        Ok((
            "",
            Module {
                name: "Basic".to_string(),
                exposing: ModuleExposing::All,
                contents: vec![Declaration::Function("a".to_string())],
            }
        ))
    );
}

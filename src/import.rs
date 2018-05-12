use nom::alphanumeric;
use nom::types::CompleteStr;

#[derive(Debug, PartialEq)]
pub enum Exposing {
    All,
    Named(Vec<String>),
    None,
}

#[derive(Debug, PartialEq)]
pub struct Import {
    pub name: Vec<String>,
    pub alias: Option<String>,
    pub exposing: Exposing,
}

/// Tests if byte is ASCII alphabetic: A-Z, a-z
#[inline]
pub fn is_uppercase(chr: char) -> bool {
    (chr >= 'A' && chr <= 'Z')
}

named!(module_token<CompleteStr, String>,
  do_parse!(
    start: take_while_m_n!(1, 1, is_uppercase) >>
    rest: alphanumeric >>
    (start.0.to_owned() + rest.0)
  )
);

fn combine(first: String, mut rest: Vec<String>) -> Vec<String> {
    rest.insert(0, first);
    rest
}

named!(module_name<CompleteStr, Vec<String>>,
  do_parse!(
    first: module_token >>
    rest: many0!(
        do_parse!(
            char!('.') >>
            name: module_token >>
            (name)
        )
    ) >>
    (combine(first, rest))
  )
);

named!(alias<CompleteStr, String>,
  do_parse!(
   is_a!(" ") >>
   tag!("as") >>
   is_a!(" ") >>
   alias: module_token >>
   (alias)
  )
);

named!(exposing_contents<CompleteStr, Exposing>,
  alt!(
      map!(tag!(".."), |_| Exposing::All)
    | map!(
        separated_list!(
          tag!(", "),
          map!(take_until_either!(", )"), |s| s.to_string())
        ),
        |v| Exposing::Named(v)
      )
  )
);

named!(exposing<CompleteStr, Exposing>,
  do_parse!(
   is_a!(" ") >>
   tag!("exposing") >>
   is_a!(" ") >>
   char!('(') >>
   exposing: exposing_contents >>
   char!(')') >>
   (exposing)
  )
);

named!(import<CompleteStr, Import>,
  do_parse!(
      tag!("import") >>
      is_a!(" ") >>
      name: module_name >>
      alias: opt!(alias) >>
      exposing: opt!(exposing) >>
      (Import {
          name: name,
          alias: alias,
          exposing: exposing.unwrap_or(Exposing::None)
      })
  )
);

named!(pub imports<CompleteStr, Vec<Import>>,
  separated_list!(
     char!('\n'),
     import
  )
);

#[cfg(test)]
fn s(s_: &str) -> String {
    s_.to_string()
}

#[test]
fn parse_single_import() {
    assert_eq!(
        import(CompleteStr("import String")),
        Ok((
            CompleteStr(""),
            Import {
                name: vec![s("String")],
                alias: None,
                exposing: Exposing::None,
            }
        ))
    );
}

#[test]
fn parse_dotted_import() {
    assert_eq!(
        import(CompleteStr("import Data.String")),
        Ok((
            CompleteStr(""),
            Import {
                name: vec![s("Data"), s("String")],
                alias: None,
                exposing: Exposing::None,
            }
        ))
    );
}

#[test]
fn fails_lowercase_import() {
    assert!(import(CompleteStr("import string")).is_err());
}

#[test]
fn parse_import_alias() {
    assert_eq!(
        import(CompleteStr("import Data.String as DataString")),
        Ok((
            CompleteStr(""),
            Import {
                name: vec![s("Data"), s("String")],
                alias: Some(s("DataString")),
                exposing: Exposing::None,
            }
        ))
    );
}

#[test]
fn parse_import_exposing_all() {
    assert_eq!(
        import(CompleteStr("import Data.String exposing (..)")),
        Ok((
            CompleteStr(""),
            Import {
                name: vec![s("Data"), s("String")],
                alias: None,
                exposing: Exposing::All,
            }
        ))
    );
}

#[test]
fn parse_import_exposing_list() {
    assert_eq!(
        import(CompleteStr("import Data.String exposing (one, two)")),
        Ok((
            CompleteStr(""),
            Import {
                name: vec![s("Data"), s("String")],
                alias: None,
                exposing: Exposing::Named(vec![s("one"), s("two")]),
            }
        ))
    );
}

#[test]
fn parse_multiple_imports() {
    assert_eq!(
        imports(CompleteStr(
            "import Data.String
import Data.Int"
        )),
        Ok((
            CompleteStr(""),
            vec![
                Import {
                    name: vec![s("Data"), s("String")],
                    alias: None,
                    exposing: Exposing::None,
                },
                Import {
                    name: vec![s("Data"), s("Int")],
                    alias: None,
                    exposing: Exposing::None,
                },
            ]
        ))
    );
}

use nom::alphanumeric0;
use nom::types::CompleteStr;

pub type Name = String;

pub type QualifiedType = Vec<Name>;

pub type ModuleName = Vec<String>;

pub type Alias = String;

const RESERVED_WORDS: &[&str] = &[
    "module", "where", "import", "as", "exposing", "type", "alias", "port", "if", "then", "else",
    "let", "in", "case", "of",
];

const RESERVED_OPERATORS: &[&str] = &["=", ".", "..", "->", "--", "|", ":"];

/// Tests if byte is ASCII: a-z
#[inline]
pub fn is_lowercase(chr: char) -> bool {
    (chr >= 'a' && chr <= 'z')
}

/// Tests if byte is ASCII: a-z
#[inline]
pub fn is_uppercase(chr: char) -> bool {
    (chr >= 'A' && chr <= 'Z')
}

named!(pub lo_name<CompleteStr, String>,
  alt!(
      map!(tag!("_"), |v| v.to_string())
    | map_res!(
        re_matches!(r"^([a-z][a-zA-Z0-9_]*)"),
        |v: Vec<CompleteStr>| {
            let name = v[0].to_string();
            if RESERVED_WORDS.contains(&name.as_str()) {
                Err("Reserved word: ".to_string() + &name)
            }
            else {
                Ok(name)
            }
        }
      )
  ) 
);

named!(pub function_name<CompleteStr, String>,
  map!(lo_name, |s| s)
);

named!(pub module_name<CompleteStr, ModuleName>,
  separated_nonempty_list!(
    char!('.'),
    up_name
  )
);

named!(pub up_name<CompleteStr, String>,
  do_parse!(
    start: take_while_m_n!(1, 1, is_uppercase) >>
    rest: map!(alphanumeric0, |v| v.to_string()) >>
    (start.to_string() + &rest)
  )
);

named!(pub operator<CompleteStr, String>,
  map_res!(
    // Regex from elm-ast with extra '-' at the start as it was failing
    map!(re_matches!(r"^([-+\\-\\/*=.$<>:&|^?%#@~!]+)"), |c| c[0].to_string()),
    |op: String| {
        if RESERVED_OPERATORS.contains(&op.as_str()) {
            Err("Reserved operator: ".to_string() + &op)
        }
        else {
            Ok(op)
        }
    }
  )
);

named_args!(pub at_least_indent(indentation: u32) <CompleteStr, u32>,
  map_res!(
      many0!(char!(' ')),
      |spaces: Vec<char>|  {
        let length = spaces.len() as u32;
        // Expect to have the same or more indentation
        if length >= indentation {
            Ok(length)
        } else {
            Err("Not enough spaces".to_string())
        }
      }
  )
);

fn exactly(spaces: CompleteStr, indentation: u32) -> Result<u32, String> {
    let length = spaces.len() as u32;
    // Expect to have exactly the indentation
    if length == indentation {
        Ok(length)
    } else {
        Err(spaces.to_string())
    }
}

named!(pub spaces <CompleteStr, String>,
  map!(is_a!(" "), |s| s.to_string())
);

named!(pub spaces_and_newlines <CompleteStr, String>,
  map!(is_a!(" \n"), |s| s.to_string())
);

named_args!(pub exactly_indent(indentation: u32) <CompleteStr, u32>,
  map_res!(is_a!(" "), |s| exactly(s, indentation))
);

named_args!(pub new_line_and_exact_indent(indentation: u32) <CompleteStr, u32>,
    preceded!(
        re_matches!(r"^[ \n]*\n"),
        call!(exactly_indent, indentation)
    )
);

named_args!(pub spaces_or_new_line_and_indent(indentation: u32) <CompleteStr, u32>,
    alt!(
          do_parse!(
            many0!(char!(' ')) >>
            char!('\n') >>
            indent: call!(at_least_indent, indentation) >>
            (indent)
          )
        | map!(is_a!(" "), |s| indentation + s.to_string().len() as u32)
    )
);

#[cfg(test)]
mod tests {

    use ast::helpers::*;
    use nom::types::CompleteStr;

    #[test]
    fn simple_operator() {
        assert_eq!(
            operator(CompleteStr("==")),
            Ok((CompleteStr(""), "==".to_string()))
        );
    }

    #[test]
    fn another_simple_operator() {
        assert_eq!(
            operator(CompleteStr(":-")),
            Ok((CompleteStr(""), ":-".to_string()))
        );
    }

    #[test]
    fn invalid_operator() {
        assert!(operator(CompleteStr("=")).is_err());
    }

    #[test]
    fn valid_zero_spaces() {
        assert_eq!(
            spaces_or_new_line_and_indent(CompleteStr("\n"), 0),
            Ok((CompleteStr(""), 0))
        );
    }

    #[test]
    fn invalid_zero_spaces() {
        assert!(spaces_or_new_line_and_indent(CompleteStr("\n"), 1).is_err());
    }
}

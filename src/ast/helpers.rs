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
  map!(re_matches!(r"^([A-Z][a-zA-Z0-9_]*)"), |c| c[0].to_string())
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

named!(pub spaces <CompleteStr, String>,
  map!(is_a!(" "), |s| s.to_string())
);

named!(pub spaces0 <CompleteStr, String>,
  map!(opt!(is_a!(" ")), |s| s.map(|c| c.to_string()).unwrap_or("".to_string()))
);

named!(pub spaces_and_newlines <CompleteStr, String>,
  map!(is_a!(" \n"), |s| s.to_string())
);

named!(single_line_comment<CompleteStr, String>,
  map!(preceded!(tag!("--"), re_matches!(r"^(.*)")), |v| v[0].to_string())
);

// IndentRelation
pub enum IR {
    EQ,
    GT,
    GTE,
}

named_args!(pub spaces_or_new_lines_and_indent(indentation: u32, ir: IR) <CompleteStr, u32>,
  map_res!(
    many1!(
      alt!(
          map!(spaces, |s| vec![indentation + s.len() as u32 ])
        | map!(single_line_comment, |_| vec![0])
        | many1!(
            preceded!(
              char!('\n'),
              alt!(
                  map!(spaces0, |s| s.len() as u32)
                | map!(single_line_comment, |_| 0)
              )
            )
          )
      )
    ),
    |v: Vec<Vec<u32>>| {
        let new_indent = *(v.last().unwrap_or(&vec![]).last().unwrap_or(&0));
        match ir {
            IR::EQ => {
                if new_indent == indentation {
                    Ok(new_indent)
                }
                else {
                    Err("Indent does not match".to_string())
                }
            },
            IR::GT => {
                if new_indent > indentation {
                    Ok(new_indent)
                }
                else {
                    Err("Indent does must be greater".to_string())
                }
            },
            IR::GTE => {
                if new_indent >= indentation {
                    Ok(new_indent)
                }
                else {
                    Err("Indent does must be greater than or equal".to_string())
                }
            }
        }
    }
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
            spaces_or_new_lines_and_indent(CompleteStr("\n "), 0, IR::GTE),
            Ok((CompleteStr(""), 1))
        );
    }

    #[test]
    fn invalid_zero_spaces() {
        assert!(spaces_or_new_lines_and_indent(CompleteStr("\n"), 1, IR::GTE).is_err());
    }

    #[test]
    fn spaces_with_comment() {
        assert_eq!(
            spaces_or_new_lines_and_indent(CompleteStr(" -- Comment\n  "), 0, IR::GTE),
            Ok((CompleteStr(""), 2))
        );
    }

    #[test]
    fn just_spaces() {
        assert_eq!(
            spaces_or_new_lines_and_indent(CompleteStr("   "), 1, IR::GTE),
            Ok((CompleteStr(""), 4))
        );
    }

    #[test]
    fn simple_name() {
        assert_eq!(
            lo_name(CompleteStr("goToUserPage")),
            Ok((CompleteStr(""), "goToUserPage".to_string()))
        );
    }
}

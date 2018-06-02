use nom::alphanumeric0;
use nom::types::CompleteStr;

pub type Name = String;

pub type QualifiedType = Vec<Name>;

pub type ModuleName = Vec<String>;

pub type Alias = String;

/// Tests if byte is ASCII: a-z
#[inline]
pub fn is_lowercase(chr: char) -> bool {
    (chr >= 'a' && chr <= 'z')
}

named!(pub lo_name<CompleteStr, String>,
  alt!(
      map!(tag!("_"), |v| v.to_string())
    | do_parse!(
        start: take_while_m_n!(1, 1, is_lowercase) >>
        rest: map!(alphanumeric0, |v| v.to_string()) >>
        (start.to_string() + &rest)
      )
  ) 
);

named!(pub operator<CompleteStr, String>,
  map!(re_match!(r"^[+\\-\\/*=.$<>:&|^?%#@~!]+"), |s| s.to_string())
);

fn at_least(spaces: CompleteStr, indentation: u32) -> Result<u32, String> {
    let length = spaces.0.len() as u32;
    // Expect to have more than the indentation
    if length > indentation {
        Ok(length)
    } else {
        Err(spaces.to_string())
    }
}

named_args!(pub at_least_indent(indentation: u32) <CompleteStr, u32>,
  map_res!(is_a!(" "), |s| at_least(s, indentation))
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

named_args!(pub exactly_indent(indentation: u32) <CompleteStr, u32>,
  map_res!(is_a!(" "), |s| exactly(s, indentation))
);

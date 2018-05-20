use nom::alphanumeric;
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
        rest: map!(alphanumeric, |v| v.to_string()) >>
        (start.to_string() + &rest)
      )
  ) 
);

named!(pub operator<CompleteStr, String>,
  map!(re_match!("[+\\-\\/*=.$<>:&|^?%#@~!]+"), |s| s.to_string())
);

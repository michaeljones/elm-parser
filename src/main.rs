#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(pub elm); // synthesized by LALRPOP

#[test]
fn calculator1() {
    assert!(elm::TermParser::new().parse("22").is_ok());
    assert!(elm::TermParser::new().parse("(22)").is_ok());
    assert!(elm::TermParser::new().parse("((((22))))").is_ok());
    assert!(elm::TermParser::new().parse("((22)").is_err());
}

fn main() {}

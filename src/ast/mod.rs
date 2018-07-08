use nom::types::CompleteStr;

pub mod binop;
pub mod expression;
pub mod helpers;
pub mod statement;
pub mod type_;

use ast::statement::statements;
pub use ast::statement::{ExportSet, Statement};

named!(pub file<CompleteStr, Vec<Statement>>,
   terminated!(statements, eof!())
);

#[cfg(test)]
mod tests {

    use ast::file;
    use nom::types::CompleteStr;

    #[test]
    fn simple_file() {
        assert!(
            file(CompleteStr(
                "module Test exposing (..)

main : Program Flags Model Msg
main =
    myProgram
"
            )).is_ok()
        );
    }
}

use nom::multispace0;
use nom::types::CompleteStr;

mod comment;
mod core;
mod export;
mod function;
mod import;
mod infix;
mod module;
mod port;
mod type_declaration;

use ast::statement::comment::comment;
pub use ast::statement::core::{ExportSet, Statement};
use ast::statement::function::{function_declaration, function_type_declaration};
use ast::statement::import::import_statement;
use ast::statement::infix::infix_declaration;
use ast::statement::module::{effect_module_declaration, module_declaration,
                             port_module_declaration};
use ast::statement::port::{port_declaration, port_type_declaration};
use ast::statement::type_declaration::{type_alias_declaration, type_declaration};

named!(pub statement<CompleteStr, Statement>,
  alt!(
      port_module_declaration
    | effect_module_declaration
    | module_declaration
    | import_statement
    | type_alias_declaration
    | call!(type_declaration, 0)
    | port_type_declaration
    | port_declaration
    | call!(function_type_declaration, 0)
    | function_declaration
    | infix_declaration
    | comment
  )
);

named!(pub statements<CompleteStr, Vec<Statement>>,
  delimited!(
     multispace0,
     separated_nonempty_list!(
       multispace0,
       statement
     ),
     multispace0
  )
);

#[cfg(test)]
mod tests {

    use ast::statement::statements;
    use nom::types::CompleteStr;

    #[test]
    fn simple_statements() {
        assert!(
            statements(CompleteStr(
                "module Test exposing (..)

main : Program Flags Model Msg
main =
    myProgram
"
            )).is_ok()
        );
    }
}

use ast::helpers::{lo_name, module_name, spaces, spaces_or_new_lines_and_indent, up_name, IR};
use ast::statement::core::Statement;
use ast::statement::export::exports;

use nom::types::CompleteStr;

named!(pub port_module_declaration<CompleteStr, Statement>,
  do_parse!(
    tag!("port") >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    tag!("module") >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    name: module_name >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    tag!("exposing") >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    exports: exports >>
    (Statement::PortModuleDeclaration(name, exports))
  )
);

named!(pub effect_module_declaration<CompleteStr, Statement>,
  do_parse!(
    tag!("effect") >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    tag!("module") >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    name: module_name >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    tag!("where") >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    where_: delimited!(
      char!('{'),
      separated_list!(
        tag!(", "),
        do_parse!(
          lname: lo_name >>
          opt!(spaces) >>
          char!('=') >>
          opt!(spaces) >>
          uname: up_name >>
          ((lname, uname))
        )
      ),
      char!('}')
    ) >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    tag!("exposing") >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    exports: exports >>
    (Statement::EffectModuleDeclaration(name, where_, exports))
  )
);

named!(pub module_declaration<CompleteStr, Statement>,
  do_parse!(
    tag!("module") >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    name: module_name >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    tag!("exposing") >>
    call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    exports: exports >>
    (Statement::ModuleDeclaration(name, exports))
  )
);

#[cfg(test)]
mod tests {

    use ast::statement::core::*;
    use ast::statement::module::*;
    use nom::types::CompleteStr;

    fn fexp(name: &str) -> ExportSet {
        ExportSet::FunctionExport(name.to_string())
    }

    #[test]
    fn simple_declaration_exposing_all() {
        assert_eq!(
            module_declaration(CompleteStr("module A exposing (..)")),
            Ok((
                CompleteStr(""),
                Statement::ModuleDeclaration(vec!["A".to_string()], ExportSet::AllExport)
            ))
        );
    }

    #[test]
    fn declaration_exposing_particular_things() {
        assert_eq!(
            module_declaration(CompleteStr("module A exposing (A, b)")),
            Ok((
                CompleteStr(""),
                Statement::ModuleDeclaration(
                    vec!["A".to_string()],
                    ExportSet::SubsetExport(vec![
                        ExportSet::TypeExport("A".to_string(), None),
                        fexp("b"),
                    ])
                )
            ))
        );
    }

    #[test]
    fn declaration_exposing_an_infix_operator() {
        assert_eq!(
            module_declaration(CompleteStr("module A exposing ((?))")),
            Ok((
                CompleteStr(""),
                Statement::ModuleDeclaration(
                    vec!["A".to_string()],
                    ExportSet::SubsetExport(vec![fexp("?")])
                )
            ))
        );
    }

    #[test]
    fn declaration_exposing_union() {
        assert_eq!(
            module_declaration(CompleteStr("module A exposing (A(..))")),
            Ok((
                CompleteStr(""),
                Statement::ModuleDeclaration(
                    vec!["A".to_string()],
                    ExportSet::SubsetExport(vec![ExportSet::TypeExport(
                        "A".to_string(),
                        Some(Box::new(ExportSet::AllExport)),
                    )])
                )
            ))
        );
    }

    #[test]
    fn declaration_exposing_constructor_subset() {
        assert_eq!(
            module_declaration(CompleteStr("module A exposing (A(A))")),
            Ok((
                CompleteStr(""),
                Statement::ModuleDeclaration(
                    vec!["A".to_string()],
                    ExportSet::SubsetExport(vec![ExportSet::TypeExport(
                        "A".to_string(),
                        Some(Box::new(ExportSet::SubsetExport(vec![fexp("A")]))),
                    )])
                )
            ))
        );
    }

    #[test]
    fn multiline_declaration() {
        assert_eq!(
            module_declaration(CompleteStr("module\n A\n exposing\n ( A, B,\nc)")),
            Ok((
                CompleteStr(""),
                Statement::ModuleDeclaration(
                    vec!["A".to_string()],
                    ExportSet::SubsetExport(vec![
                        ExportSet::TypeExport("A".to_string(), None),
                        ExportSet::TypeExport("B".to_string(), None),
                        fexp("c"),
                    ])
                )
            ))
        );
    }

    #[test]
    fn declaration_using_a_port() {
        assert_eq!(
            port_module_declaration(CompleteStr("port module A exposing (A(..))")),
            Ok((
                CompleteStr(""),
                Statement::PortModuleDeclaration(
                    vec!["A".to_string()],
                    ExportSet::SubsetExport(vec![ExportSet::TypeExport(
                        "A".to_string(),
                        Some(Box::new(ExportSet::AllExport)),
                    )])
                )
            ))
        );
    }

    #[test]
    fn simple_effects() {
        assert_eq!(
            effect_module_declaration(CompleteStr(
                "effect module A where {subscription = MySub, command = MyCmd} exposing (..)"
            )),
            Ok((
                CompleteStr(""),
                Statement::EffectModuleDeclaration(
                    vec!["A".to_string()],
                    vec![
                        ("subscription".to_string(), "MySub".to_string()),
                        ("command".to_string(), "MyCmd".to_string()),
                    ],
                    ExportSet::AllExport
                )
            ))
        );
    }
}

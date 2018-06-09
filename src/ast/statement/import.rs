use ast::helpers::{module_name, spaces, up_name, Alias};
use ast::statement::core::Statement;
use ast::statement::export::exports;

use nom::types::CompleteStr;

named!(pub import_statement<CompleteStr, Statement>,
  do_parse!(
    tag!("import") >>
    spaces >>
    name: module_name >>
    alias: opt!(
      preceded!(
        delimited!(spaces, tag!("as"), spaces),
        up_name
      )
    ) >>
    exposing: opt!(
      preceded!(
        delimited!(spaces, tag!("exposing"), spaces),
        exports
      )
    ) >>
    (Statement::ImportStatement(name, alias, exposing))
  )
);

#[cfg(test)]
mod tests {

    use ast::statement::core::*;
    use ast::statement::import::*;
    use nom::types::CompleteStr;

    fn imp(name: &str, alias: Option<Alias>, exposing: Option<ExportSet>) -> Statement {
        Statement::ImportStatement(vec![name.to_string()], alias, exposing)
    }

    fn fexp(name: &str) -> ExportSet {
        ExportSet::FunctionExport(name.to_string())
    }

    fn texp(name: &str, exposing: Option<Box<ExportSet>>) -> ExportSet {
        ExportSet::TypeExport(name.to_string(), exposing)
    }

    #[test]
    fn simple_import() {
        assert_eq!(
            import_statement(CompleteStr("import A")),
            Ok((CompleteStr(""), imp("A", None, None)))
        );
    }

    #[test]
    fn import_as() {
        assert_eq!(
            import_statement(CompleteStr("import A as B")),
            Ok((CompleteStr(""), imp("A", Some("B".to_string()), None)))
        );
    }

    #[test]
    fn import_exposing_all() {
        assert_eq!(
            import_statement(CompleteStr("import A exposing (..)")),
            Ok((CompleteStr(""), imp("A", None, Some(ExportSet::AllExport))))
        );
    }

    #[test]
    fn import_exposing() {
        assert_eq!(
            import_statement(CompleteStr("import A exposing (A, b)")),
            Ok((
                CompleteStr(""),
                imp(
                    "A",
                    None,
                    Some(ExportSet::SubsetExport(vec![texp("A", None), fexp("b")]))
                )
            ))
        );
    }

    #[test]
    fn import_exposing_union() {
        assert_eq!(
            import_statement(CompleteStr("import A exposing (A(..))")),
            Ok((
                CompleteStr(""),
                imp(
                    "A",
                    None,
                    Some(ExportSet::SubsetExport(vec![texp(
                        "A",
                        Some(Box::new(ExportSet::AllExport)),
                    )]))
                )
            ))
        );
    }

    #[test]
    fn import_exposing_constructor_subset() {
        assert_eq!(
            import_statement(CompleteStr("import A exposing (A(A))")),
            Ok((
                CompleteStr(""),
                imp(
                    "A",
                    None,
                    Some(ExportSet::SubsetExport(vec![texp(
                        "A",
                        Some(Box::new(ExportSet::SubsetExport(vec![fexp("A")]))),
                    )]))
                )
            ))
        );
    }

    #[test]
    fn import_multiline() {
        assert_eq!(
            import_statement(CompleteStr("import A as B exposing (A, B,\nc)")),
            Ok((
                CompleteStr(""),
                imp(
                    "A",
                    Some("B".to_string()),
                    Some(ExportSet::SubsetExport(vec![
                        texp("A", None),
                        texp("B", None),
                        fexp("c"),
                    ]))
                )
            ))
        );
    }
}

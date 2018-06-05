use ast::statement::core::ExportSet;
use ast::helpers::{function_name, operator, up_name};

use nom::types::CompleteStr;

named!(all_export<CompleteStr, ExportSet>,
  map!(tag!(".."), |_| ExportSet::AllExport)
);

named!(function_export<CompleteStr, ExportSet>,
  map!(
    alt!(
        function_name
      | delimited!(char!('('), operator, char!(')'))
    ),
    ExportSet::FunctionExport
  )
);

named!(constructor_subset_exports<CompleteStr, ExportSet>,
  map!(
    separated_nonempty_list!(
      tag!(", "),
      map!(up_name, ExportSet::FunctionExport)
    ),
    ExportSet::SubsetExport
  )
);

named!(constructor_exports<CompleteStr, Option<Box<ExportSet>>>,
  opt!(
    map!(
      delimited!(
        char!('('),
        alt!(
            all_export
          | constructor_subset_exports
        ),
        char!(')')
      ),
      |es| Box::new(es)
    )
  )
);

named!(type_export<CompleteStr, ExportSet>,
  do_parse!(
    name: up_name >>
    constructors: constructor_exports >>
    (ExportSet::TypeExport(name, constructors))
  )
);

named!(subset_export<CompleteStr, ExportSet>,
  map!(
    separated_nonempty_list!(
      tag!(", "),
      alt!(
          function_export
        | type_export
      )
    ),
    ExportSet::SubsetExport
  )
);

named!(pub exports<CompleteStr, ExportSet>,
  delimited!(
    char!('('),
    alt!(
        all_export
      | subset_export
    ),
    char!(')')
  )
);

#[cfg(test)]
mod tests {

    use ast::statement::*;
    use ast::statement::core::*;
    use nom::types::CompleteStr;

    #[test]
    fn simple_all_export() {
        assert_eq!(
            exports(CompleteStr("(..)")),
            Ok((CompleteStr(""), ExportSet::AllExport))
        );
    }

    #[test]
    fn simple_function_export() {
        assert_eq!(
            exports(CompleteStr("(a, b)")),
            Ok((
                CompleteStr(""),
                ExportSet::SubsetExport(vec![
                    ExportSet::FunctionExport("a".to_string()),
                    ExportSet::FunctionExport("b".to_string()),
                ])
            ))
        );
    }

    #[test]
    fn simple_operator_export() {
        assert_eq!(
            exports(CompleteStr("((=>), (++))")),
            Ok((
                CompleteStr(""),
                ExportSet::SubsetExport(vec![
                    ExportSet::FunctionExport("=>".to_string()),
                    ExportSet::FunctionExport("++".to_string()),
                ])
            ))
        );
    }

    #[test]
    fn simple_constructors_export() {
        assert_eq!(
            exports(CompleteStr("(ReadOnly, Write(..), Sync(First, Last))")),
            Ok((
                CompleteStr(""),
                ExportSet::SubsetExport(vec![
                    ExportSet::TypeExport("ReadOnly".to_string(), None),
                    ExportSet::TypeExport(
                        "Write".to_string(),
                        Some(Box::new(ExportSet::AllExport)),
                    ),
                    ExportSet::TypeExport(
                        "Sync".to_string(),
                        Some(Box::new(ExportSet::SubsetExport(vec![
                            ExportSet::FunctionExport("First".to_string()),
                            ExportSet::FunctionExport("Last".to_string()),
                        ]))),
                    ),
                ])
            ))
        );
    }
}

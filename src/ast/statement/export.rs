use ast::helpers::{
    function_name, operator, spaces_and_newlines, spaces_or_new_lines_and_indent, up_name, IR,
};
use ast::statement::core::ExportSet;

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
      Box::new
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
      delimited!(opt!(spaces_and_newlines), char!(','), opt!(spaces_and_newlines)),
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
    preceded!(char!('('), opt!(call!(spaces_or_new_lines_and_indent, 0, IR::GTE))),
    alt!(
        all_export
      | subset_export
    ),
    terminated!(opt!(call!(spaces_or_new_lines_and_indent, 0, IR::GTE)), char!(')'))
  )
);

#[cfg(test)]
mod tests {

    use ast::statement::core::ExportSet;
    use ast::statement::export::exports;
    use nom::types::CompleteStr;

    fn fexp(name: &str) -> ExportSet {
        ExportSet::FunctionExport(name.to_string())
    }

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
                ExportSet::SubsetExport(vec![fexp("a"), fexp("b")])
            ))
        );
    }

    #[test]
    fn simple_multiline_function_export() {
        assert_eq!(
            exports(CompleteStr("( a\n,\n b\n,\nc)")),
            Ok((
                CompleteStr(""),
                ExportSet::SubsetExport(vec![fexp("a"), fexp("b"), fexp("c")])
            ))
        );
    }

    #[test]
    fn simple_operator_export() {
        assert_eq!(
            exports(CompleteStr("((=>), (++))")),
            Ok((
                CompleteStr(""),
                ExportSet::SubsetExport(vec![fexp("=>"), fexp("++")])
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
                            fexp("First"),
                            fexp("Last"),
                        ]))),
                    ),
                ])
            ))
        );
    }
}

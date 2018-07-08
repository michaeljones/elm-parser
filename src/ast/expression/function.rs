use ast::expression::core::{Function, FunctionDefinition, FunctionSignature};
use ast::expression::{expression, term};
use ast::helpers::{
    lo_name, new_line_and_same_indent, operator, spaces, spaces_or_new_lines_and_indent, IR,
};
use ast::type_::type_annotation;

use nom::types::CompleteStr;

named_args!(function_signature(indentation: u32)<CompleteStr, FunctionSignature>,
  do_parse!(
    name: alt!(
        lo_name
      | delimited!(char!('('), operator, char!(')'))
    ) >>
    spaces >>
    char!(':') >>
    new_indent: call!(spaces_or_new_lines_and_indent, indentation, IR::GT) >>
    type_: call!(type_annotation, new_indent) >>
    (FunctionSignature { name, type_ })
  )
);

named_args!(function_definition(indentation: u32)<CompleteStr, FunctionDefinition>,
  do_parse!(
    name: alt!(
        lo_name
      | delimited!(char!('('), operator, char!(')'))
    ) >>
    args: many0!(preceded!(spaces, call!(term, indentation))) >>
    spaces >>
    char!('=') >>
    new_indent: call!(spaces_or_new_lines_and_indent, indentation, IR::GT) >>
    body: call!(expression, new_indent) >>
    (FunctionDefinition {name, args, body})
  )
);

named_args!(pub function(indentation: u32)<CompleteStr, Function>,
    do_parse!(
        signature: opt!(
            terminated!(
                call!(function_signature, indentation),
                call!(new_line_and_same_indent, indentation)
            )
        ) >>
        definition: call!(function_definition, indentation) >>
        (Function { signature, definition })
    )
);

#[cfg(test)]
mod tests {

    use ast::expression::function::*;
    use ast::expression::Expression;
    use ast::type_::core::Type;

    use nom::types::CompleteStr;

    fn application(a: Expression, b: Expression) -> Expression {
        Expression::Application(Box::new(a), Box::new(b))
    }

    fn var(name: &str) -> Expression {
        Expression::Variable(vec![name.to_string()])
    }

    fn int(text: &str) -> Expression {
        Expression::Integer(text.to_string())
    }

    fn tapp(a: Type, b: Type) -> Type {
        Type::TypeApplication(Box::new(a), Box::new(b))
    }

    fn tcon(name: &str, v: Vec<Type>) -> Type {
        Type::TypeConstructor(vec![name.to_string()], v)
    }

    #[test]
    fn simple_function_type() {
        assert_eq!(
            function_signature(CompleteStr("f : Int -> Int"), 0),
            Ok((
                CompleteStr(""),
                FunctionSignature {
                    name: "f".to_string(),
                    type_: tapp(tcon("Int", vec![]), tcon("Int", vec![])),
                }
            ))
        );
    }

    #[test]
    fn simple_function_type_with_new_line() {
        assert_eq!(
            function_signature(CompleteStr("f : Int ->\n  Int"), 0),
            Ok((
                CompleteStr(""),
                FunctionSignature {
                    name: "f".to_string(),
                    type_: tapp(tcon("Int", vec![]), tcon("Int", vec![])),
                }
            ))
        );
    }

    #[test]
    fn simple_function() {
        assert_eq!(
            function_definition(CompleteStr("f x = a { r | f = 1 }    c"), 0),
            Ok((
                CompleteStr(""),
                FunctionDefinition {
                    name: "f".to_string(),
                    args: vec![var("x")],
                    body: application(
                        application(
                            var("a"),
                            Expression::RecordUpdate(
                                "r".to_string(),
                                vec![("f".to_string(), Expression::Integer("1".to_string()))],
                            ),
                        ),
                        var("c"),
                    ),
                }
            ))
        );
    }

    #[test]
    fn simple_function_with_new_line() {
        assert_eq!(
            function_definition(CompleteStr("f x = a \n c"), 0),
            Ok((
                CompleteStr(""),
                FunctionDefinition {
                    name: "f".to_string(),
                    args: vec![var("x")],
                    body: application(var("a"), var("c")),
                }
            ))
        );
    }

    #[test]
    fn function_type_with_tuple() {
        assert_eq!(
            function_signature(CompleteStr("h : (Int, Int) -> Int"), 0),
            Ok((
                CompleteStr(""),
                FunctionSignature {
                    name: "h".to_string(),
                    type_: tapp(
                        Type::TypeTuple(vec![tcon("Int", vec![]), tcon("Int", vec![])]),
                        tcon("Int", vec![])
                    ),
                }
            ))
        );
    }

    #[test]
    fn function_type_with_operator() {
        assert_eq!(
            function_signature(CompleteStr("(+) : Int -> Int"), 0),
            Ok((
                CompleteStr(""),
                FunctionSignature {
                    name: "+".to_string(),
                    type_: tapp(tcon("Int", vec![]), tcon("Int", vec![])),
                }
            ))
        );
    }

    #[test]
    fn function_with_operator() {
        assert_eq!(
            function_definition(CompleteStr("(+) a b = 1"), 0),
            Ok((
                CompleteStr(""),
                FunctionDefinition {
                    name: "+".to_string(),
                    args: vec![var("a"), var("b")],
                    body: int("1"),
                }
            ))
        );
    }

    #[test]
    fn multi_line_function_type() {
        assert_eq!(
            function_signature(
                CompleteStr(
                    "cssm :
    CssModules.Helpers
        { a : String
        , b : String
        }
        msg"
                ),
                0
            ),
            Ok((
                CompleteStr(""),
                FunctionSignature {
                    name: "cssm".to_string(),
                    type_: Type::TypeConstructor(
                        vec!["CssModules".to_string(), "Helpers".to_string()],
                        vec![
                            Type::TypeRecord(vec![
                                (
                                    "a".to_string(),
                                    Type::TypeConstructor(vec!["String".to_string()], vec![]),
                                ),
                                (
                                    "b".to_string(),
                                    Type::TypeConstructor(vec!["String".to_string()], vec![]),
                                ),
                            ]),
                            Type::TypeVariable("msg".to_string()),
                        ]
                    ),
                }
            ))
        );
    }

    #[test]
    fn function_with_signature() {
        assert_eq!(
            function(CompleteStr("f : Int -> Int\nf x = a \n c"), 0),
            Ok((
                CompleteStr(""),
                Function {
                    signature: Some(FunctionSignature {
                        name: "f".to_string(),
                        type_: tapp(tcon("Int", vec![]), tcon("Int", vec![])),
                    }),
                    definition: FunctionDefinition {
                        name: "f".to_string(),
                        args: vec![var("x")],
                        body: application(var("a"), var("c")),
                    },
                }
            ))
        );
    }

    #[test]
    fn function_without_signature() {
        assert_eq!(
            function(CompleteStr("f x = a \n c"), 0),
            Ok((
                CompleteStr(""),
                Function {
                    signature: None,
                    definition: FunctionDefinition {
                        name: "f".to_string(),
                        args: vec![var("x")],
                        body: application(var("a"), var("c")),
                    },
                }
            ))
        );
    }
}

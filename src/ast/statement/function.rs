use ast::expression::{expression, term};
use ast::helpers::{lo_name, operator, spaces, spaces_and_newlines};
use ast::statement::core::Statement;
use ast::statement::type_::{type_, type_annotation, type_constructor};

use nom::types::CompleteStr;

named!(pub function_type_declaration<CompleteStr, Statement>,
  do_parse!(
    name: alt!(
        lo_name
      | delimited!(char!('('), operator, char!(')'))
    ) >>
    spaces >>
    char!(':') >>
    spaces >>
    type_: type_annotation >>
    (Statement::FunctionTypeDeclaration(name, type_))
  )
);

named!(pub function_declaration<CompleteStr, Statement>,
  do_parse!(
    name: alt!(
        lo_name
      | delimited!(char!('('), operator, char!(')'))
    ) >>
    args: many0!(preceded!(spaces, call!(term, 0))) >>
    spaces >>
    char!('=') >>
    spaces_and_newlines >>
    exp: call!(expression, 0) >>
    (Statement::FunctionDeclaration(name, args, exp))
  )
);

#[cfg(test)]
mod tests {

    use ast::expression::Expression;
    use ast::statement::core::*;
    use ast::statement::function::*;
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

    fn bin_op(op: Expression, left: Expression, right: Expression) -> Expression {
        Expression::BinOp(Box::new(op), Box::new(left), Box::new(right))
    }

    fn tvar(name: &str) -> Type {
        Type::TypeVariable(name.to_string())
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
            function_type_declaration(CompleteStr("f : Int -> Int")),
            Ok((
                CompleteStr(""),
                Statement::FunctionTypeDeclaration(
                    "f".to_string(),
                    tapp(tcon("Int", vec![]), tcon("Int", vec![]))
                )
            ))
        );
    }

    #[test]
    fn simple_function_type_with_new_line() {
        assert_eq!(
            function_type_declaration(CompleteStr("f : Int ->\n  Int")),
            Ok((
                CompleteStr(""),
                Statement::FunctionTypeDeclaration(
                    "f".to_string(),
                    tapp(tcon("Int", vec![]), tcon("Int", vec![]))
                )
            ))
        );
    }

    #[test]
    fn simple_function() {
        assert_eq!(
            function_declaration(CompleteStr("f x = a { r | f = 1 }    c")),
            Ok((
                CompleteStr(""),
                Statement::FunctionDeclaration(
                    "f".to_string(),
                    vec![var("x")],
                    application(
                        application(
                            var("a"),
                            Expression::RecordUpdate(
                                "r".to_string(),
                                vec![("f".to_string(), Expression::Integer("1".to_string()))],
                            ),
                        ),
                        var("c"),
                    )
                )
            ))
        );
    }

    #[test]
    fn simple_function_with_new_line() {
        assert_eq!(
            function_declaration(CompleteStr("f x = a \n c")),
            Ok((
                CompleteStr(""),
                Statement::FunctionDeclaration(
                    "f".to_string(),
                    vec![var("x")],
                    application(var("a"), var("c"))
                )
            ))
        );
    }

    #[test]
    fn function_type_with_tuple() {
        assert_eq!(
            function_type_declaration(CompleteStr("h : (Int, Int) -> Int")),
            Ok((
                CompleteStr(""),
                Statement::FunctionTypeDeclaration(
                    "h".to_string(),
                    tapp(
                        Type::TypeTuple(vec![tcon("Int", vec![]), tcon("Int", vec![])]),
                        tcon("Int", vec![])
                    )
                )
            ))
        );
    }

    #[test]
    fn function_type_with_operator() {
        assert_eq!(
            function_type_declaration(CompleteStr("(+) : Int -> Int")),
            Ok((
                CompleteStr(""),
                Statement::FunctionTypeDeclaration(
                    "+".to_string(),
                    tapp(tcon("Int", vec![]), tcon("Int", vec![]))
                )
            ))
        );
    }

    #[test]
    fn function_with_operator() {
        assert_eq!(
            function_declaration(CompleteStr("(+) a b = 1")),
            Ok((
                CompleteStr(""),
                Statement::FunctionDeclaration("+".to_string(), vec![var("a"), var("b")], int("1"))
            ))
        );
    }
}

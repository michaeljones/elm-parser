use ast::expression::expression;
use ast::helpers::{lo_name, spaces, spaces_or_new_lines_and_indent, IR};
use ast::statement::core::Statement;
use ast::statement::type_::type_annotation;

use nom::types::CompleteStr;

named_args!(pub port_type_declaration(indentation: u32)<CompleteStr, Statement>,
  do_parse!(
    tag!("port") >>
    spaces >>
    name: lo_name >>
    spaces >>
    char!(':') >>
    new_indent: call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    type_: call!(type_annotation, new_indent) >>
    (Statement::PortTypeDeclaration(name, type_))
  )
);

named!(pub port_declaration<CompleteStr, Statement>,
  do_parse!(
    tag!("port") >>
    spaces >>
    name: lo_name >>
    args: many0!(preceded!(spaces, lo_name)) >>
    spaces >>
    char!('=') >>
    new_indent: call!(spaces_or_new_lines_and_indent, 0, IR::GT) >>
    exp: call!(expression, new_indent) >>
    (Statement::PortDeclaration(name, args, exp))
  )
);

#[cfg(test)]
mod tests {

    use ast::expression::Expression;
    use ast::statement::core::*;
    use ast::statement::port::*;
    use nom::types::CompleteStr;

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
    fn constant() {
        assert_eq!(
            port_type_declaration(CompleteStr("port focus : String -> Cmd msg"), 0),
            Ok((
                CompleteStr(""),
                Statement::PortTypeDeclaration(
                    "focus".to_string(),
                    tapp(tcon("String", vec![]), tcon("Cmd", vec![tvar("msg")]))
                )
            ))
        );
    }

    #[test]
    fn another_port_type_declaration() {
        assert_eq!(
            port_type_declaration(CompleteStr("port users : (User -> msg) -> Sub msg"), 0),
            Ok((
                CompleteStr(""),
                Statement::PortTypeDeclaration(
                    "users".to_string(),
                    tapp(
                        tapp(tcon("User", vec![]), tvar("msg")),
                        tcon("Sub", vec![tvar("msg")])
                    )
                )
            ))
        );
    }

    #[test]
    fn port_definition() {
        assert_eq!(
            port_declaration(CompleteStr("port focus = Cmd.none")),
            Ok((
                CompleteStr(""),
                Statement::PortDeclaration(
                    "focus".to_string(),
                    vec![],
                    Expression::Access(
                        Box::new(Expression::Variable(vec!["Cmd".to_string()])),
                        vec!["none".to_string()],
                    )
                )
            ))
        );
    }
}
